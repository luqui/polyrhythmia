{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
{-# LANGUAGE BangPatterns, TupleSections, LambdaCase, MultiWayIf #-}

import qualified System.MIDI as MIDI
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_, when)
import Control.Concurrent.STM
import Data.Foldable (toList)
import Data.Maybe (maybeToList)
import Data.List (foldl', foldl1')
import Control.Monad.Random hiding (next)
import Data.Ratio
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import qualified Control.Monad.Trans.State as State
import Control.Monad.IO.Class (liftIO)

import qualified APC40
import qualified Scale

-- TWEAKS --
minimumGrid, maximumPeriod, minimumNote, maximumNote, minimumChord, maximumChord :: Rational
minimumGrid = 1000/10  -- 10th of a second
maximumPeriod = 1000 * 10
minimumNote = 1000/5
maximumNote = 1000/2
minimumChord = 2000
maximumChord = maximumPeriod

deathFadePerCycle :: Double
deathFadePerCycle = 1/50

modTime :: Int
modTime = 20000

inputAccuracy :: Int
inputAccuracy = 50

happyLightsTiming :: Int
happyLightsTiming = 100

-- END TWEAKS --

data Conns = Conns {
    cMainConn :: MIDI.Connection,
    cAPC :: Maybe APC40.Devs
  }

openConn :: IO MIDI.Connection
openConn = do
    dest <- MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
    MIDI.start dest
    return dest

openConns :: IO Conns
openConns = Conns <$> openConn <*> APC40.openDevs

type Time = Rational  -- millisecs

data GhostCount = GhostCount {
    gcTries :: Int,
    gcHits :: Int,
    gcTotal :: Int }

data ActiveRecord = ActiveRecord {
    arMessageChan :: TChan Message,
    arGhostCount :: TVar GhostCount
    } 
    deriving (Eq)

data InactiveRecord = InactiveRecord { irLastKillTime :: Time }
    deriving (Eq)

data State = State {
    sActive :: Map.Map Rhythm ActiveRecord,
    sInactive :: Map.Map Rhythm InactiveRecord, 
    -- The key is modeled by a "map" from velocity to a particular scale
    -- The highest velocity is the actual key, and the others act as a sort of stack to use
    --   when the highest velocity is released.
    sKey :: Map.Map Int Scale.Scale,  -- vel -> scale
    sScore :: Int
   }
   deriving (Eq)

data Pitch = Percussion Scale.MIDINote
           | RootTonal  Scale.Range Int
           | ShiftTonal Scale.Range Int
           | ControlChange Int
           | GlobalScaleChange Scale.Scale
    deriving (Eq, Ord, Show)

data Instrument = Instrument 
    { iMinLength :: Rational
    , iMaxLength :: Rational
    , iMinNotes :: Int
    , iMaxNotes :: Int
    , iPeriodExempt :: Bool -- disable period check for this instrument
                            -- (so rhythms can be longer)
    , iChannel :: Int
    , iAPCCoord :: APCCoord
    , iPitches :: [Pitch]
    , iModulate :: Bool
    } 

data APCCoord = APCCoord (Int,Int) APC40.RGB | NoCoord
    deriving (Eq, Ord, Show)

rgbMagToVel :: APC40.RGB -> Double -> Int
rgbMagToVel (r,g,b) mag = APC40.rgbToVel (r*mag,g*mag,b*mag)

data Note = Note {
    nChannel :: Int,
    nPitch ::  Pitch,
    nVel ::  Int,
    nDuration ::  Rational,  -- in fraction of voice timing
    nCoord :: APCCoord
    }
    deriving (Eq, Ord, Show)

data Rhythm = Rhythm {
    rPeriodExempt :: Bool,
    rRole :: String,
    rTiming :: Rational,
    rNotes :: [Note],
    rAltNotes :: [Note]  -- played as a pickup to phrase alignment
    }
    deriving (Eq, Ord, Show)

timeLength :: Rhythm -> Rational
timeLength r = fromIntegral (length (rNotes r)) * rTiming r

findPeriod :: (Foldable f) => f Rhythm -> Rational
findPeriod f | null f = 1
             | otherwise = lcmsRat . map timeLength . filter (not . rPeriodExempt) . toList $ f

findGrid :: (Foldable f) => f Rhythm -> Rational
findGrid f | null f = 0
           | otherwise = gcdsRat . map rTiming . toList $ f

type Kit = Map.Map String Instrument

data Message 
    = MsgTerm
    deriving Show

whenMay :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenMay m f = maybe (return ()) f m

-- A GhostEntry records whether an apc key was pressed recently enough to
-- warrant playing the note, and also whether a note was triggered recently
-- enough to play when the apc key is pressed.
data GhostEntry = GhostNone | GhostNote ActiveRecord Note | GhostKey

type GhostMap = Map.Map (Int,Int) (TVar GhostEntry)

playNoteNow :: Conns -> TVar State -> Rational -> Note -> IO ()
playNoteNow conns stateVar timing (Note ch pitch 0 dur coord) = return ()
playNoteNow conns stateVar timing (Note ch pitch vel dur coord) = do
    (midi0, midi1) <- pitchToMIDI pitch 
    (apc0, apc1) <- apcCommands coord
    midi0
    apc0

    let lightOffTime = 0.9 * minimumGrid
    let noteOffTime = dur * timing

    if lightOffTime < noteOffTime then do
        threadDelay (floor (1000 * lightOffTime))
        apc1
        threadDelay (floor (1000 * (noteOffTime - lightOffTime)))
        midi1
    else do
        threadDelay (floor (1000 * noteOffTime))
        midi1
        threadDelay (floor (1000 * (lightOffTime - noteOffTime)))
        apc1

    where
    infix 0 //
    a // b = return (a, b)

    conn = cMainConn conns

    apcCommands :: APCCoord -> IO (IO (), IO ())
    apcCommands (APCCoord coord color) = do
        case cAPC conns of
            Nothing -> return () // return ()
            Just apc -> APC40.lightOn coord (rgbMagToVel color (fromIntegral vel / 127)) apc
                        // APC40.lightOn coord 0 apc
    apcCommands NoCoord = return () // return ()

    pitchToMIDI :: Pitch -> IO (IO (), IO ())
    pitchToMIDI (Percussion n) = 
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn n vel))
            // MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn n 0))
    pitchToMIDI (RootTonal range deg)  = do
        key <- atomically $ snd . Map.findMax . sKey <$> readTVar stateVar
        let note = Scale.apply key range deg
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
            // MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
    pitchToMIDI (ShiftTonal range deg)  = do
        key <- atomically $ snd . Map.findMax . sKey <$> readTVar stateVar
        let note = Scale.apply key range deg
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
            // MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
    pitchToMIDI (ControlChange ctrl) = 
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.CC ctrl vel))
            // return ()
    pitchToMIDI (GlobalScaleChange scale) = do
        atomically (modifyTVar stateVar 
            (\s -> s { sKey = Map.insert vel scale (sKey s) }))
        // atomically (modifyTVar stateVar 
            (\s -> s { sKey = Map.delete vel (sKey s) }))
    
playGhostNote :: GhostMap -> Conns -> TVar State -> ActiveRecord -> Rational -> Note -> IO ()
playGhostNote ghostMap conns stateVar activerecord timing note@(Note _ch _pitch _vel _dur (APCCoord coord _color)) = do
    whenMay (Map.lookup coord ghostMap) $ \tvar -> do
        join . atomically $ readTVar tvar >>= \case
            GhostNone -> do
                writeTVar tvar (GhostNote activerecord note)
                return . void . forkIO $ do
                    threadDelay (1000*inputAccuracy)
                    atomically $ writeTVar tvar GhostNone
            GhostKey -> do
                modifyTVar (arGhostCount activerecord) $ \gc -> gc { gcHits = gcHits gc + 1 }
                return $ playNoteNow conns stateVar timing note
            GhostNote _ _ -> return $ do
                -- Theoretically shouldn't happen
                putStrLn "Double ghost! Boo!"
playGhostNote _ _ _ _ _ _ = return ()

deathModulateNote :: Double -> Note -> Note
deathModulateNote mod n = n { nCoord = modCoord (nCoord n) }
    where
    modCoord NoCoord = NoCoord
    modCoord (APCCoord c (r,g,b))
        | mod > 1    = APCCoord c (1,1,1)
        | mod < 0.25 = APCCoord c (mod/0.25,0,0)
        | otherwise  = APCCoord c (param*r + (1-param)*1, param*g, param*b)
        where
        param = (mod-0.25)/0.75


rhythmThread :: GhostMap -> TVar State -> Conns -> ActiveRecord -> Rhythm -> IO ()
rhythmThread ghostmap stateVar conns activerecord rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = quantize (timeLength rhythm) now
    -- volume modulation
    State.evalStateT (go starttime) 1

    where
    timing = rTiming rhythm
    conn = cMainConn conns

    playNote :: Time -> Note -> State.StateT Double IO ()
    playNote _ (Note _ _ 0 _ _) = return () 
    playNote t inNote = do
        deathmod <- State.get
        let note = deathModulateNote deathmod inNote
        liftIO $ waitTill conn t
        join . liftIO . atomically $ do
            count <- readTVar (arGhostCount activerecord)
            if | gcTotal count == 0 || gcTries count > gcTotal count -> do
                    return . liftIO $ playNoteNow conns stateVar timing note
               | gcTries count == gcTotal count -> do
                    writeTVar (arGhostCount activerecord) (count { gcTries = gcTries count + 1 })
                    return $ do
                        let win = fromIntegral (gcHits count) >= 0.75 * fromIntegral (gcTotal count)
                        when win $ State.modify (+1)
                        liftIO . atomically . modifyTVar stateVar $ \s -> s { sScore = sScore s + 1 }
                        liftIO $ playGhostNote ghostmap conns stateVar activerecord timing note
                        liftIO . void . forkIO $ do
                            let color | win = 122
                                      | otherwise = 120
                            makeHappyLights color note
                            atomically $ writeTVar (arGhostCount activerecord) (GhostCount 0 0 0)
                | otherwise -> do
                    writeTVar (arGhostCount activerecord) (count { gcTries = gcTries count + 1 })
                    return . liftIO $ playGhostNote ghostmap conns stateVar activerecord timing note

    makeHappyLights color (Note _ _ _ _ (APCCoord coord _)) = do
        whenMay (cAPC conns) $ \apc -> replicateM_ 8 $ do
            APC40.lightOn coord color apc
            threadDelay (1000 * happyLightsTiming)
            APC40.lightOn coord 0 apc
            threadDelay (1000 * happyLightsTiming)
    makeHappyLights _ _ = return ()

    playPhrase t0 notes = do
        let times = [t0, t0 + timing ..]
        let !ret = last (zipWith const times (notes ++ [error "too clever"]))
        forM_ (zip times notes) $ \(t, note) -> playNote t note
        return ret

    chooseAndPlayPhrase t0 = do
        state <- liftIO . atomically $ readTVar stateVar
        let end = t0 + timeLength rhythm
        let period = findPeriod (Map.keys (sActive state))
        if end == quantize period end && period /= timeLength rhythm
            then
                --playPhrase t0 (rAltNotes rhythm)
                playPhrase t0 (rNotes rhythm)
            else
                playPhrase t0 (rNotes rhythm)

    go :: Rational -> State.StateT Double IO ()
    go t0 = do
        sig <- liftIO . atomically . tryReadTChan $ arMessageChan activerecord
        deathmod <- subtract deathFadePerCycle <$> State.get
        trying <- fmap ((/= 0) . gcTotal) . liftIO . atomically . readTVar $ arGhostCount activerecord  
        State.put deathmod
        case sig of
            Nothing | deathmod > 0 || trying -> chooseAndPlayPhrase t0 >>= go
                    | otherwise              -> do
                        liftIO . atomically $ modifyTVar stateVar $ \s -> s { sScore = sScore s - 1 }
            Just MsgTerm -> return ()
             
quantize :: Rational -> Rational -> Rational
quantize grid x = fromIntegral (ceiling (x / grid)) * grid

waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- fromIntegral <$> MIDI.currentTime conn
    threadDelay (floor (1000 * (target - now)))

rhythmMain :: GhostMap -> Conns -> TVar State -> Rhythm -> IO ()
rhythmMain ghostmap conns stateVar rhythm = do
    join . atomically $ do
        state <- readTVar stateVar
        if rhythm `Map.member` sActive state then
            return $ return ()
        else do
            activerecord <- ActiveRecord <$> newTChan <*> newTVar (GhostCount 0 0 0)
            writeTVar stateVar $ state { sActive = Map.insert rhythm activerecord (sActive state)
                                       , sInactive = Map.delete rhythm (sInactive state) }
            return $ do
                rhythmThread ghostmap stateVar conns activerecord rhythm
                now <- fromIntegral <$> MIDI.currentTime (cMainConn conns)
                atomically . modifyTVar stateVar $ \s -> s { sActive = Map.delete rhythm (sActive s)
                                                           , sInactive = Map.insert rhythm (InactiveRecord now) (sInactive s) }

renderState :: State -> IO ()
renderState state = do
    clearScreen
    setCursorPosition 0 0
    let timebase = findGrid (Map.keys (sActive state))
    let period = findPeriod (Map.keys (sActive state))
    putStrLn $ "score:  " ++ show (sScore state)
    putStrLn $ "grid:   " ++ show (round timebase) ++ "ms"
    putStrLn $ "period: " ++ show (round period) ++ "ms"
    putStrLn $ "key:    " ++ show (snd . Map.findMax $ sKey state)

    let padding = maximum [ length (rRole r) | r <- Map.keys (sActive state) ]
    mapM_ (putStrLn . renderRhythm timebase padding) (Map.keys (sActive state))


    hFlush stdout


renderRhythm :: Rational -> Int -> Rhythm -> String
renderRhythm timebase padding rhythm = 
    padString padding (rRole rhythm) 
      ++ " |" ++ concat [ renderNote n ++ replicate (spacing - 1) ' ' | n <- rNotes rhythm ] ++ "|"
    where
    spacing = floor (rTiming rhythm / timebase)
    renderNote (Note _ _ v _ _) | v == 0    = "."
                                | v < 75    = "x"
                                | otherwise = "X"
    padString p s = take p (s ++ repeat ' ')

mainThread :: Kit -> Conns -> IO ()
mainThread chkit conns = do
    stateVar <- newTVarIO $ State { sActive = Map.empty, sInactive = Map.empty, sKey = Map.singleton 0 Scale.cMinorPentatonic, sScore = 0 }
    -- display thread
    void . forkIO . forever $ do
        state <- atomically (readTVar stateVar)
        renderState state
        atomically $ do
            state' <- readTVar stateVar
            when (state == state') retry

    -- APC thread
    ghostMap <- traverse (\_ -> newTVarIO GhostNone) roleMap
    whenMay (cAPC conns) $ \apcdevs -> do
        void . forkIO . forever $ do
            state <- atomically (readTVar stateVar)
            events <- APC40.pollNotes apcdevs
            forM_ events $ \coord -> do
                whenMay (Map.lookup coord roleMap) $ \role -> do
                    sequence_ $ do
                        (k,ar) <- Map.assocs (sActive state)
                        guard (rRole k == role)
                        return . atomically $ do
                            count <- readTVar (arGhostCount ar)
                            when (gcTotal count == 0) $ writeTVar (arGhostCount ar) 
                                                    (GhostCount { gcTotal = 10, gcTries = 0, gcHits = 0 })
                whenMay (Map.lookup coord ghostMap) $ \ghostVar -> do
                    join . atomically $ readTVar ghostVar >>= \case
                        GhostNone -> do
                            writeTVar ghostVar GhostKey
                            return . void . forkIO $ do
                                threadDelay (1000 * inputAccuracy)
                                atomically $ writeTVar ghostVar GhostNone
                        GhostKey -> return $ return ()
                        GhostNote activerecord note -> do
                            -- XXX minimumGrid is not correct
                            -- Bundle timing with GhostNote constructor
                            modifyTVar (arGhostCount activerecord) $ \gc -> gc { gcHits = gcHits gc + 1 }
                            return . void . forkIO $ playNoteNow conns stateVar minimumGrid note
            threadDelay 10000
                
    -- song evolution thread:
    forever $ do
        newRhythm ghostMap Nothing stateVar
        state <- atomically $ readTVar stateVar
        now <- fromIntegral <$> MIDI.currentTime (cMainConn conns)
        let period = findPeriod (Map.keys (sActive state))
        let next = quantize period (now + fromIntegral modTime) - 200
          -- make modification slightly before beginning of phrase
          -- so thread has time to start on time (& maybe even pickup)
        waitTill (cMainConn conns) next
    
  where
  newRhythm ghostmap mayrole stateVar = void . forkIO $ do
    state <- atomically (readTVar stateVar)  -- XXX another race condition, could make two incompatible rhythms
    now <- fromIntegral <$> MIDI.currentTime (cMainConn conns)
    ret <- evalRandIO $ do
        newr <- fmap (state,) <$> makeDerivedRhythmG mayrole chkit (Map.keys (sActive state))
        --pastr <- choosePastRhythm mayrole state now
        --uniform [pastr `mplus` newr, newr `mplus` pastr]
        return newr
    case ret of
        Nothing -> return()
        Just (state', r) -> do
            atomically (writeTVar stateVar state')
            rhythmMain ghostmap conns stateVar r


type Cloud = Rand StdGen

randomNote :: Kit -> String -> Cloud Note
randomNote chkit role = do
    let instr = chkit Map.! role
    pitch <- uniform (iPitches instr)
    vel <- id =<< uniform [return 0, getRandomR (64,127)]
    dur <- uniform [1/10, 1/2, 9/10, 1]
    return $ Note (iChannel instr) pitch vel dur (iAPCCoord instr)

makeRhythmRole :: Kit -> String -> Rational -> Int -> Cloud Rhythm
makeRhythmRole chkit role timing numNotes = do
    notes <- replicateM numNotes (randomNote chkit role)
    altNotes <- replicateM numNotes (randomNote chkit role)
    return $ Rhythm 
        { rTiming = timing
        , rRole = role
        , rNotes = notes
        , rPeriodExempt = iPeriodExempt (chkit Map.! role)
        , rAltNotes = altNotes
        }

makeRhythm :: Kit -> Rational -> Int -> Cloud Rhythm
makeRhythm chkit timing numNotes = do
    role <- uniform (Map.keys chkit)
    makeRhythmRole chkit role timing numNotes

admits :: (Foldable f) => f Rhythm -> Rhythm -> Bool
admits rs = \cand -> and [ minimumGrid <= gcdRat grid (rTiming cand)
                         , rPeriodExempt cand || lcmRat period (timeLength cand) <= maximumPeriod
                         , rRole cand `Set.notMember` roles
                         ]
    where
    grid = findGrid rs
    period = findPeriod rs
    roles = Set.fromList (map rRole (toList rs))

choosePastRhythm :: Maybe String -> State -> Time -> Cloud (Maybe (State, Rhythm))
choosePastRhythm mayrole state now = do
    choice <- weightedMay $ do
        (rhythm, irecord) <- Map.assocs (sInactive state)
        guard (Map.keys (sActive state) `admits` rhythm)
        guard (maybe True (rRole rhythm ==) mayrole)
        return (rhythm, now - irLastKillTime irecord)
    return $ fmap (state,) choice

ratToInt :: Rational -> Maybe Integer
ratToInt r | denominator r == 1 = Just (numerator r)
           | otherwise = Nothing

divisors :: (Integral a) => a -> [a]
divisors n = [ m | m <- [1..n], n `mod` m == 0 ]

makeDerivedRhythm :: Kit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythm chkit rs = do
    mayrole <- uniformMay (Map.keysSet chkit `Set.difference` Set.fromList (map rRole rs))
    case mayrole of
        Nothing -> return Nothing
        Just role -> makeDerivedRhythmRole role chkit rs

makeDerivedRhythmG :: Maybe String -> Kit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythmG = maybe makeDerivedRhythm makeDerivedRhythmRole

makeDerivedRhythmRole :: String -> Kit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythmRole role chkit [] = do
    timing <- (2000 %) <$> getRandomR (4,12)
    notes  <- uniform [3..8]
    Just <$> makeRhythmRole chkit role timing notes
makeDerivedRhythmRole role chkit rs = do
    let instr = chkit Map.! role
    let grid = findGrid rs
    let newGrids = map (grid/) [1..fromIntegral (floor (grid/minimumGrid))]

    let period = findPeriod rs
    let newPeriods
            | iPeriodExempt instr
                = map (period*) [1..fromIntegral (iMaxNotes instr)]
            | otherwise 
                = map (period*) [1..fromIntegral (floor (maximumPeriod/period))]

    selection <- uniformMay $ do
        g <- newGrids
        p <- newPeriods
        let (minT, maxT) = (iMinLength instr, iMaxLength instr)
        timing <- map (g*) [fromIntegral(ceiling (minT/g))..fromIntegral (floor (maxT/g))]
        guard (minT <= timing && timing <= maxT)
            

        maxnotes <- map fromIntegral (maybeToList (ratToInt (p/timing)))
        notes <- divisors maxnotes
        guard (iMinNotes instr <= notes && notes <= iMaxNotes instr)
        return (timing, fromIntegral notes)

    case selection of
      Just (timing, notes) -> Just <$> makeRhythmRole chkit role timing notes
      Nothing -> return Nothing

gcdRat :: Rational -> Rational -> Rational
gcdRat r r' = gcd (numerator r) (numerator r') % lcm (denominator r) (denominator r')

gcdsRat  :: [Rational] -> Rational
gcdsRat = foldl' gcdRat 0

lcmRat :: Rational -> Rational -> Rational
lcmRat r r' = lcm (numerator r) (numerator r') % gcd (denominator r) (denominator r')

lcmsRat :: [Rational] -> Rational
lcmsRat [] = 1          -- I think this should be "1/0".
lcmsRat xs = foldl1' lcmRat xs

(-->) :: a -> b -> (a,b)
(-->) = (,)

defaultInstrument :: Instrument
defaultInstrument = Instrument
    { iMinLength = minimumNote
    , iMaxLength = maximumNote
    , iMinNotes = 3
    , iMaxNotes = 8
    , iPeriodExempt = False
    , iChannel = 0
    , iPitches = []
    , iAPCCoord = NoCoord
    , iModulate = True
    }

perc :: APCCoord -> [Int] -> Instrument
perc coord notes = defaultInstrument { iPitches = map Percussion notes, iAPCCoord = coord }

rootTonal :: APCCoord -> Scale.Range -> Instrument
rootTonal coord range = defaultInstrument { iPitches = map (RootTonal range) [0..m], iAPCCoord = coord }
    where
    m = ceiling (fromIntegral (snd range - fst range) * 7/12)

shiftTonal :: APCCoord -> Scale.Range -> Instrument
shiftTonal coord range = defaultInstrument { iPitches = map (ShiftTonal range) [0..m], iAPCCoord = coord }
    where
    m = ceiling (fromIntegral (snd range - fst range) * 7/12)

chords :: [Scale.Scale] -> Instrument
chords scales = defaultInstrument
    { iPitches = map GlobalScaleChange scales
    , iMinLength = minimumChord
    , iMaxLength = maximumChord
    , iMinNotes = 1
    , iMaxNotes = 8
    , iPeriodExempt = False
    , iModulate = False
    }

pedal :: Instrument
pedal = defaultInstrument
    { iPitches = [ControlChange 64]
    , iMinLength = minimumNote
    , iMaxLength = maximumChord
    , iMinNotes = 1
    , iMaxNotes = 8
    , iPeriodExempt = False
    , iModulate = False
    }

cMinorKit :: Kit
cMinorKit = Map.fromList [
    "bass" --> rootTonal (APCCoord (3,3) (0,0,1)) (31,48)
  , "mid"  --> shiftTonal (APCCoord (3,2) (0,1,1)) (51,72)
  , "high" --> shiftTonal (APCCoord (3,1) (1,0,1)) (72,89)
  , "pedal" --> pedal
  ]

cMinorBassKit :: Kit
cMinorBassKit = Map.fromList [
  "bass" --> rootTonal (APCCoord (4,1) (0,1,1)) (31,48)
  ]

chordKit :: Kit
chordKit = Map.fromList [
    "chord" --> (chords $ 
        [ Scale.transposeChr d Scale.cMajor | d <- [0..11] ] ++
        [ Scale.transposeChr d Scale.cMinor | d <- [0..11] ])
    ]

studioDrummerKit :: Kit
studioDrummerKit = Map.fromList [
  "kick"  --> perc (APCCoord (1,1) (0,0,1)) [36],
  "snare" --> perc (APCCoord (1,2) (1,1,0)) [37, 38, 39, 40],
  "hat"   --> perc (APCCoord (1,3) (0,1,0)) [42, 44, 46],
  "tom"   --> perc (APCCoord (1,4) (0,1,1)) [41, 43, 45, 47],
  "ride"  --> perc (APCCoord (1,5) (1,0,1)) [50, 53]
  ]

glitchKit :: Kit
glitchKit = Map.fromList [
    "kick"  --> perc (APCCoord (2,1) (0,0,1)) [36,37,48,49,60,61,72,73],
    "snare" --> perc (APCCoord (2,2) (1,1,0)) [38,40,50,52,62,64,74,76],
    "hat"   --> perc (APCCoord (2,3) (0,1,0)) [44,46,58],
    "click" --> perc (APCCoord (2,4) (0,1,1)) [39,41,42,43,53,54,56,57,59
                                              ,69,77,78,79,81,83,84,86,89]
    ]

makeKit :: [(String, Int, Kit)] -> Kit
makeKit kits = Map.unions 
    [ Map.mapKeysMonotonic ((name ++ ".") ++) . fmap (\i -> i { iChannel = ch }) $ kit 
    | (name, ch, kit) <- kits ]

myKit :: Kit
myKit = makeKit [
      ("kit", 1, studioDrummerKit)
    , ("glitch", 2, glitchKit)
    --, ("bell", 2, gamillionKit)
    --, ("elec", 3, sAndBKit)
    , ("keys", 3, cMinorKit)
    , ("bass", 4, cMinorBassKit)
    , ("chord", 0, chordKit)
    ]

roleMap :: Map.Map (Int,Int) String
roleMap = Map.fromList
    [ (1,1) --> "kit.kick"
    , (1,2) --> "kit.snare"
    , (1,3) --> "kit.hat"
    , (1,4) --> "kit.tom"
    , (1,5) --> "kit.ride"
    , (2,1) --> "glitch.kick"
    , (2,2) --> "glitch.snare"
    , (2,3) --> "glitch.hat"
    , (2,4) --> "glitch.click"
    , (3,1) --> "keys.high"
    , (3,2) --> "keys.mid"
    , (3,3) --> "keys.bass"
    , (4,1) --> "bass.bass"
    ]

main :: IO ()
main = do
    conns <- openConns
    mainThread myKit conns
