{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
{-# LANGUAGE BangPatterns, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_, when)
import Control.Concurrent.STM
import Control.Applicative
import Data.Foldable (toList)
import Data.Maybe (maybeToList)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl', foldl1')
import Control.Monad.Random
import Data.Ratio
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM, raiseSignal)

import qualified APC40
import qualified Scale

-- TWEAKS --
minimumGrid, maximumPeriod, minimumNote, maximumNote, minimumChord, maximumChord :: Rational
minimumGrid = 1000/16  -- 16th of a second
maximumPeriod = 1000 * 10
minimumNote = 1000/8
maximumNote = 1000/2
minimumChord = 2000
maximumChord = maximumPeriod

averageVoices :: Int
averageVoices = 8

modTime :: Int
modTime = 4000

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

data ActiveRecord = ActiveRecord { arMessageChan :: TChan Message }
    deriving (Eq)

data InactiveRecord = InactiveRecord { irLastKillTime :: Time }
    deriving (Eq)

data State = State {
    sActive :: Map.Map Rhythm ActiveRecord,
    sInactive :: Map.Map Rhythm InactiveRecord, 
    -- The key is modeled by a "map" from velocity to a particular scale
    -- The highest velocity is the actual key, and the others act as a sort of stack to use
    --   when the highest velocity is released.
    sKey :: Map.Map Int Scale.Scale  -- vel -> scale
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

data APCCoord = APCCoord Int Int APC40.RGB | NoCoord
    deriving (Eq, Ord, Show)

rgbMagToVel :: APC40.RGB -> Double -> Int
rgbMagToVel (r,g,b) mag = APC40.rgbToVel (r*mag,g*mag,b*mag)

data Note = Note Int Pitch Int Rational APCCoord  -- ch note vel dur apccoord  (dur in fraction of voice timing)
    deriving (Eq, Ord, Show)

data Rhythm = Rhythm {
    rPeriodExempt :: Bool,
    rRole :: String,
    rTiming :: Rational,
    rNotes :: [Note],
    rAltNotes :: [Note],  -- played as a pickup to phrase alignment
    rModulate :: Bool
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

rhythmThread :: TVar State -> Conns -> TChan Message -> Rhythm -> IO ()
rhythmThread stateVar conns chan rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = quantize (timeLength rhythm) now
    -- volume modulation
    volperiod <- evalRandIO $ getRandomR (5000.0,20000.0)
    volamp <- evalRandIO $ getRandomR (0.01, 0.99)
    go volperiod volamp starttime

    where
    timing = rTiming rhythm
    conn = cMainConn conns

    playNote :: Double -> Time -> Note -> IO ()
    playNote vmod t (Note ch pitch vel dur coord) = do
        waitTill conn t
        let vel' | rModulate rhythm = round (fromIntegral vel * vmod)
                 | otherwise = vel
        when (vel' > 0) $ do
            whenMay (cAPC conns) $ \apc -> 
                case coord of
                    APCCoord x y color -> void . forkIO $ do
                        APC40.lightOn x y (rgbMagToVel color (fromIntegral vel' / 127)) apc
                        waitTill conn (t + 0.9 * minimumGrid)
                        APC40.lightOn x y 0 apc
                    NoCoord -> return ()
            pitchToMIDI pitch ch vel' (waitTill conn (t + dur * timing))

    pitchToMIDI :: Pitch -> Int -> Int -> IO () -> IO ()
    pitchToMIDI (Percussion n) ch vel wait = do
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn n vel))
        wait
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn n 0))
    pitchToMIDI (RootTonal range deg) ch vel wait = do
        key <- atomically $ snd . Map.findMax . sKey <$> readTVar stateVar
        let note = Scale.apply key range deg
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
        wait
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
    pitchToMIDI (ShiftTonal range deg) ch vel wait = do
        key <- atomically $ snd . Map.findMax . sKey <$> readTVar stateVar
        let note = Scale.apply key range deg
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
        wait
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
    pitchToMIDI (ControlChange ctrl) ch vel wait = do
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.CC ctrl vel))
        wait
    pitchToMIDI (GlobalScaleChange scale) _ch vel wait = do
        atomically . modifyTVar stateVar $ 
            \s -> s { sKey = Map.insert vel scale (sKey s) }
        wait
        atomically . modifyTVar stateVar $ 
            \s -> s { sKey = Map.delete vel (sKey s) }

    playPhrase volperiod volamp t0 notes = do
        let times = [t0, t0 + timing ..]
        let !ret = last (zipWith const times (notes ++ [error "too clever"]))
        forM_ (zip times notes) $ \(t, note) -> 
            let velmod = 0.5 * (1 + sin (realToFrac t / volperiod)) * volamp
            in playNote (1-velmod) t note
        return ret

    chooseAndPlayPhrase volperiod volamp t0 = do
        state <- atomically $ readTVar stateVar
        let end = t0 + timeLength rhythm
        if end == quantize (findPeriod (Map.keys (sActive state))) end
            then
                playPhrase volperiod volamp t0 (rAltNotes rhythm)
            else
                playPhrase volperiod volamp t0 (rNotes rhythm)

    fadePhrase t0 = do
        let times = [t0, t0 + timing ..]
        let velmod = [1,1-1/40..1/40]
        forM_ (zip3 velmod times (cycle (rNotes rhythm))) $ \(vmod, t, note) -> 
            playNote vmod t note
             
    go :: Double -> Double -> Rational -> IO ()
    go volperiod volamp t0 = do
        sig <- atomically (tryReadTChan chan)
        case sig of
            Nothing -> chooseAndPlayPhrase volperiod volamp t0 >>= go volperiod volamp
            Just MsgTerm -> fadePhrase t0
             
quantize :: Rational -> Rational -> Rational
quantize grid x = fromIntegral (ceiling (x / grid)) * grid

waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- fromIntegral <$> MIDI.currentTime conn
    threadDelay (floor (1000 * (target - now)))

rhythmMain :: Conns -> TVar State -> Rhythm -> IO ()
rhythmMain conns stateVar rhythm = do
    join . atomically $ do
        state <- readTVar stateVar
        if rhythm `Map.member` sActive state then
            return $ return ()
        else do
            chan <- newTChan
            writeTVar stateVar $ state { sActive = Map.insert rhythm (ActiveRecord chan) (sActive state)
                                       , sInactive = Map.delete rhythm (sInactive state) }
            return $ do
                rhythmThread stateVar conns chan rhythm
                now <- fromIntegral <$> MIDI.currentTime (cMainConn conns)
                atomically . modifyTVar stateVar $ \s -> s { sActive = Map.delete rhythm (sActive s)
                                                           , sInactive = Map.insert rhythm (InactiveRecord now) (sInactive s) }

renderState :: State -> IO ()
renderState state = do
    clearScreen
    setCursorPosition 0 0
    let timebase = findGrid (Map.keys (sActive state))
    let period = findPeriod (Map.keys (sActive state))
    putStrLn $ "grid:   " ++ show (round timebase) ++ "ms"
    putStrLn $ "period: " ++ show (round period) ++ "ms"
    putStrLn $ "key:    " ++ show (snd . Map.findMax $ sKey state)

    let padding = maximum [ length (rRole r) | r <- Map.keys (sActive state) ]
    mapM_ (putStrLn . renderRhythm timebase padding) (Map.keys (sActive state))

    dietime <- readIORef timeToDie
    when dietime $ putStrLn "Winding down..."

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

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM condm action = do
    r <- condm
    when r (action >> whileM condm action)

mainThread :: Kit -> Conns -> IO ()
mainThread chkit conns = do
    stateVar <- newTVarIO $ State { sActive = Map.empty, sInactive = Map.empty, sKey = Map.singleton 0 Scale.cMinorPentatonic }
    -- display thread
    void . forkIO . forever $ do
        state <- atomically (readTVar stateVar)
        renderState state
        atomically $ do
            state' <- readTVar stateVar
            when (state == state') retry
    -- song evolution thread:
    whileM (liftA2 (||) (not <$> readIORef timeToDie) 
                        (not . null . sActive <$> atomically (readTVar stateVar))) $ do
        makeChange stateVar
        state <- atomically $ readTVar stateVar
        now <- fromIntegral <$> MIDI.currentTime (cMainConn conns)
        let period = findPeriod (Map.keys (sActive state))
        let next = quantize period (now + fromIntegral modTime) - 200   -- make modification slightly before beginning of phrase
          -- so thread has time to start on time (& maybe even pickup)
        waitTill (cMainConn conns) next
  where
  makeChange stateVar = do
    voices <- fromIntegral . length . sActive <$> atomically (readTVar stateVar)
    id =<< evalRandIO (weighted [ 
        (newRhythm stateVar, fromIntegral averageVoices), 
        (sendRandMessage MsgTerm stateVar, voices) ])

  newRhythm stateVar = do
    dietime <- readIORef timeToDie
    if dietime then sendRandMessage MsgTerm stateVar else newRhythmReal stateVar

  newRhythmReal stateVar = void . forkIO $ do
    state <- atomically (readTVar stateVar)  -- XXX another race condition, could make two incompatible rhythms
    now <- fromIntegral <$> MIDI.currentTime (cMainConn conns)
    ret <- evalRandIO $ do
        pastr <- fmap (state,) <$> makeDerivedRhythm chkit (Map.keys (sActive state))
        newr <- choosePastRhythm state now
        uniform [pastr `mplus` newr, newr `mplus` pastr]
    case ret of
        Nothing -> return()
        Just (state', r) -> do
            atomically (writeTVar stateVar state')
            rhythmMain conns stateVar r

  sendRandMessage sig stateVar = do
    state <- atomically (readTVar stateVar)
    evalRandIO (uniformMay (sActive state)) >>= \case
        Nothing -> return ()
        Just record -> atomically $ writeTChan (arMessageChan record) sig


type Cloud = Rand StdGen

randomNote :: Kit -> String -> Cloud Note
randomNote chkit role = do
    let instr = chkit Map.! role
    pitch <- uniform (iPitches instr)
    vel <- id =<< uniform [return 0, getRandomR (32,127)]
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
        , rModulate = iModulate (chkit Map.! role)
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

choosePastRhythm :: State -> Time -> Cloud (Maybe (State, Rhythm))
choosePastRhythm state now = do
    choice <- weightedMay $ do
        (rhythm, irecord) <- Map.assocs (sInactive state)
        guard (Map.keys (sActive state) `admits` rhythm)
        return (rhythm, now - irLastKillTime irecord)
    return $ fmap (state,) choice

ratToInt :: Rational -> Maybe Integer
ratToInt r | denominator r == 1 = Just (numerator r)
           | otherwise = Nothing

divisors :: (Integral a) => a -> [a]
divisors n = [ m | m <- [1..n], n `mod` m == 0 ]

makeDerivedRhythm :: Kit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythm chkit [] = do
    timing <- (2000 %) <$> getRandomR (4,12)
    notes  <- uniform [3..8]
    Just <$> makeRhythm chkit timing notes
makeDerivedRhythm chkit rs = 
    uniformMay (Map.keysSet chkit `Set.difference` Set.fromList (map rRole rs)) >>= \case
    Nothing -> return Nothing
    Just role -> do
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

rootTonal :: Scale.Range -> Instrument
rootTonal range = defaultInstrument { iPitches = map (RootTonal range) [0..m] }
    where
    m = ceiling (fromIntegral (snd range - fst range) * 7/12)

shiftTonal :: Scale.Range -> Instrument
shiftTonal range = defaultInstrument { iPitches = map (ShiftTonal range) [0..m] }
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

studioDrummerKit :: Kit
studioDrummerKit = Map.fromList [
  "kick"  --> perc (APCCoord 1 1 (0,0,1)) [36],
  "snare" --> perc (APCCoord 1 2 (1,1,0)) [37, 38, 39, 40],
  "hat"   --> perc (APCCoord 1 3 (0,1,0)) [42, 44, 46],
  "tom"   --> perc (APCCoord 1 4 (1,0,0)) [41, 43, 45, 47],
  "ride"  --> perc (APCCoord 1 5 (1,0,1)) [50, 53]
  ]

{-
repThatKit :: Kit
repThatKit = Map.fromList [
  "kick"  --> perc [48, 49, 60, 61, 72, 73],
  "snare" --> perc [50, 51, 52, 62, 63, 64, 74, 75, 76],
  "hat"   --> perc [42, 46, 54, 56, 58, 66, 68, 70, 78, 80, 82],
  "ride"  --> perc [59, 83],
  "perc"  --> perc [43, 53, 55, 65, 67, 77, 79]
  ]

{-
gamillionKit :: Kit
gamillionKit = Map.fromList [
  --"kick"  --> perc [36, 37, 48, 49, 60, 61],
  --"snare" --> perc [38, 39, 40, 50, 51, 52, 62, 63, 64],
  --"hat"   --> perc [42, 44, 46, 54, 56, 58, 66, 68, 70],
  --"bell1" --> perc [41, 43, 45, 47],
  --"bell2" --> perc [53, 55, 57, 59],
  --"bell3" --> perc [65, 67, 69, 71],
  --"bell4" --> perc [72..83]
  "bell" --> perc ([41,43,45,47, 53,55,57,59, 65,67,69,71] ++ [72..83])
  ]
-}

cMinorKit :: Kit
cMinorKit = Map.fromList [
    "bass" --> rootTonal (31,48)
  , "mid"  --> shiftTonal (51,72)
  , "high" --> shiftTonal (72,89)
  , "pedal" --> pedal
  ]

cMinorBassKit :: Kit
cMinorBassKit = Map.fromList [
  "bass" --> rootTonal (31,48)
  ]

chordKit :: Kit
chordKit = Map.fromList [
    "chord" --> (chords $ 
        [ Scale.transposeChr d Scale.cMajor | d <- [0..11] ] ++
        [ Scale.transposeChr d Scale.cMinor | d <- [0..11] ])
    ]
-}

glitchKit :: Kit
glitchKit = Map.fromList [
    "kick"  --> perc (APCCoord 2 1 (0,0,1)) [36,37,48,49,60,61,72,73],
    "snare" --> perc (APCCoord 2 2 (1,1,0)) [38,40,50,52,62,64,74,76],
    "hat"   --> perc (APCCoord 2 3 (0,1,0)) [44,46,58],
    "click" --> perc (APCCoord 2 4 (0,1,1)) [39,41,42,43,53,54,56,57,59
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
    --, ("keys", 3, cMinorKit)
    --, ("bass", 2, cMinorBassKit)
    --, ("chord", 0, chordKit)
    ]

timeToDie :: IORef Bool
timeToDie = unsafePerformIO $ newIORef False

main :: IO ()
main = do
    !conns <- openConns
    void $ installHandler sigINT (Catch onInt) Nothing
    mainThread myKit conns
    where
    onInt = do
        dietime <- readIORef timeToDie
        if dietime
            then raiseSignal sigTERM
            else writeIORef timeToDie True
