{-# OPTIONS_GHC -Wall -Wno-type-defaults -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns, TupleSections, LambdaCase, MultiWayIf #-}

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans.State as State
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.MIDI as MIDI
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Monad (filterM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (STM, TVar)
import Data.Foldable (toList)
import Data.Maybe (maybeToList)
import Data.List (foldl', foldl1')
import Data.Ratio (numerator, denominator, (%))
import GHC.Conc (unsafeIOToSTM)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)

import Control.Monad.Random hiding (next)
import Control.Monad.Trans.Reader

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
    arMessageChan :: STM.TChan Message,
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

-- A GhostEntry records whether an apc key was pressed recently enough to
-- warrant playing the note, and also whether a note was triggered recently
-- enough to play when the apc key is pressed.
data GhostEntry = GhostNone | GhostNote ActiveRecord Note | GhostKey

type GhostMap = Map.Map (Int,Int) (TVar GhostEntry)

data Env = Env {
    eConns    :: Conns,
    eStateVar :: TVar State,
    eGhostMap :: GhostMap
  }

type PolyT = ReaderT Env

class (Monad m) => MonadFork m where
    forkM :: m () -> m ThreadId

instance MonadFork IO where forkM = forkIO
instance (MonadFork m) => MonadFork (ReaderT r m) where
    forkM m = ReaderT $ forkM . runReaderT m

forkM_ :: (MonadFork m) => m () -> m ()
forkM_ = void . forkM


class (Monad m) => MonadVars m where
    newVar :: a -> m (TVar a)
    readVar :: TVar a -> m a
    writeVar :: TVar a -> a -> m ()
    modifyVar :: TVar a -> (a -> a) -> m ()

instance MonadVars IO where
    newVar = STM.newTVarIO
    readVar = STM.atomically . STM.readTVar
    writeVar v = STM.atomically . STM.writeTVar v
    modifyVar v = STM.atomically . STM.modifyTVar v

instance MonadVars STM where
    newVar = STM.newTVar
    readVar = STM.readTVar
    writeVar = STM.writeTVar
    modifyVar = STM.modifyTVar

instance (MonadVars m) => MonadVars (ReaderT r m) where
    newVar = lift . newVar
    readVar = lift . readVar
    writeVar v = lift . writeVar v
    modifyVar v = lift . modifyVar v

instance (MonadVars m) => MonadVars (State.StateT s m) where
    newVar = lift . newVar
    readVar = lift . readVar
    writeVar v = lift . writeVar v
    modifyVar v = lift . modifyVar v


atomicallyP :: PolyT STM a -> PolyT IO a
atomicallyP m = ReaderT $ STM.atomically . runReaderT m

getState :: (MonadVars m) => PolyT m State
getState = readVar =<< asks eStateVar

modifyState :: (MonadVars m) => (State -> State) -> PolyT m ()
modifyState f = flip modifyVar f =<< asks eStateVar

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

whenAPC :: (Monad m) => (APC40.Devs -> PolyT m ()) -> PolyT m ()
whenAPC f = do
    apcConn <- asks (cAPC . eConns)
    case apcConn of
        Just conn -> f conn
        Nothing   -> return ()

sendMIDI :: MIDI.MidiMessage -> PolyT IO ()
sendMIDI msg = do
    midiConn <- asks (cMainConn . eConns)
    liftIO $ MIDI.send midiConn msg

currentTime :: PolyT IO Time
currentTime = fmap fromIntegral . liftIO . MIDI.currentTime =<< asks (cMainConn . eConns)

playNoteNow :: Rational -> Note -> PolyT IO ()
playNoteNow _timing (Note _ch _pitch 0 _dur _coord) = return ()
playNoteNow timing (Note ch pitch vel dur coord) = do
    (midi0, midi1) <- pitchToMIDI pitch 
    (apc0, apc1) <- apcCommands coord
    midi0
    apc0

    let lightOffTime = 0.9 * minimumGrid
    let noteOffTime = dur * timing

    if lightOffTime < noteOffTime then do
        liftIO $ threadDelay (floor (1000 * lightOffTime))
        apc1
        liftIO $ threadDelay (floor (1000 * (noteOffTime - lightOffTime)))
        midi1
    else do
        liftIO $ threadDelay (floor (1000 * noteOffTime))
        midi1
        liftIO $ threadDelay (floor (1000 * (lightOffTime - noteOffTime)))
        apc1

    where
    infix 0 //
    a // b = return (a, b)

    apcCommands :: APCCoord -> PolyT IO (PolyT IO (), PolyT IO ())
    apcCommands (APCCoord apccoord color) = do
        whenAPC (liftIO . APC40.lightOn apccoord (rgbMagToVel color (fromIntegral vel / 127)))
            // whenAPC (liftIO . APC40.lightOn apccoord 0)
    apcCommands NoCoord = return () // return ()

    pitchToMIDI :: Pitch -> PolyT IO (PolyT IO (), PolyT IO ())
    pitchToMIDI (Percussion n) = do
        sendMIDI (MIDI.MidiMessage ch (MIDI.NoteOn n vel))
            // sendMIDI (MIDI.MidiMessage ch (MIDI.NoteOn n 0))
    pitchToMIDI (RootTonal range deg)  = do
        key <- atomicallyP $ snd . Map.findMax . sKey <$> getState
        let note = Scale.apply key range deg
        sendMIDI (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
            // sendMIDI (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
    pitchToMIDI (ShiftTonal range deg)  = do
        key <- atomicallyP $ snd . Map.findMax . sKey <$> getState
        let note = Scale.apply key range deg
        sendMIDI (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
            // sendMIDI (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
    pitchToMIDI (ControlChange ctrl) =  do
        sendMIDI (MIDI.MidiMessage ch (MIDI.CC ctrl vel))
            // return ()
    pitchToMIDI (GlobalScaleChange scale) = do
        stateVar <- asks eStateVar
        modifyVar stateVar (\s -> s { sKey = Map.insert vel scale (sKey s) })
          // modifyVar stateVar (\s -> s { sKey = Map.delete vel (sKey s) })
    
playGhostNote :: ActiveRecord -> Rational -> Note -> PolyT IO ()
playGhostNote activerecord timing note@(Note _ch _pitch _vel _dur (APCCoord coord _color)) = do
    ghostMap <- asks eGhostMap
    whenMay (Map.lookup coord ghostMap) $ \tvar -> do
        join . liftIO . STM.atomically $ readVar tvar >>= \case
            GhostNone -> do
                writeVar tvar (GhostNote activerecord note)
                return . void . liftIO . forkIO $ do
                    threadDelay (1000*inputAccuracy)
                    writeVar tvar GhostNone
            GhostKey -> do
                modifyVar (arGhostCount activerecord) $ \gc -> gc { gcHits = gcHits gc + 1 }
                return $ playNoteNow timing note
            GhostNote _ _ -> return . liftIO $ do
                -- Theoretically shouldn't happen
                putStrLn "Double ghost! Boo!"
playGhostNote _ _ _ = return ()

deathModulateNote :: Double -> Note -> Note
deathModulateNote lambda n = n { nCoord = modCoord (nCoord n) }
    where
    modCoord NoCoord = NoCoord
    modCoord (APCCoord c (r,g,b))
        | lambda > 1    = APCCoord c (1,1,1)
        | lambda < 0.25 = APCCoord c (lambda/0.25,0,0)
        | otherwise  = APCCoord c (param*r + (1-param)*1, param*g, param*b)
        where
        param = (lambda-0.25)/0.75


rhythmThread :: ActiveRecord -> Rhythm -> PolyT IO ()
rhythmThread activerecord rhythm = do
    now <- currentTime
    let starttime = quantize (timeLength rhythm) now
    -- volume modulation
    State.evalStateT (go starttime) 1

    where
    timing = rTiming rhythm

    playNote :: Time -> Note -> State.StateT Double (PolyT IO) ()
    playNote _ (Note _ _ 0 _ _) = return () 
    playNote t inNote = do
        deathmod <- State.get
        let note = deathModulateNote deathmod inNote
        lift $ waitTill t
        join . liftIO . STM.atomically $ do
            count <- readVar (arGhostCount activerecord)
            if | gcTotal count == 0 || gcTries count > gcTotal count -> do
                    return . lift $ playNoteNow timing note
               | gcTries count == gcTotal count -> do
                    writeVar (arGhostCount activerecord) (count { gcTries = gcTries count + 1 })
                    return $ do
                        let win = fromIntegral (gcHits count) >= 0.75 * fromIntegral (gcTotal count)
                        when win $ State.modify (+1)
                        lift . atomicallyP . modifyState $ \s -> s { sScore = sScore s + 1 }
                        lift $ playGhostNote activerecord timing note
                        lift . forkM_ $ do
                            let color | win = 122
                                      | otherwise = 120
                            makeHappyLights color note
                            writeVar (arGhostCount activerecord) (GhostCount 0 0 0)
                | otherwise -> do
                    writeVar (arGhostCount activerecord) (count { gcTries = gcTries count + 1 })
                    return . lift $ playGhostNote activerecord timing note

    makeHappyLights color (Note _ _ _ _ (APCCoord coord _)) = do
        whenAPC $ \apc -> replicateM_ 8 . liftIO $ do
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
        state <- lift getState
        let end = t0 + timeLength rhythm
        let period = findPeriod (Map.keys (sActive state))
        if end == quantize period end && period /= timeLength rhythm
            then
                --playPhrase t0 (rAltNotes rhythm)
                playPhrase t0 (rNotes rhythm)
            else
                playPhrase t0 (rNotes rhythm)

    go :: Rational -> State.StateT Double (PolyT IO) ()
    go t0 = do
        sig <- liftIO . STM.atomically . STM.tryReadTChan $ arMessageChan activerecord
        deathmod <- subtract deathFadePerCycle <$> State.get
        trying <- fmap ((/= 0) . gcTotal) . readVar $ arGhostCount activerecord  
        State.put deathmod
        case sig of
            Nothing | deathmod > 0 || trying -> chooseAndPlayPhrase t0 >>= go
                    | otherwise              ->
                          lift . modifyState $ \s -> s { sScore = sScore s - 1 }
            Just MsgTerm -> return ()
             
quantize :: Rational -> Rational -> Rational
quantize grid x = fromIntegral (ceiling (x / grid)) * grid

waitTill :: Time -> PolyT IO ()
waitTill target = do
    now <- currentTime
    liftIO $ threadDelay (floor (1000 * (target - now)))

rhythmMain :: Rhythm -> PolyT IO ()
rhythmMain rhythm = do
    join . atomicallyP $ do
        stateVar <- asks eStateVar
        state <- readVar stateVar
        if rhythm `Map.member` sActive state then
            return $ return ()
        else do
            activerecord <- lift $ ActiveRecord <$> STM.newTChan <*> STM.newTVar (GhostCount 0 0 0)
            writeVar stateVar $ state { sActive = Map.insert rhythm activerecord (sActive state)
                                      , sInactive = Map.delete rhythm (sInactive state) }
            return $ do
                rhythmThread activerecord rhythm
                now <- currentTime
                modifyVar stateVar $ \s -> s { sActive = Map.delete rhythm (sActive s)
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


unsafePollChanges :: STM a -> IO a
unsafePollChanges stm = do
    retryVar <- IORef.newIORef True
    STM.atomically $ do
        result <- stm
        retryQ <- unsafeIOToSTM $ IORef.readIORef retryVar
        if retryQ then do
            unsafeIOToSTM $ IORef.writeIORef retryVar False
            STM.retry
        else
            return result

mainThread :: Kit -> Conns -> IO ()
mainThread chkit conns = do
    stateVar <- STM.newTVarIO $ State { sActive = Map.empty, sInactive = Map.empty, sKey = Map.singleton 0 Scale.cMinorPentatonic, sScore = 0 }
    -- display thread
    void . forkIO . forever $ do
        state <- unsafePollChanges $ readVar stateVar
        renderState state

    -- APC thread
    ghostMap <- traverse (\_ -> newVar GhostNone) roleMap

    let env = Env { eStateVar = stateVar
                  , eConns = conns
                  , eGhostMap = ghostMap
                  } 

    flip runReaderT env $ do
        whenAPC $ \apcdevs -> do
            forkM_ . forever $ do
                state <- readVar stateVar
                events <- liftIO $ APC40.pollNotes apcdevs
                forM_ events $ \coord -> do
                    whenMay (Map.lookup coord roleMap) $ \role -> do
                        sequence_ $ do
                            (k,ar) <- Map.assocs (sActive state)
                            guard (rRole k == role)
                            return . atomicallyP $ do
                                count <- readVar (arGhostCount ar)
                                when (gcTotal count == 0) $ writeVar (arGhostCount ar) 
                                                        (GhostCount { gcTotal = 10, gcTries = 0, gcHits = 0 })
                    whenMay (Map.lookup coord ghostMap) $ \ghostVar -> do
                        join . atomicallyP $ readVar ghostVar >>= \case
                            GhostNone -> do
                                writeVar ghostVar GhostKey
                                return . forkM_ $ do
                                    liftIO $ threadDelay (1000 * inputAccuracy)
                                    writeVar ghostVar GhostNone
                            GhostKey -> return $ return ()
                            GhostNote activerecord note -> do
                                -- XXX minimumGrid is not correct
                                -- Bundle timing with GhostNote constructor
                                modifyVar (arGhostCount activerecord) $ \gc -> gc { gcHits = gcHits gc + 1 }
                                return $ forkM_ (playNoteNow minimumGrid note)
                liftIO $ threadDelay 10000
                
        -- song evolution thread:
        forever $ do
            newRhythm Nothing 
            state <- getState
            now <- currentTime
            let period = findPeriod (Map.keys (sActive state))
            let next = quantize period (now + fromIntegral modTime) - 200
              -- make modification slightly before beginning of phrase
              -- so thread has time to start on time (& maybe even pickup)
            waitTill next
    
  where
  newRhythm mayrole = forkM_ $ do
    state <- getState  -- XXX another race condition, could make two incompatible rhythms
    ret <- liftIO . evalRandIO $ do
        newr <- fmap (state,) <$> makeDerivedRhythmG mayrole chkit (Map.keys (sActive state))
        --pastr <- choosePastRhythm mayrole state now
        --uniform [pastr `mplus` newr, newr `mplus` pastr]
        return newr
    case ret of
        Nothing -> return()
        Just (state', r) -> do
            stateVar <- asks eStateVar
            writeVar stateVar state'
            rhythmMain r


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
