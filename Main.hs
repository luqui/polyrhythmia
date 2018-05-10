{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
{-# LANGUAGE BangPatterns, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import qualified Data.Map as Map
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_, when)
import Control.Concurrent.STM
import Control.Applicative
import Data.Foldable (toList)
import Data.Maybe (maybeToList)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl1')
import Control.Monad.Random
import Data.Ratio
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM, raiseSignal)

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
averageVoices = 7

maxModTimeSeconds, meanModTimeSeconds :: Double
maxModTimeSeconds = 10
meanModTimeSeconds = 3

mutateProbability :: Double
mutateProbability = 0.25

-- END TWEAKS --

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 

type Time = Rational  -- millisecs

data State = State {
  sActive :: Map.Map Rhythm (TChan Signal),
  sInactive :: Map.Map Rhythm Time,  -- value is timestamp when it was last killed
  sKey :: Scale.Scale
 }
 deriving (Eq)

data Pitch = Percussion Scale.MIDINote
           | RootTonal  Scale.Range Int
           | ShiftTonal Scale.Range Int
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
    , iPitches :: [Pitch]
    } 

data Note = Note Int Pitch Int Rational  -- ch note vel dur  (dur in fraction of voice timing)
    deriving (Eq, Ord, Show)

data Rhythm = Rhythm {
    rTiming :: Rational,
    rRole :: String,
    rNotes :: [Note],
    rPeriodExempt :: Bool } 
    deriving (Eq, Ord, Show)

timeLength :: Rhythm -> Rational
timeLength r = fromIntegral (length (rNotes r)) * rTiming r

findPeriod :: (Foldable f) => f Rhythm -> Rational
findPeriod f | null f = 1
             | otherwise = foldl1' lcmRat . map timeLength . filter (not . rPeriodExempt) . toList $ f

findGrid :: (Foldable f) => f Rhythm -> Rational
findGrid f | null f = 0
           | otherwise = foldl1' gcdRat . map rTiming . toList $ f

type Kit = Map.Map String Instrument

data Signal 
    = SigKill
    | SigMutate
    deriving Show

rhythmThread :: Kit -> TVar State -> MIDI.Connection -> TChan Signal -> Rhythm -> IO Rhythm
rhythmThread chkit stateVar conn chan rhythm0 = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = quantize timing now
    go rhythm0 starttime

    where
    timing = rTiming rhythm0

    playNote :: Double -> Time -> Note -> IO ()
    playNote vmod t (Note ch pitch vel dur) = do
        waitTill conn t
        let vel' = round (fromIntegral vel * vmod)
        when (vel' > 0) $
            pitchToMIDI pitch >>= \case
                Just note | vel' > 0 -> do
                     MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel'))
                     waitTill conn (t + dur * timing)
                     MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
                _ -> do
                     waitTill conn (t + dur * timing)

    pitchToMIDI (Percussion n) = return (Just n)
    pitchToMIDI (RootTonal range deg) = do
        key <- atomically $ sKey <$> readTVar stateVar
        return . Just $ Scale.apply key range deg
    pitchToMIDI (ShiftTonal range deg) = do
        key <- atomically $ sKey <$> readTVar stateVar
        return . Just $ Scale.applyShift key range 0 deg
    pitchToMIDI (GlobalScaleChange scale) = do
        atomically . modifyTVar stateVar $ \s -> s { sKey = scale }
        return Nothing

    playPhrase rhythm t0 = do
        let times = [t0, t0 + timing ..]
        let !ret = last (zipWith const times (rNotes rhythm ++ [error "too clever"]))
        forM_ (zip times (rNotes rhythm)) $ \(t, note) -> playNote 1 t note
        return ret

    fadePhrase rhythm t0 = do
        let times = [t0, t0 + timing ..]
        let velmod = [1,1-1/40..1/40]
        forM_ (zip3 velmod times (cycle (rNotes rhythm))) $ \(vmod, t, note) -> 
            playNote vmod t note
             
    go rhythm t0 = do
        sig <- atomically (tryReadTChan chan)
        case sig of
            Nothing -> playPhrase rhythm t0 >>= go rhythm
            Just SigKill -> fadePhrase rhythm t0 >> return rhythm
            Just SigMutate -> do
                rhythm' <- evalRandIO $ mutate chkit rhythm
                atomically . modifyTVar stateVar $ \s -> s {
                    sActive = Map.insert rhythm' chan (Map.delete rhythm (sActive s))
                  }
                playPhrase rhythm' t0 >>= go rhythm'
             
quantize :: Rational -> Rational -> Rational
quantize grid x = fromIntegral (floor (x / grid)) * grid

mutate :: Kit -> Rhythm -> Cloud Rhythm
mutate chkit r = do
    notes' <- mapM mutateNote (rNotes r)
    return $ r { rNotes = notes' }
    where
    mutateNote note = do
        p <- getRandomR (0,1 :: Double)
        if p <= mutateProbability
            then randomNote chkit (rRole r)
            else return note

waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- fromIntegral <$> MIDI.currentTime conn
    threadDelay (floor (1000 * (target - now)))

rhythmMain :: Kit -> MIDI.Connection -> TVar State -> Rhythm -> IO ()
rhythmMain chkit conn stateVar rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    maychan <- atomically $ do
        state <- readTVar stateVar
        if rhythm `Map.member` sActive state then
            return Nothing
        else do
            chan <- newTChan
            -- Insert into the inactive set immediately, so that if the rhythm mutates,
            -- we can ressurect the original, which is more musically satisfying.
            writeTVar stateVar $ state { sActive = Map.insert rhythm chan (sActive state)
                                       , sInactive = Map.insert rhythm now (sInactive state) }
            return (Just chan)
    case maychan of
        Nothing -> return ()
        Just chan -> do
            rhythm' <- rhythmThread chkit stateVar conn chan rhythm
            atomically . modifyTVar stateVar $ \s -> 
                s { sActive = Map.delete rhythm' (sActive s)
                  , sInactive = Map.insert rhythm' now (sInactive s)
                  }

renderState :: State -> IO ()
renderState state = do
    clearScreen
    setCursorPosition 0 0
    let timebase = findGrid (Map.keys (sActive state))
    let period = findPeriod (Map.keys (sActive state))
    putStrLn $ "grid:   " ++ show (round timebase) ++ "ms"
    putStrLn $ "period: " ++ show (round period) ++ "ms"
    putStrLn $ "key:    " ++ show (sKey state)

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
    renderNote (Note _ _ v _) | v == 0    = "."
                              | v < 75    = "x"
                              | otherwise = "X"
    padString p s = take p (s ++ repeat ' ')

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM condm action = do
    r <- condm
    when r (action >> whileM condm action)

mainThread :: Kit -> MIDI.Connection -> IO ()
mainThread chkit conn = do
    stateVar <- newTVarIO $ State { sActive = Map.empty, sInactive = Map.empty, sKey = Scale.cMinorPentatonic }
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
        -- exp distribution
        param <- evalRandIO $ getRandomR (0, 1 :: Double)
        let delay = min (maxModTimeSeconds*10^6) (-log param * meanModTimeSeconds*10^6) 
        threadDelay (floor delay)
  where
  makeChange stateVar = do
    voices <- fromIntegral . length . sActive <$> atomically (readTVar stateVar)
    id =<< evalRandIO (weighted [ 
        (newRhythm stateVar, fromIntegral averageVoices), 
        (sendRandSignal SigKill stateVar, voices), 
        (sendRandSignal SigMutate stateVar, voices) ])

  newRhythm stateVar = do
    dietime <- readIORef timeToDie
    if dietime then sendRandSignal SigKill stateVar else newRhythmReal stateVar

  newRhythmReal stateVar = void . forkIO $ do
    state <- atomically (readTVar stateVar)  -- XXX another race condition, could make two incompatible rhythms
    now <- fromIntegral <$> MIDI.currentTime conn
    ret <- evalRandIO $ do
        pastr <- fmap (state,) <$> makeDerivedRhythm chkit (Map.keys (sActive state))
        newr <- choosePastRhythm state now
        uniform [pastr `mplus` newr, newr `mplus` pastr]
    case ret of
        Nothing -> return()
        Just (state', r) -> do
            atomically (writeTVar stateVar state')
            rhythmMain chkit conn stateVar r

  sendRandSignal sig stateVar = do
    state <- atomically (readTVar stateVar)
    evalRandIO (uniformMay (sActive state)) >>= \case
        Nothing -> return ()
        Just chan -> atomically $ writeTChan chan sig


type Cloud = Rand StdGen

randomNote :: Kit -> String -> Cloud Note
randomNote chkit role = do
    let instr = chkit Map.! role
    pitch <- uniform (iPitches instr)
    vel <- id =<< uniform [return 0, getRandomR (32,127)]
    dur <- uniform [1/10, 1/2, 9/10, 1]
    return $ Note (iChannel instr) pitch vel dur

makeRhythmRole :: Kit -> String -> Rational -> Int -> Cloud Rhythm
makeRhythmRole chkit role timing numNotes = do
    notes <- replicateM numNotes (randomNote chkit role)
    return $ Rhythm timing role notes (iPeriodExempt (chkit Map.! role))

makeRhythm :: Kit -> Rational -> Int -> Cloud Rhythm
makeRhythm chkit timing numNotes = do
    role <- uniform (Map.keys chkit)
    makeRhythmRole chkit role timing numNotes

admits :: (Foldable f) => f Rhythm -> Rhythm -> Bool
admits rs = \cand -> and [ minimumGrid <= gcdRat grid (rTiming cand)
                       , lcmRat period (timeLength cand) <= maximumPeriod
                       ]
    where
    grid = findGrid rs
    period = findPeriod rs

choosePastRhythm :: State -> Time -> Cloud (Maybe (State, Rhythm))
choosePastRhythm state now = do
    choice <- weightedMay $ do
        (rhythm, time) <- Map.assocs (sInactive state)
        guard (Map.keys (sActive state) `admits` rhythm)
        return (rhythm, now - time)
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
makeDerivedRhythm chkit rs = do
    role <- uniform (Map.keysSet chkit) -- uniformMay (Map.keysSet kit `Set.difference` Set.fromList (map rRole rs))
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

lcmRat :: Rational -> Rational -> Rational
lcmRat r r' = recip (gcdRat (recip r) (recip r'))

(-->) :: a -> b -> (a,b)
(-->) = (,)

defaultInstrument :: Instrument
defaultInstrument = Instrument
    { iMinLength = minimumNote
    , iMaxLength = maximumNote
    , iMinNotes = 3
    , iMaxNotes = 16
    , iPeriodExempt = False
    , iChannel = 0
    , iPitches = []
    }

perc :: [Int] -> Instrument
perc notes = defaultInstrument { iPitches = map Percussion notes }

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
    , iMaxNotes = 4
    , iPeriodExempt = True
    }

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

sAndBKit :: Kit
sAndBKit = Map.fromList [
  "kick"  --> perc [36, 37, 48, 49, 60, 61, 72, 73, 84, 85],
  "snare" --> perc [38, 39, 40, 50, 51, 52, 62, 63, 64, 74, 75],
  "hat"   --> perc [42, 44, 46, 54, 56, 58, 66],
  "perc1" --> perc [41, 43, 45, 47],
  "perc2" --> perc [53, 55, 57, 59],
  "perc3" --> perc [65, 67, 86, 88, 91, 92, 93, 94, 95, 98, 100]
  ]

cMinorKit :: Kit
cMinorKit = Map.fromList [
    "bass" --> rootTonal (31,48)
  , "mid"  --> shiftTonal (51,72)
  , "high" --> shiftTonal (72,89)
  --, "high-alt" --> [72,73,75,76,78,80,82,84,85,87,88]
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

makeKit :: [(String, Int, Kit)] -> Kit
makeKit kits = Map.unions 
    [ Map.mapKeysMonotonic ((name ++ ".") ++) . fmap (\i -> i { iChannel = ch }) $ kit 
    | (name, ch, kit) <- kits ]

myKit :: Kit
myKit = makeKit [
      ("kit", 1, repThatKit)
    --, ("bell", 2, gamillionKit)
    --, ("elec", 3, sAndBKit)
    , ("keys", 4, cMinorKit)
    , ("bass", 5, cMinorBassKit)
    , ("chord", 0, chordKit)
    ]

timeToDie :: IORef Bool
timeToDie = unsafePerformIO $ newIORef False

main :: IO ()
main = do
    !conn <- openConn
    MIDI.start conn
    void $ installHandler sigINT (Catch onInt) Nothing
    mainThread myKit conn
    where
    onInt = do
        dietime <- readIORef timeToDie
        if dietime
            then raiseSignal sigTERM
            else writeIORef timeToDie True
