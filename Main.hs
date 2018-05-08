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
minimumGrid, maximumPeriod, minimumNote, maximumNote :: Rational
minimumGrid = 1000/16  -- 16th of a second
maximumPeriod = 1000 * 10
minimumNote = 1000/8
maximumNote = 1000/2

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

data Pitch = Percussion Scale.MIDINote
           | RootTonal  Scale.Range Int
           | ShiftTonal Scale.Range Int
           | GlobalScaleChange Scale.Scale
    deriving (Eq, Ord, Show)

data Note = Note Int Pitch Int Rational  -- ch note vel dur  (dur in fraction of voice timing)
    deriving (Eq, Ord, Show)

data Rhythm = Rhythm {
    rTiming :: Rational,
    rRole :: String,
    rNotes :: [Note] } 
    deriving (Eq, Ord, Show)

timeLength :: Rhythm -> Rational
timeLength r = fromIntegral (length (rNotes r)) * rTiming r

findPeriod :: (Foldable f) => f Rhythm -> Rational
findPeriod f | null f = 1
             | otherwise = foldl1' lcmRat . map timeLength . toList $ f

findGrid :: (Foldable f) => f Rhythm -> Rational
findGrid f | null f = 0
           | otherwise = foldl1' gcdRat . map rTiming . toList $ f

type Kit = Map.Map String [Pitch]

type ChKit = Map.Map String [(Int,Pitch)] -- channel, note

data Signal 
    = SigKill
    | SigMutate
    deriving Show

rhythmThread :: ChKit -> TVar State -> MIDI.Connection -> TChan Signal -> Rhythm -> IO Rhythm
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
        mayNote <- pitchToMIDI pitch
        case mayNote of
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

mutate :: ChKit -> Rhythm -> Cloud Rhythm
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

rhythmMain :: ChKit -> MIDI.Connection -> TVar State -> Rhythm -> IO ()
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

mainThread :: ChKit -> MIDI.Connection -> IO ()
mainThread chkit conn = do
    stateVar <- newTVarIO $ State { sActive = Map.empty, sInactive = Map.empty, sKey = Scale.cMinorPentatonic }
    -- Display thread
    void . forkIO . forever $ do
        renderState =<< atomically (readTVar stateVar)
        threadDelay 1000000
    
    -- Song evolution thread
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

randomNote :: ChKit -> String -> Cloud Note
randomNote chkit role =
     uncurry Note <$> uniform (chkit Map.! role) 
                  <*> (id =<< uniform [return 0, getRandomR (32,127)]) 
                  <*> uniform [1/10, 1/2, 9/10, 1]

makeRhythmRole :: ChKit -> String -> Rational -> Int -> Cloud Rhythm
makeRhythmRole chkit role timing numNotes = do
    notes <- replicateM numNotes (randomNote chkit role)
    return $ Rhythm timing role notes

makeRhythm :: ChKit -> Rational -> Int -> Cloud Rhythm
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

divisors :: Integer -> [Integer]
divisors n = [ m | m <- [1..n], n `mod` m == 0 ]

makeDerivedRhythm :: ChKit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythm chkit [] = do
    timing <- (2000 %) <$> getRandomR (4,12)
    notes  <- uniform [3..8]
    Just <$> makeRhythm chkit timing notes
makeDerivedRhythm chkit rs = do
    role <- uniform (Map.keysSet chkit) -- uniformMay (Map.keysSet kit `Set.difference` Set.fromList (map rRole rs))
    let grid = findGrid rs
    let newGrids = map (grid/) [1..fromIntegral (floor (grid/minimumGrid))]

    let period = findPeriod rs
    let newPeriods = map (period*) [1..fromIntegral (floor (maximumPeriod/period))]

    selection <- uniformMay $ do
        g <- newGrids
        p <- newPeriods
        timing <- map (g*) [1..fromIntegral (floor (maximumNote/g))]
        guard (minimumNote <= timing && timing <= maximumNote)

        maxnotes <- maybeToList (ratToInt (p/timing))
        notes <- divisors maxnotes
        guard (3 <= notes && notes <= 16)
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

{-
repThatKit :: Kit
repThatKit = Map.fromList [
  "kick " --> perc [48, 49, 60, 61, 72, 73],
  "snare" --> perc [50, 51, 52, 62, 63, 64, 74, 75, 76],
  "hat  " --> perc [42, 46, 54, 56, 58, 66, 68, 70, 78, 80, 82],
  "ride " --> perc [59, 83],
  "perc " --> perc [43, 53, 55, 65, 67, 77, 79]
  ]
  where
  perc = map Percussion

gamillionKit :: Kit
gamillionKit = Map.fromList [
  --"kick " --> [36, 37, 48, 49, 60, 61],
  --"snare" --> [38, 39, 40, 50, 51, 52, 62, 63, 64],
  --"hat  " --> [42, 44, 46, 54, 56, 58, 66, 68, 70],
  --"bell1" --> [41, 43, 45, 47],
  --"bell2" --> [53, 55, 57, 59],
  --"bell3" --> [65, 67, 69, 71],
  --"bell4" --> [72..83]
  "bell" --> ([41,43,45,47, 53,55,57,59, 65,67,69,71] ++ [72..83])
  ]

sAndBKit :: Kit
sAndBKit = Map.fromList [
  "kick" --> [36, 37, 48, 49, 60, 61, 72, 73, 84, 85],
  "snare" --> [38, 39, 40, 50, 51, 52, 62, 63, 64, 74, 75],
  "hat" --> [42, 44, 46, 54, 56, 58, 66],
  "perc1" --> [41, 43, 45, 47],
  "perc2" --> [53, 55, 57, 59],
  "perc3" --> [65, 67, 86, 88, 91, 92, 93, 94, 95, 98, 100]
  ]
-}

cMinorKit :: Kit
cMinorKit = Map.fromList [
    "bass" --> map (RootTonal (31,48)) [0..7]
  , "mid"  --> map (ShiftTonal (51,72)) [0..9]
  , "high" --> map (ShiftTonal (72,89)) [0..7]
  --, "high-alt" --> [72,73,75,76,78,80,82,84,85,87,88]
  ]

{-
cMinorBassKit :: Kit
cMinorBassKit = Map.fromList [
  "bass" --> [31,34,36,39,41,43,46,48]
  ]
-}

chordKit :: Kit
chordKit = Map.fromList [
    "chord" --> (map GlobalScaleChange $
        [ Scale.transposeChr d Scale.cMajor | d <- [0..11] ] ++
        [ Scale.transposeChr d Scale.cMinor | d <- [0..11] ])
    ]

makeChKit :: [(String, Int, Kit)] -> ChKit
makeChKit kits = Map.unions [ Map.mapKeysMonotonic ((name ++ ".") ++) . (fmap.map) (ch,) $ kit | (name, ch, kit) <- kits ]

myKit :: ChKit
myKit = makeChKit [
    --  ("kit", 1, repThatKit)
    --, ("bell", 2, gamillionKit)
    --, ("elec", 3, sAndBKit)
      ("keys", 4, cMinorKit)
    --, ("bass", 5, cMinorBassKit)
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
