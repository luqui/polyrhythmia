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
import Data.List (foldl', foldl1')
import Control.Monad.Random
import Data.Ratio
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM, raiseSignal)

import qualified Scale

-- TWEAKS --
timeScale, minimumGrid, maximumPeriod, minimumNote, maximumNote, minimumChord, maximumChord :: Rational
timeScale = 1.5
minimumGrid = timeScale*1000/16  -- 24th of a second
--maximumPeriod = timeScale*1000*10
maximumPeriod = 4*2000
minimumNote = timeScale*1000/8
maximumNote = timeScale*1000/2
minimumChord = timeScale*2000
maximumChord = maximumPeriod

averageVoices :: Int
averageVoices = 5

modTime :: Int
modTime = 4000

-- END TWEAKS --

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 

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
    , iPitches :: [Pitch]
    , iModulate :: Bool
    } 

data Note = Note Int Pitch Int Rational  -- ch note vel dur  (dur in fraction of voice timing)
    deriving (Eq, Ord, Show)

data Rhythm = Rhythm {
    rPeriodExempt :: Bool,
    rRole :: String,
    rTiming :: Rational,
    rNotes :: [Note],
    rAltNotes :: [Note],  -- played as a pickup to phrase alignment
    rModulate :: Bool,
    rPinned :: Bool     -- don't ever remove this rhythm from the state
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

rhythmThread :: TVar State -> MIDI.Connection -> TChan Message -> Rhythm -> IO ()
rhythmThread stateVar conn chan rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = quantize (timeLength rhythm) now
    -- volume modulation
    volperiod <- evalRandIO $ getRandomR (5000.0,20000.0)
    volamp <- evalRandIO $ getRandomR (0.01, 0.99)
    go volperiod volamp starttime

    where
    timing = rTiming rhythm

    playNote :: Double -> Time -> Note -> IO ()
    playNote vmod t (Note ch pitch vel dur) = do
        waitTill conn t
        let vel' | rModulate rhythm = round (fromIntegral vel * vmod)
                 | otherwise = vel
        when (vel' > 0) $
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

rhythmMain :: MIDI.Connection -> TVar State -> Rhythm -> IO ()
rhythmMain conn stateVar rhythm = do
    join . atomically $ do
        state <- readTVar stateVar
        if rhythm `Map.member` sActive state then
            return $ return ()
        else do
            chan <- newTChan
            writeTVar stateVar $ state { sActive = Map.insert rhythm (ActiveRecord chan) (sActive state)
                                       , sInactive = Map.delete rhythm (sInactive state) }
            return $ do
                rhythmThread stateVar conn chan rhythm
                now <- fromIntegral <$> MIDI.currentTime conn
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
    renderNote (Note _ _ v _) | v == 0    = "."
                              | v < 75    = "x"
                              | otherwise = "X"
    padString p s = take p (s ++ repeat ' ')

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM condm action = do
    r <- condm
    when r (action >> whileM condm action)

emptyState :: State
emptyState = State { sActive = Map.empty, sInactive = Map.empty, sKey = Map.singleton 0 Scale.cMinorPentatonic }

mainThread :: Kit -> [Rhythm] -> MIDI.Connection -> IO ()
mainThread chkit initRhythms conn = do
    stateVar <- newTVarIO emptyState
    -- display thread
    void . forkIO . forever $ do
        state <- atomically (readTVar stateVar)
        renderState state
        atomically $ do
            state' <- readTVar stateVar
            when (state == state') retry
    mapM_ (void . forkIO . rhythmMain conn stateVar) initRhythms
    -- song evolution thread:
    whileM (liftA2 (||) (not <$> readIORef timeToDie) 
                        (not . null . sActive <$> atomically (readTVar stateVar))) $ do
        makeChange stateVar
        state <- atomically $ readTVar stateVar
        now <- fromIntegral <$> MIDI.currentTime conn
        let period = findPeriod (Map.keys (sActive state))
        let next = quantize period (now + fromIntegral modTime) - 200   -- make modification slightly before beginning of phrase
          -- so thread has time to start on time (& maybe even pickup)
        waitTill conn next
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
    now <- fromIntegral <$> MIDI.currentTime conn
    ret <- evalRandIO $ do
        pastr <- fmap (state,) <$> makeDerivedRhythm chkit (Map.keys (sActive state))
        newr <- choosePastRhythm state now
        uniform [pastr `mplus` newr, newr `mplus` pastr]
    case ret of
        Nothing -> return()
        Just (state', r) -> do
            atomically (writeTVar stateVar state')
            rhythmMain conn stateVar r

  sendRandMessage sig stateVar = do
    state <- atomically (readTVar stateVar)
    evalRandIO (uniformMay (Map.filterWithKey (\k _ -> not (rPinned k)) (sActive state))) >>= \case
        Nothing -> return ()
        Just record -> atomically $ writeTChan (arMessageChan record) sig


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
    altNotes <- replicateM numNotes (randomNote chkit role)
    return $ Rhythm 
        { rTiming = timing
        , rRole = role
        , rNotes = notes
        , rPeriodExempt = iPeriodExempt (chkit Map.! role)
        , rAltNotes = altNotes
        , rModulate = iModulate (chkit Map.! role)
        , rPinned = False
        }

makeRhythm :: Kit -> Rational -> Int -> Cloud Rhythm
makeRhythm chkit timing numNotes = do
    role <- uniform (Map.keys chkit)
    makeRhythmRole chkit role timing numNotes

admits :: (Foldable f) => f Rhythm -> Rhythm -> Bool
admits rs = \cand -> and [ minimumGrid <= gcdRat grid (rTiming cand)
                         , rPeriodExempt cand || lcmRat period (timeLength cand) <= maximumPeriod
                         ]
    where
    grid = findGrid rs
    period = findPeriod rs

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
    , iModulate = True
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
    , iMaxNotes = 8
    , iPeriodExempt = True
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
  "kick" --> perc [36],
  "snare" --> perc [37, 38, 39, 40],
  "hat" --> perc [42, 44, 46],
  "tom" --> perc [41, 43, 45, 47],
  "ride" --> perc [50, 53]
  ]

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
    "bass-root" --> rootTonal (31,48)
  , "bass-mode" --> shiftTonal (31,48)
  , "mid-root"  --> rootTonal (51,72)
  , "mid-mode"  --> shiftTonal (51,72)
  , "high-root" --> rootTonal (72,89)
  , "high-mode" --> shiftTonal (72,89)
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

glitchKit :: Kit
glitchKit = Map.fromList [
    "kick"  --> perc [36,37,48,49,60,61,72,73],
    "snare" --> perc [38,40,50,52,62,64,74,76],
    "hat"   --> perc [44,46,58],
    "click" --> perc [39,41,42,43,53,54,56,57,59
                     ,69,77,78,79,81,83,84,86,89]
    ]

ambientKit :: Kit
ambientKit = Map.fromList [
    "ambient" --> perc [1..63]
    ]

makeKit :: [(String, Int, Kit)] -> Kit
makeKit kits = Map.unions 
    [ Map.mapKeysMonotonic ((name ++ ".") ++) . fmap (\i -> i { iChannel = ch }) $ kit 
    | (name, ch, kit) <- kits ]

myKit :: Kit
myKit = makeKit [
    ("kit", 4, studioDrummerKit)
    --, ("bell", 2, gamillionKit)
    --, ("elec", 3, sAndBKit)
    --, ("keys", 3, cMinorKit)
    -- , ("bass", 2, cMinorBassKit)
    -- , ("chord", 0, chordKit)
    --, ("glitch", 1, glitchKit)
    -- ("ambient" ++ show n, n, ambientKit)
    -- | n <- [1..6]
    ]

upHat :: Rhythm
upHat = Rhythm 
    { rPeriodExempt = False
    , rRole = "kit.hat"
    , rTiming = timeScale * 250
    , rNotes = [ Note 4 (Percussion 44) 0 1
               , Note 4 (Percussion 44) 72 1
               , Note 4 (Percussion 44) 0 1
               , Note 4 (Percussion 44) 72 1 ]
    , rAltNotes = rNotes upHat
    , rModulate = False
    , rPinned = True
    }

mozartBass :: Rhythm
mozartBass = Rhythm
    { rPeriodExempt = False
    , rRole = "bass.bass"
    , rTiming = timeScale * 250
    , rNotes = [ Note 2 (RootTonal range 0) 88 1
               , Note 2 (RootTonal range 4) 66 1
               , Note 2 (RootTonal range 2) 77 1
               , Note 2 (RootTonal range 4) 66 1 ]
    , rAltNotes = rNotes mozartBass
    , rModulate = True
    , rPinned = True
    }
    where
    range = (48,72)

blueBossa :: Rhythm
blueBossa = Rhythm
    { rPeriodExempt = True
    , rRole = "chord.chord"
    , rTiming = timeScale * 1000
    , rNotes = [ Note 0 (GlobalScaleChange (Scale.transposeChr 0 Scale.cMinor)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 0 Scale.cMinor)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 5 Scale.cDorian)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 5 Scale.cDorian)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 2 Scale.cHalfDim)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 7 Scale.cDominant)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 0 Scale.cMinor)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 10 Scale.cDominant)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 3 Scale.cDorian)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 8 Scale.cDominant)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 1 Scale.cMajor)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 1 Scale.cMajor)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 2 Scale.cHalfDim)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 7 Scale.cDominant)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 0 Scale.cMinor)) 64 1
               , Note 0 (GlobalScaleChange (Scale.transposeChr 7 Scale.cDominant)) 64 1
               ]
    , rAltNotes = rNotes blueBossa
    , rModulate = False
    , rPinned = True
    }



timeToDie :: IORef Bool
timeToDie = unsafePerformIO $ newIORef False

main :: IO ()
main = do
    !conn <- openConn
    MIDI.start conn
    void $ installHandler sigINT (Catch onInt) Nothing
    mainThread myKit [upHat] conn
    where
    onInt = do
        dietime <- readIORef timeToDie
        if dietime
            then raiseSignal sigTERM
            else writeIORef timeToDie True
