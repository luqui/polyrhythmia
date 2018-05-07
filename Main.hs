{-# LANGUAGE BangPatterns, TupleSections #-}

import qualified System.MIDI as MIDI
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_, when)
import Data.Word (Word32)
import Data.Foldable (toList)
import Data.Semigroup ((<>))
import Data.Maybe (maybeToList)
import Data.List (foldl1')
import Control.Concurrent.MVar
import Control.Monad.Random
import Data.Ratio
import Data.List (delete, insert)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)


openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 

type Time = Rational  -- millisecs

data State = State {
  sActive :: Set.Set Rhythm,
  sInactive :: Set.Set Rhythm
 }
 deriving (Show)

data Note = Note Int Int Int Rational  -- ch note vel dur  (dur in fraction of voice timing)
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

type Kit = Map.Map String [Int]

type ChKit = Map.Map String [(Int,Int)] -- channel, note

data Signal 
    = SigKill
    | SigMutate

forBreak :: [a] -> (a -> IO Bool) -> IO [a]
forBreak [] _ = return []
forBreak (x:xs) body = do
    r <- body x
    if r
        then forBreak xs body
        else return xs
    

rhythmThread :: MIDI.Connection -> Rhythm -> IO ()
rhythmThread conn rhythm = do
    let playNote vmod t (Note ch note vel dur) = do
            waitTill conn t
            let vel' = round (fromIntegral vel * vmod)
            MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel'))
            waitTill conn (t + dur * rTiming rhythm)
            MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))

    let playPhrase t0 = do
            let times = [t0, t0 + rTiming rhythm ..]
            let !ret = last (zipWith const times (rNotes rhythm ++ [error "too clever"]))
            forM_ (zip times (rNotes rhythm)) $ \(t, note) -> playNote 1 t note
            return ret
    let fadePhrase t0 = do
            let times = [t0, t0 + rTiming rhythm ..]
            let velmod = [1,1-1/40,1/40]
            forM_ (zip3 velmod times (cycle (rNotes rhythm))) $ \(vmod, t, note) -> 
                playNote vmod t note
             
    let go t0 = do
                nextt0 <- playPhrase t0
                go nextt0
             
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = rTiming rhythm * fromIntegral (floor (now / rTiming rhythm))
    go starttime

waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- fromIntegral <$> MIDI.currentTime conn
    threadDelay (floor (1000 * (target - now)))

rhythmMain :: MIDI.Connection -> MVar State -> Rhythm -> IO ()
rhythmMain conn stateVar rhythm = do
    state <- takeMVar stateVar
    if rhythm `Set.member` sActive state then putMVar stateVar state else do
    putMVar stateVar $ state { sActive = Set.insert rhythm (sActive state) }
    rhythmThread conn rhythm
    modMVar stateVar $ \s -> s { sActive = Set.delete rhythm (sActive s)
                               , sInactive = Set.insert rhythm (sInactive s)
                             }

modMVar v = modifyMVar_ v . (return .)

renderState :: State -> IO ()
renderState state = do
    clearScreen
    setCursorPosition 0 0
    let timebase = findGrid (sActive state)
    let period = findPeriod (sActive state)
    putStrLn $ "grid:   " ++ show (round timebase) ++ "ms"
    putStrLn $ "period: " ++ show (round period) ++ "ms"

    let padding = maximum [ length (rRole r) | r <- toList (sActive state) ]
    mapM_ (putStrLn . renderRhythm timebase padding) (reverse (toList (sActive state)))

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


mainThread :: ChKit -> MIDI.Connection -> IO ()
mainThread chkit conn = do
    stateVar <- newMVar $ State { sActive = Set.empty, sInactive = Set.empty }
    forkIO . forever $ do
        renderState =<< readMVar stateVar
        threadDelay 1000000
    forever $ do
        forkIO $ hand stateVar
        state <- readMVar stateVar
        -- Delay approximately 3 seconds per active rhythm,
        -- but always rounding up to the nearest period
        let period = findPeriod (sActive state)
        let target = 3000 * fromIntegral (length (sActive state) + 1)
        let delay = period * fromIntegral (ceiling (target / period))
        threadDelay (round (1000 * delay))
  where
  hand stateVar = do
    state <- readMVar stateVar
    r <- evalRandIO $ do
        pastr <- makeDerivedRhythm chkit (toList (sActive state))
        newr <- choosePastRhythm state
        uniform [pastr `mplus` newr, newr `mplus` pastr]
    maybe (return ()) (rhythmMain conn stateVar) r

type Cloud = Rand StdGen

makeRhythmRole :: ChKit -> String -> Rational -> Int -> Cloud Rhythm
makeRhythmRole chkit role timing numNotes = do
    notes <- replicateM numNotes $
                uncurry Note <$> uniform (chkit Map.! role) 
                             <*> (id =<< uniform [return 0, getRandomR (32,127)]) 
                             <*> uniform [1/10, 1/2, 9/10, 1]
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

choosePastRhythm :: State -> Cloud (Maybe Rhythm)
choosePastRhythm state =
    uniformMay $ filter (sActive state `admits`) . toList $ sInactive state

minimumGrid, maximumPeriod, minimumNote, maximumNote :: Rational
minimumGrid = 1000/16  -- 16th of a second
maximumPeriod = 1000 * 10
minimumNote = 1000/8
maximumNote = 1000/2

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

(-->) = (,)

repThatKit :: Kit
repThatKit = Map.fromList [
  "kick " --> [48, 49, 60, 61, 72, 73],
  "snare" --> [50, 51, 52, 62, 63, 64, 74, 75, 76],
  "hat  " --> [42, 46, 54, 56, 58, 66, 68, 70, 78, 80, 82],
  --"ride " --> [59, 83],
  "perc " --> [43, 53, 55, 65, 67, 77, 79]
  ]

gamillionKit :: Kit
gamillionKit = Map.fromList [
  --"kick " --> [36, 37, 48, 49, 60, 61],
  --"snare" --> [38, 39, 40, 50, 51, 52, 62, 63, 64],
  --"hat  " --> [42, 44, 46, 54, 56, 58, 66, 68, 70],
  "bell1" --> [41, 43, 45, 47],
  "bell2" --> [53, 55, 57, 59],
  "bell3" --> [65, 67, 69, 71],
  "bell4" --> [72..83]
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

cMinorKit :: Kit
cMinorKit = Map.fromList [
  -- "bass" --> [31,34,36,39,41,43,46,48],
  "mid"  --> [51,53,55,58,60,63,65,67,70,72],
  "high" --> [72,75,77,79,82,84,87,89],
  "high-alt" --> [72,73,75,76,78,80,82,84,85,87,88]
  ]

cMinorBassKit :: Kit
cMinorBassKit = Map.fromList [
  "bass" --> [31,34,36,39,41,43,46,48]
  ]

makeChKit :: [(String, Int, Kit)] -> ChKit
makeChKit kits = Map.unions [ Map.mapKeysMonotonic ((name ++ ".") ++) . (fmap.map) (ch,) $ kit | (name, ch, kit) <- kits ]

myKit = makeChKit [("kit", 1, repThatKit), {-("elec", 3, sAndBKit),-} ("keys", 4, cMinorKit), ("bass", 5, cMinorBassKit)]

main = do
    !conn <- openConn
    MIDI.start conn
    mainThread myKit conn

forkIO_ a = forkIO a >> return ()
