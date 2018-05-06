{-# LANGUAGE BangPatterns #-}

import qualified System.MIDI as MIDI
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_, when)
import Data.Word (Word32)
import Data.Foldable (toList)
import Data.Semigroup ((<>))
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

data Note = Note Int Int
    deriving (Eq, Ord)

data Rhythm = Rhythm {
    rTiming :: Rational,
    rNotes :: [Note],
    rLength :: Int,
    rChannel :: Int,  -- 1-4
    rRole :: String } 
    deriving (Eq, Ord)

type Kit = Map.Map String [Int]


rhythmThread :: MIDI.Connection -> Rhythm -> IO ()
rhythmThread conn rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = rTiming rhythm * fromIntegral (floor (now / rTiming rhythm))
    let times = take (rLength rhythm) [starttime, starttime + rTiming rhythm ..]
    forM_ (zip times (cycle (rNotes rhythm))) $ \(t,Note note vel) -> do
        waitTill conn t
        MIDI.send conn (MIDI.MidiMessage (rChannel rhythm) (MIDI.NoteOn note vel))
        waitTill conn (t + rTiming rhythm / 2)
        MIDI.send conn (MIDI.MidiMessage (rChannel rhythm) (MIDI.NoteOn note 0))

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

mainThread :: Kit -> MIDI.Connection -> IO ()
mainThread kit conn = do
    stateVar <- newMVar $ State { sActive = Set.empty, sInactive = Set.empty }
    forkIO . forever $ do
        state <- readMVar stateVar
        clearScreen
        setCursorPosition 0 0
        let timebase = foldr gcdRat 0 (map rTiming (toList (sActive state)))
        mapM_ (putStrLn . renderRhythm timebase) (reverse (toList (sActive state)))
        hFlush stdout
        threadDelay 1000000
    forever $ do
        forkIO $ hand stateVar
        delay <- evalRandIO $ getRandomR (10*10^6,30*10^6)
        threadDelay delay
  where
  hand stateVar = do
    state <- readMVar stateVar
    r <- evalRandIO $ do
        pastr <- makeDerivedRhythm kit (toList (sActive state))
        newr <- choosePastRhythm state
        uniform [pastr `mplus` newr, newr `mplus` pastr]
    maybe (return ()) (rhythmMain conn stateVar) r

type Cloud = Rand StdGen

makeRhythmRole :: Kit -> String -> Rational -> Cloud Rhythm
makeRhythmRole kit role timing = do
    numNotes <- uniform [3,3,4,4,4,4,5,6,6,6,6,8,8,9,10,12,15,16]
    notes <- replicateM numNotes $
                Note <$> uniform (kit Map.! role) <*> (id =<< uniform [return 0, getRandomR (32,127)])
    len <- (2^) <$> getRandomR (6,10 :: Int)
    channel <- getRandomR (1, 4)
    return $ Rhythm timing notes len channel role

makeRhythm :: Kit -> Rational -> Cloud Rhythm
makeRhythm kit timing = do
    role <- uniform (Map.keys kit)
    makeRhythmRole kit role timing

intervals :: [Rational]
intervals = [1/6, 1/4, 1/3, 1/2, 2/3, 3/4, 1, 4/3, 3/2, 2, 3, 4, 6]


choosePastRhythm :: State -> Cloud (Maybe Rhythm)
choosePastRhythm state = do
  let base = foldr gcdRat 0 . map rTiming . toList $ sActive state
  let possible = filter (\p -> (base == 0 || (rTiming p / base) `elem` intervals)
                            {- && rRole p `notElem` map rRole (toList (sActive state)) -})
                . toList $ sInactive state
  uniformMay possible


makeDerivedRhythm :: Kit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythm kit [] = Just <$> makeRhythm kit (1000/4)
makeDerivedRhythm kit rs = do
    role <- uniformMay (Map.keysSet kit) -- uniformMay (Map.keysSet kit `Set.difference` Set.fromList (map rRole rs))
    let base = foldr1 gcdRat (map rTiming rs)
    let timings = [base] -- (base *) <$> intervals
    let timings' = filter (\t -> 1000/8 < t && t < 1000) timings
    case role of
        Just r | not (null timings') -> fmap Just . makeRhythmRole kit r =<< uniform timings'
        _ -> return Nothing

renderRhythm :: Rational -> Rhythm -> String
renderRhythm timebase rhythm = 
    rRole rhythm ++ " |" ++ concat [ renderNote n ++ replicate (spacing - 1) ' ' | n <- rNotes rhythm ] ++ "|"
    where
    spacing = floor (rTiming rhythm / timebase)
    renderNote (Note _ v) | v == 0    = "."
                          | v < 75    = "x"
                          | otherwise = "X"

gcdRat :: Rational -> Rational -> Rational
gcdRat r r' = gcd (numerator r) (numerator r') % lcm (denominator r) (denominator r')

repThatKit :: Kit
repThatKit = Map.fromList [
  "kick " --> [48, 49, 60, 61, 72, 73],
  "snare" --> [50, 51, 52, 62, 63, 64, 74, 75, 76],
  "hat  " --> [42, 46, 54, 56, 58, 66, 68, 70, 78, 80, 82],
  "ride " --> [59, 83],
  "perc " --> [43, 53, 55, 65, 67, 77, 79]
  ]
  where
  (-->) = (,)

main = do
    !conn <- openConn
    MIDI.start conn
    mainThread repThatKit conn

forkIO_ a = forkIO a >> return ()
