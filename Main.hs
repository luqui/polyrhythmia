{-# LANGUAGE BangPatterns #-}

import qualified System.MIDI as MIDI
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_)
import Data.Word (Word32)
import Control.Concurrent.MVar
import Control.Monad.Random
import Data.Ratio
import Data.List (delete)

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 

type Time = Rational  -- millisecs

data Rhythm = Rhythm {
    rTiming :: Rational,
    rNotes :: [Int],
    rLength :: Int,
    rNote :: Int }  -- 36-83
    deriving (Eq)


rhythmThread :: MIDI.Connection -> Rhythm -> IO ()
rhythmThread conn rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = rTiming rhythm * fromIntegral (floor (now / rTiming rhythm))
    let times = take (rLength rhythm) [starttime, starttime + rTiming rhythm ..]
    forM_ (zip times (cycle (rNotes rhythm))) $ \(t,vel) -> do
        waitTill conn t
        MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn (rNote rhythm) 0))
        MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn (rNote rhythm) vel))
    MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn (rNote rhythm) 0))

waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- fromIntegral <$> MIDI.currentTime conn
    threadDelay (floor (1000 * (target - now)))

rhythmMain :: MIDI.Connection -> MVar [Rhythm] -> Rhythm -> IO ()
rhythmMain conn state rhythm = forkIO_ $ do
    putStrLn $ "Starting rhythm: " ++ show (1000/4/rTiming rhythm)
    modMVar state (rhythm:)
    rhythmThread conn rhythm
    modMVar state (delete rhythm)
    putStrLn "Ending rhythm"

modMVar v = modifyMVar_ v . (return .)

mainThread :: MIDI.Connection -> IO ()
mainThread conn = do
    state <- newMVar []
    forever $ do
        rhythms <- length <$> readMVar state
        when (rhythms < 4) $ do
            rhythm <- evalRandIO . makeDerivedRhythm =<< readMVar state
            rhythmMain conn state rhythm
            threadDelay (15*1000000)

type Cloud = Rand StdGen

makeRhythm :: Rational -> Cloud Rhythm
makeRhythm timing = do
    numNotes <- uniform [4,5,6,8,10,12,15,16]
    notes <- id =<< (sequenceA <$> replicateM numNotes (uniform [return 0, getRandomR (32,127)]))
    len <- (length notes *) . (2^) <$> getRandomR (4,7 :: Int)
    note <- getRandomR (36, 83)
    return $ Rhythm timing notes len note

intervals :: [Rational]
intervals = [1/3, 1/2, 2/3, 1, 3/2, 2, 3]

makeDerivedRhythm :: [Rhythm] -> Cloud Rhythm
makeDerivedRhythm [] = makeRhythm (1000/4)
makeDerivedRhythm [r] = do
    let timings = (rTiming r *) <$> intervals 
    let timings' = filter (\t -> 1000/8 < t && t < 1000) timings
    makeRhythm =<< uniform timings'
makeDerivedRhythm rs = do
    let base = foldr1 gcdRat (map rTiming rs)
    let timings = (base *) <$> intervals
    let timings' = filter (\t -> 1000/8 < t && t < 1000) timings
    makeRhythm =<< uniform timings'

gcdRat :: Rational -> Rational -> Rational
gcdRat r r' = gcd (numerator r) (numerator r') % lcm (denominator r) (denominator r')

main = do
    !conn <- openConn
    MIDI.start conn
    mainThread conn

forkIO_ a = forkIO a >> return ()
