{-# LANGUAGE BangPatterns #-}

import qualified System.MIDI as MIDI
import qualified Data.Set as Set
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


openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 

type Time = Rational  -- millisecs

data State = State {
  sActive :: Set.Set Rhythm,
  sInactive :: Set.Set Rhythm
 }

data Rhythm = Rhythm {
    rTiming :: Rational,
    rNotes :: [Int],
    rLength :: Int,
    rChannel :: Int,  -- 1-4
    rNote :: Int }  -- 36-83
    deriving (Eq, Ord)


rhythmThread :: MIDI.Connection -> Rhythm -> IO ()
rhythmThread conn rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = rTiming rhythm * fromIntegral (floor (now / rTiming rhythm))
    let times = take (rLength rhythm) [starttime, starttime + rTiming rhythm ..]
    forM_ (zip times (cycle (rNotes rhythm))) $ \(t,vel) -> do
        waitTill conn t
        MIDI.send conn (MIDI.MidiMessage (rChannel rhythm) (MIDI.NoteOn (rNote rhythm) vel))
        waitTill conn (t + rTiming rhythm / 2)
        MIDI.send conn (MIDI.MidiMessage (rChannel rhythm) (MIDI.NoteOn (rNote rhythm) 0))

waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- fromIntegral <$> MIDI.currentTime conn
    threadDelay (floor (1000 * (target - now)))

rhythmMain :: MIDI.Connection -> MVar State -> Rhythm -> IO ()
rhythmMain conn stateVar rhythm = forkIO_ $ do
    modMVar stateVar $ \s -> s { sActive = Set.insert rhythm (sActive s)} 
    rhythmThread conn rhythm
    modMVar stateVar $ \s -> s { sActive = Set.delete rhythm (sActive s)
                               , sInactive = Set.insert rhythm (sInactive s)
                             }

modMVar v = modifyMVar_ v . (return .)

mainThread :: MIDI.Connection -> IO ()
mainThread conn = do
    stateVar <- newMVar $ State { sActive = Set.empty, sInactive = Set.empty }
    forkIO_ . forever $ do
        state <- readMVar stateVar
        clearScreen
        setCursorPosition 0 0
        let timebase = foldr gcdRat 0 (map rTiming (toList (sActive state)))
        mapM_ (putStrLn . renderRhythm timebase) (reverse (toList (sActive state)))
        threadDelay 1000000
    forever $ do
        state <- readMVar stateVar
        r <- evalRandIO $ do
            pastr <- makeDerivedRhythm (toList (sActive state))
            newr <- choosePastRhythm state
            uniform [pastr `mplus` newr, newr `mplus` pastr]
        maybe (return ()) (rhythmMain conn stateVar) r
        waitTime <- evalRandIO $ getRandomR (1,30)
        threadDelay (waitTime*1000000)

type Cloud = Rand StdGen

makeRhythm :: Rational -> Cloud Rhythm
makeRhythm timing = do
    numNotes <- uniform [3,3,4,4,4,4,5,6,6,6,6,8,8,9,10,12,15,16]
    notes <- id =<< (sequenceA <$> replicateM numNotes (uniform [return 0, getRandomR (32,127)]))
    len <- (length notes *) . (2^) <$> getRandomR (2,7 :: Int)
    channel <- getRandomR (1, 4)
    note <- getRandomR (36, 83)
    return $ Rhythm timing notes len channel note

intervals :: [Rational]
intervals = [1/6, 1/4, 1/3, 1/2, 2/3, 3/4, 1, 4/3, 3/2, 2, 3, 4, 6]


choosePastRhythm :: State -> Cloud (Maybe Rhythm)
choosePastRhythm state = do
  let base = foldr gcdRat 0 . map rTiming . toList $ sActive state
  let possible = filter (\p -> base == 0 || (rTiming p / base) `elem` intervals) . toList $ sInactive state
  uniformMay possible


makeDerivedRhythm :: [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythm [] = Just <$> makeRhythm (1000/4)
makeDerivedRhythm rs = do
    let base = foldr1 gcdRat (map rTiming rs)
    let timings = (base *) <$> intervals
    let timings' = filter (\t -> 1000/8 < t && t < 1000) timings
    if null timings'
        then return Nothing
        else fmap Just . makeRhythm =<< uniform timings'

renderRhythm :: Rational -> Rhythm -> String
renderRhythm timebase rhythm = 
    "|" ++ concat [ renderVel v ++ replicate (spacing - 1) ' ' | v <- rNotes rhythm ] ++ "|"
    where
    spacing = floor (rTiming rhythm / timebase)
    renderVel v | v == 0    = "."
                | v < 75    = "x"
                | otherwise = "X"

gcdRat :: Rational -> Rational -> Rational
gcdRat r r' = gcd (numerator r) (numerator r') % lcm (denominator r) (denominator r')

main = do
    !conn <- openConn
    MIDI.start conn
    mainThread conn

forkIO_ a = forkIO a >> return ()
