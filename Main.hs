{-# LANGUAGE BangPatterns, TupleSections #-}

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

data Note = Note Int Int Int  -- ch note vel
    deriving (Eq, Ord)

data Rhythm = Rhythm {
    rTiming :: Rational,
    rNotes :: [Note],
    rLength :: Int,
    rRole :: String } 
    deriving (Eq, Ord)

type Kit = Map.Map String [Int]

type ChKit = Map.Map String [(Int,Int)] -- channel, note

rhythmThread :: MIDI.Connection -> Rhythm -> IO ()
rhythmThread conn rhythm = do
    now <- fromIntegral <$> MIDI.currentTime conn
    let starttime = rTiming rhythm * fromIntegral (floor (now / rTiming rhythm))
    let times = take (rLength rhythm) [starttime, starttime + rTiming rhythm ..]
    let velmod = replicate (rLength rhythm - 20) 1 ++ [1,1-1/20..1/20]
    forM_ (zip3 velmod times (cycle (rNotes rhythm))) $ \(vmod,t,Note ch note vel) -> do
        waitTill conn t
        let vel' = round (fromIntegral vel * vmod)
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel'))
        waitTill conn (t + rTiming rhythm / 2)
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))

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

mainThread :: ChKit -> MIDI.Connection -> IO ()
mainThread chkit conn = do
    stateVar <- newMVar $ State { sActive = Set.empty, sInactive = Set.empty }
    forkIO . forever $ do
        state <- readMVar stateVar
        clearScreen
        setCursorPosition 0 0
        let timebase = foldr gcdRat 0 (map rTiming (toList (sActive state)))
        print timebase
        mapM_ (putStrLn . renderRhythm timebase) (reverse (toList (sActive state)))
        hFlush stdout
        threadDelay 1000000
    forever $ do
        forkIO $ hand stateVar
        delay <- evalRandIO $ getRandomR (5*10^6,15*10^6)
        threadDelay delay
  where
  hand stateVar = do
    state <- readMVar stateVar
    r <- evalRandIO $ do
        pastr <- makeDerivedRhythm chkit (toList (sActive state))
        newr <- choosePastRhythm state
        uniform [pastr `mplus` newr, newr `mplus` pastr]
    maybe (return ()) (rhythmMain conn stateVar) r

type Cloud = Rand StdGen

makeRhythmRole :: ChKit -> String -> Rational -> Cloud Rhythm
makeRhythmRole chkit role timing = do
    numNotes <- uniform [3,3,4,4,4,4,5,6,6,6,6,8,8,9,10,12,15,16]
    notes <- replicateM numNotes $
                uncurry Note <$> uniform (chkit Map.! role) <*> (id =<< uniform [return 0, getRandomR (32,127)])
    len <- round . (/timing) . fromIntegral <$> getRandomR (30000,120000::Int)  -- timing = msec
    return $ Rhythm timing notes len role

makeRhythm :: ChKit -> Rational -> Cloud Rhythm
makeRhythm chkit timing = do
    role <- uniform (Map.keys chkit)
    makeRhythmRole chkit role timing

intervals :: [Rational]
intervals = [1/6, 1/4, 1/3, 1/2, 2/3, 1, 3/2, 2, 3, 4, 6]


choosePastRhythm :: State -> Cloud (Maybe Rhythm)
choosePastRhythm state = do
  let base = foldr gcdRat 0 . map rTiming . toList $ sActive state
  let possible = filter (\p -> (base == 0 || (rTiming p / base) `elem` intervals)
                            {- && rRole p `notElem` map rRole (toList (sActive state)) -})
                . toList $ sInactive state
  uniformMay possible


makeDerivedRhythm :: ChKit -> [Rhythm] -> Cloud (Maybe Rhythm)
makeDerivedRhythm chkit [] = Just <$> makeRhythm chkit (1000/4)
makeDerivedRhythm chkit rs = do
    role <- uniformMay (Map.keysSet chkit) -- uniformMay (Map.keysSet kit `Set.difference` Set.fromList (map rRole rs))
    let base = foldr1 gcdRat (map rTiming rs)
    let timings = (base *) <$> intervals
    let timings' = filter (\t -> 1000/8 < t && t < 1000) timings
    case role of
        Just r | not (null timings') -> fmap Just . makeRhythmRole chkit r =<< uniform timings'
        _ -> return Nothing

renderRhythm :: Rational -> Rhythm -> String
renderRhythm timebase rhythm = 
    rRole rhythm ++ " |" ++ concat [ renderNote n ++ replicate (spacing - 1) ' ' | n <- rNotes rhythm ] ++ "|"
    where
    spacing = floor (rTiming rhythm / timebase)
    renderNote (Note _ _ v) | v == 0    = "."
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
  where
  (-->) = (,)

makeChKit :: [(String, Int, Kit)] -> ChKit
makeChKit kits = Map.unions [ Map.mapKeysMonotonic ((name ++ ".") ++) . (fmap.map) (ch,) $ kit | (name, ch, kit) <- kits ]

myKit = makeChKit [("bells", 2, gamillionKit), ("kit", 1, repThatKit)]

main = do
    !conn <- openConn
    MIDI.start conn
    mainThread myKit conn

forkIO_ a = forkIO a >> return ()
