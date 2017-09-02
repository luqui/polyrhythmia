{-# LANGUAGE BangPatterns #-}

import qualified System.MIDI as MIDI
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forM_, when, join)
import Data.Word (Word32)
import Control.Concurrent.MVar
import Control.Monad.Random
import Data.Ratio hiding ((%))
import qualified Data.Ratio
import Data.List (delete)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Environment (getArgs)


openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 

type Time = Rational  -- seconds

data Note = Note 
    { nChannel :: Int  -- 1-4
    , nNote :: Int     -- 36-83
    , nVel :: Int      -- 0-127
    }
    deriving (Show)

data Media a 
    = Atom Rational a
    | Media a :<>: Media a
    | Media a :||: Media a
    deriving (Show)
infixr 2 :||:
infixr 3 :<>:

size :: Media a -> Rational
size (Atom s _) = s
size (m :<>: m') = size m + size m'
size (m :||: m') = size m `max` size m'

playMedia :: MIDI.Connection -> Rational -> Media (Maybe Note) -> IO Rational
playMedia conn t0 (Atom len Nothing) = do
    waitTill conn t0
    return (t0 + len)
playMedia conn t0 (Atom len (Just note)) = do
    waitTill conn t0
    MIDI.send conn $ MIDI.MidiMessage (nChannel note) (MIDI.NoteOn (nNote note) (nVel note))
    waitTill conn (t0 + len/2)
    MIDI.send conn $ MIDI.MidiMessage (nChannel note) (MIDI.NoteOn (nNote note) 0)
    return (t0 + len)
playMedia conn t0 (m1 :<>: m2) = do
    t1 <- playMedia conn t0 m1
    t2 <- playMedia conn t1 m2
    return t2
playMedia conn t0 (m1 :||: m2) = do
    t1v <- newEmptyMVar
    t2v <- newEmptyMVar
    forkIO_ $ putMVar t1v =<< playMedia conn t0 m1
    forkIO_ $ putMVar t2v =<< playMedia conn t0 m2
    max <$> takeMVar t1v <*> takeMVar t2v

playMediaNow :: MIDI.Connection -> Media (Maybe Note) -> IO ()
playMediaNow conn m = do
    now <- (/1000) . fromIntegral <$> MIDI.currentTime conn
    playMedia conn now m
    return ()
    


waitTill :: MIDI.Connection -> Time -> IO ()
waitTill conn target = do
    now <- (/1000) . fromIntegral <$> MIDI.currentTime conn
    when (target > now) $ do
        threadDelay (floor (1000000 * (target - now)))


type Cloud = Rand StdGen

type Range a = (a,a)
inRange :: (Ord a) => Range a -> a -> Bool
inRange (a,b) x = a <= x && x <= b

noteRange :: Range Rational
noteRange = (1/8, 1/4)

patternRange :: Range Rational
patternRange = (1/4,2)

makeInstrument :: Cloud (Int -> Note)
makeInstrument = Note <$> getRandomR (1,4) <*> getRandomR (36,83)

makeNote :: (Int -> Note) -> Cloud (Maybe Note)
makeNote instr = join $ uniform [
    return Nothing, 
    Just . instr <$> getRandomR (32,127)
  ]

makeRhythm :: Int -> Rational -> Cloud (Media (Maybe Note))
makeRhythm voicedepth s
    -- | voicedepth <= 0 = return (Atom s Nothing)
    | inRange noteRange s || voicedepth <= 0 = do
        instr <- makeInstrument
        note <- makeNote instr
        return $ Atom s note
    | inRange patternRange s = do
        instr <- makeInstrument
        let subdivs = filter (\d -> inRange noteRange (s/fromIntegral d)) $ [3,4,5,6,8]
        if not (null subdivs) then do
            subdiv <- uniform subdivs
            foldr1 (:<>:) <$> replicateM subdiv (Atom (s/fromIntegral subdiv) <$> makeNote instr)
        else
            return $ Atom s Nothing
    | s > snd patternRange = join . fromList $
        [ 20% do { times <- uniform [2,3,4,5]; a <- makeRhythm voicedepth (s/fromIntegral times); return (foldr1 (:<>:) (replicate times a)) }
        , 15% do { a <- makeRhythm voicedepth (s/2); b <- makeRhythm voicedepth (s/2); return (a :<>: b) }
        , 15% do { a <- makeRhythm voicedepth (s/4); b <- makeRhythm voicedepth (s/4); return (a :<>: b :<>: b :<>: a) }
        , 50% do { a <- makeRhythm (voicedepth-1) s; b <- makeRhythm (voicedepth-1) s; return (a :||: b) }
        --, do { a <- makeRhythm (voicedepth-1) (s/2); b <- makeRhythm (voicedepth-1) (s/2); return (a :<>: (a :||: b)) }

       -- , do { a <- makeRhythm (s/2); b <- makeRhythm (s/2); return (a :<>: (a :||: b)) }
       -- , do { a <- makeRhythm (s/3); b <- makeRhythm (s/3); c <- makeRhythm (s/3); return (a :<>: b :<>: (a :||: b)) }
       -- , do { a <- makeRhythm (s/3); b <- makeRhythm (s/3); return (a :<>: b :<>: a) }
       -- , do { a <- makeRhythm (s/4); b <- makeRhythm (s/4); c <- makeRhythm (s/4); return (a :<>: b :<>: a :<>: c) }
        ]
    | otherwise = return (Atom s Nothing)
        

(%) :: Rational -> a -> (a, Rational)
r % a = (a, r)
         


gcdRat :: Rational -> Rational -> Rational
gcdRat r r' = gcd (numerator r) (numerator r') Data.Ratio.% lcm (denominator r) (denominator r')

main = do
    args <- getArgs
    let time = case args of
                [] -> 60
                [s] -> read s
                [m,s] -> 60*read m + read s
    !conn <- openConn
    MIDI.start conn
    rhythm <- evalRandIO $ makeRhythm 4 (fromIntegral time)
    putStrLn "Playing"
    playMediaNow conn rhythm

forkIO_ a = forkIO a >> return ()
