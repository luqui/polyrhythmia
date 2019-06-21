{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Arrow (first)
import Control.Monad (filterM, void, when)
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Monad.Random as Rand
import qualified System.MIDI as MIDI
import Data.Monoid (Sum(..))

type Time = Rational

newtype Event a = Event { unEvent :: [(Time, a)] }
    deriving (Functor, Show)

mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith f = \(Event ts) (Event ts') -> Event (go ts ts')
    where
    go [] ts' = ts'
    go ts [] = ts
    go ((t,x):ts) ((t',x'):ts')
        = case compare t t' of
            LT -> (t,x) : go ts ((t',x'):ts')
            EQ -> (t, f x x') : go ts ts'
            GT -> (t',x') : go ((t,x):ts) ts'

instance Semigroup a => Semigroup (Event a) where
    (<>) = mergeWith (<>)

instance Semigroup a => Monoid (Event a) where
    mempty = Event []

eventSpaces :: Interval -> Event a -> [Interval]
eventSpaces i (Event []) = [i]
eventSpaces i (Event ((t,x):ts))
    | lower i > t = eventSpaces (Interval t (upper i)) (Event ((t,x):ts))
    | lower i == t = eventSpaces i (Event ts)
    | otherwise = Interval (lower i) t : eventSpaces (Interval t (upper i)) (Event ts)
     

data Interval = Interval { lower :: Rational, upper :: Rational }

intervalSize :: Interval -> Rational
intervalSize (Interval l u) = u - l

divideEvenly :: Int -> Interval -> [Rational]  -- does not include right endpoint
divideEvenly divs (Interval l u) = [ l + fromIntegral n * (u - l) / fromIntegral divs | n <- [0..divs-1] ]

type Cloud = Rand.Rand Rand.StdGen

type Rule a = Interval -> Cloud (Maybe (Event a))

rules :: Rule (Sum Int)
rules i = Rand.uniformMay $ concat
    [ [ Event [(t1, Sum 1), (t2, Sum 1)] | intervalSize i > 1/4, let [t1,t2] = divideEvenly 2 i ]
    , [ Event [(t1, Sum 1), (t3, Sum 1)] | intervalSize i > 1/4, let [t1,_,t3] = divideEvenly 3 i ]
    ]

iterateFS :: (Semigroup a) => Rule a -> Interval -> Cloud (Event a)
iterateFS rule i = do
    mevent <- rule i
    case mevent of
        Nothing -> pure mempty
        Just e -> do
            e's <- mapM (iterateFS rule) (eventSpaces i e)
            pure $ e <> mconcat e's

toNote :: Int -> Sum Int -> Note
toNote note (Sum _) = Note 1 note 72

shift :: Time -> Event a -> Event a
shift t (Event es) = Event (map (first (t+)) es)

data Note = Note Int Int Int -- ch note vel

playEvent :: MIDI.Connection -> Event Note -> IO ()
playEvent conn (Event e) = go e
    where
    go [] = pure ()
    go ((t,Note ch note vel):ts) = do
        waitUntil conn t
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
        void . forkIO $ do
            threadDelay 10000
            MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))
        go ts

waitUntil :: MIDI.Connection -> Rational -> IO ()
waitUntil conn t = do
    now <- MIDI.currentTime conn
    let delay = round (1000 * t - fromIntegral now)
    when (delay > 0) $ do
        threadDelay (1000 * delay)

openConn :: IO MIDI.Connection
openConn = do
    dest <- MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
    MIDI.start dest
    pure dest

repeatE :: (Semigroup a) => Rational -> Int -> Event a -> Event a
repeatE _ 0 _ = mempty
repeatE s n e = e <> repeatE s (n-1) (shift s e)

main :: IO ()
main = do
    conn <- openConn
    ride <- fmap (repeatE 2 4) $ Rand.evalRandIO (iterateFS rules (Interval 0 2)) 
    kick <- fmap (repeatE 2 4) $ Rand.evalRandIO (iterateFS rules (Interval 0 2)) 
    snare <- fmap (repeatE 2 4) $ Rand.evalRandIO (iterateFS rules (Interval 0 2)) 
    void . forkIO $ playEvent conn (fmap (toNote 48) ride)
    void . forkIO $ playEvent conn (fmap (toNote 36) kick)
    void . forkIO $ playEvent conn (fmap (toNote 38) snare)
