{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, TupleSections #-}

import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (filterM, forM, guard, forM_, void, replicateM, when)
import qualified Control.Monad.Random as Rand
import Data.Function (on)
import Data.List (nubBy)
import qualified Data.Map as Map
import qualified System.MIDI as MIDI

-- Rhythmic grammars:
--
-- A production maps a certain length to a sequence of smaller
-- lengths, possibly with repetition. E.g.
--
-- 4 = A(2) A(2)
--
-- This rule says that when we encounter a phrase of length 4,
-- we may rewrite it by generating a phrase of length 2 and
-- repeating it twice.
--
-- There are also "terminal" phrases, which are simply velocities
-- (strengths).
--
-- 3 = [2] [0] [1]   -- basic swing

type Time = Rational

data Phrase a = Phrase Time [(Time, a)]
    deriving (Functor, Show)

instance Semigroup (Phrase a) where
    Phrase t es <> Phrase t' es' = Phrase (t+t') (es <> map (first (+t)) es')

instance Monoid (Phrase a) where
    mempty = Phrase 0 []

shift :: Time -> Phrase a -> Phrase a
shift t (Phrase len evs) = Phrase len (map (first (+t)) evs)

scale :: Rational -> Phrase a -> Phrase a
scale r (Phrase len evs) = Phrase (len * r) (map (first (*r)) evs)

data Sym = Sym String Int
         | Terminal Int
    deriving (Show)
         
data Production = Production Int [Sym]
    deriving (Show)

type Grammar = [Production]

type Cloud = Rand.Rand Rand.StdGen

genRhythms :: Grammar -> Int -> [Phrase Int]
genRhythms prods time = do
    Production t syms <- prods
    guard (t == time)

    let subgens = nubBy ((==) `on` fst) [ (label,len) | Sym label len <- syms ]
    subpats <- fmap Map.fromList . forM subgens $ \(label,len) -> (label,) <$> genRhythms prods len
    
    let renderSym (Sym label _) = subpats Map.! label
        renderSym (Terminal s) = Phrase 1 [(0, s)]
    pure $ foldMap renderSym syms

grammar :: Grammar
grammar = 
    [ Production 16 [Sym "A" 8, Sym "A" 8]
    , Production 8 [Sym "A" 4, Sym "B" 4]
    , Production 8 [Sym "A" 3, Sym "A" 3, Sym "B" 2]
    , Production 4 [Sym "A" 2, Sym "A" 2]
    , Production 3 [Terminal 2, Terminal 0, Terminal 1]
    , Production 2 [Terminal 1, Terminal 0]
    ]


type Instrument = Int -> Note

instruments :: [Cloud Instrument]
instruments = [ drumkit [36], drumkit [37,38,39,40], drumkit [42,44,46], drumkit [41,43,45,47], drumkit [50, 53] ]
    where
    drumkit notes = do
        chosen <- replicateM 5 (Rand.uniform notes)
        pure $ \i -> Note 1 (cycle chosen !! i) (if i == 0 then 0 else min 127 (i * 15 + 30))


main :: IO ()
main = do
    let phraseLen = 16
    let tempo = 90
    let phraseScale = 60/(4*tempo)

    conn <- openConn
    MIDI.start conn
    let phrases = map (scale phraseScale) $ genRhythms grammar (round phraseLen)
    instrs <- Rand.evalRandIO (sequenceA instruments)

    let go = do
            now <- getCurrentTime conn
            forM_ instrs $ \instr -> void . forkIO $ do
                phrase <- Rand.evalRandIO (Rand.uniform phrases)
                playPhrase conn (fmap instr (shift now phrase))
            waitUntil conn (now + phraseScale * phraseLen)
            go
    go


data Note = Note Int Int Int -- ch note vel

playPhrase :: MIDI.Connection -> Phrase Note -> IO ()
playPhrase _ (Phrase _ []) = pure ()
playPhrase conn (Phrase len evs) = do
    let t0 = fst (head evs)
    forM_ (zip evs (map fst (tail evs) ++ [t0+len])) $ \((t,Note ch note vel),t') -> do
        waitUntil conn t
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
        waitUntil conn t'
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))

getCurrentTime :: MIDI.Connection -> IO Time
getCurrentTime conn = do
    now <- MIDI.currentTime conn
    pure $ fromIntegral now / 1000

waitUntil :: MIDI.Connection -> Time -> IO ()
waitUntil conn target = do
    now <- getCurrentTime conn
    let delay = target - now
    when (delay > 0) (threadDelay (round (1000000 * delay)))
    

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
