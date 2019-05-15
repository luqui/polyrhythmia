{-# LANGUAGE BangPatterns, TupleSections, RankNTypes, LambdaCase #-}

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (guard, join, void, forM, forM_, filterM, replicateM)
import qualified Control.Monad.Free as Free
import qualified Control.Monad.Random as Rand
import qualified Control.Monad.Trans.State as State
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.List (nub, nubBy, transpose)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import qualified System.MIDI as MIDI

subdivsOrd :: (Ord a) => Int -> Int -> Int -> [a] -> [[(Int,a)]]
subdivsOrd = go Map.empty
    where
    go _ 0 _ _ _ = [[]]
    go _ _ 0 _ _ = []
    go mp size maxchunks maxchunk syms = do
        (csz, label, syms') <- callbacks <|> newsym
        guard (csz <= size)
        ((csz, label) :) <$> go (Map.insert label (csz,label) mp) (size - csz) (maxchunks - 1) maxchunk syms'
        where
        callbacks = [ (csz,label,syms) | (csz,label) <- Map.elems mp ]
        newsym
            | [] <- syms     = []
            | (s:ss) <- syms = [ (sz, s, ss) | sz <- [1..min maxchunk size] ]

toPhrasing :: (Ord a) => [(Int, a)] -> Phrasing Int
toPhrasing chunks = Phrasing $ \render -> 
    let assemble rendered = foldMap ((rendered Map.!) . snd) chunks in
    assemble . Map.fromList <$> traverse (\(sz,label) -> (label,) <$> render sz) (nub chunks)


-- `subdivs size maxchunks syms` finds all the ways to break up a "bar" of
-- `size` beats into a maximum of `maxchunks`, using at most `syms` distinct
-- symbols.
subdivs :: Int -> Int -> Int -> Int -> [Phrasing Int]
subdivs size maxchunks maxchunk syms = 
    map toPhrasing $ subdivsOrd size maxchunks maxchunk [0..syms-1]

fromLabels :: (Ord l) => Map.Map l a -> [l] -> Phrasing a
fromLabels rend labs = Phrasing $ \render -> (\m -> foldMap (m Map.!) labs) <$> traverse render rend


newtype Phrasing a = Phrasing { runPhrasing :: forall f m. (Applicative f, Monoid m) => (a -> f m) -> f m }


instance Functor Phrasing where
    fmap f (Phrasing p) = Phrasing $ \r -> p (r . f)

instance Foldable Phrasing where
    foldMap f = runIdentity . flip runPhrasing (Identity . f)

instance Traversable Phrasing where
    sequenceA p = reshuf <$> rendered
        where
        desc = descPhrasing [0..] p  -- [(Label, f a)]
        effects = Map.fromList desc  -- Map Label (f a)
        rendered = sequenceA effects  -- f (Map Label a)
        reshuf r = fromLabels r (map fst desc)

instance (Show a) => Show (Phrasing a) where
    show = unwords . map (\(l,x) -> [l] ++ ": " ++ show x) . descPhrasing ['A'..]

descPhrasing :: [l] -> Phrasing a -> [(l,a)]
descPhrasing labels p = State.evalState (runPhrasing p allocate) labels
    where
    allocate n = do
        (s:ss) <- State.get
        State.put ss
        pure [(s,n)]


type PhraseTree = Free.Free Phrasing

type PDist = Rand.Rand Rand.StdGen

embellish1 :: [a] -> Int -> PDist (PhraseTree a)
embellish1 leaves = Free.unfoldM $ \case
        1 -> Left <$> Rand.uniform leaves
        n -> Right <$> Rand.uniform (subdivs n 4 7 2)

showPhraseTree :: (Show a) => PhraseTree a -> String
showPhraseTree (Free.Pure x) = show x
showPhraseTree (Free.Free p) = "[ " ++ unwords (map (\(l,x) -> [l] ++ ": " ++ showPhraseTree x) (descPhrasing ['A'..'Z'] p)) ++ "]"

flatten :: PhraseTree a -> [a]
flatten = Free.iter (join . toList) . fmap (:[])


example :: PhraseTree Int
example = Free.Free (aaba (Free.Free (aaba (Free.Pure 12))))
    where
    aaba sub = fromLabels (Map.fromList [('A', sub), ('B', sub)]) "AABA"

example2 :: PhraseTree Int
example2 = Free.Free (aaaa (Free.Free (aaba (Free.Pure 64))))
    where
    aaba sub = fromLabels (Map.fromList [('A', sub), ('B', sub)]) "AABA"
    aaaa sub = fromLabels (Map.fromList [('A', sub)] ) "AAAA"


data Note = Note Int Int Int -- ch, note, vel


playSimple :: MIDI.Connection -> Double -> [[Note]] -> IO ()
playSimple conn timing = mapM_ $ \now -> do
    forM_ now $ \(Note ch note vel) -> do
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
    threadDelay (round (1000000 * timing))
    forM_ now $ \(Note ch note vel) -> do
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))


main :: IO ()
main = do
    !conn <- openConn
    MIDI.start conn

    msgs <- forM instrs $ \instr ->
        let messages = Note 1 <$> instr <*> vels in
        fmap flatten . Rand.evalRandIO . fmap join . sequenceA $ embellish1 messages <$> example
    playSimple conn (1/6) (transpose msgs)
    where
    instrs = [ [36], [37,38,39,40], [42,44,46], [41,43,45,47], [50, 53] ]
    vels = [0,0,0,0,0,0,32,64,96]

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
