{-# LANGUAGE MultiWayIf, DeriveFunctor #-}

import Debug.Trace (trace)
import Control.Concurrent (threadDelay)
import Control.Monad (ap, filterM, replicateM, forM_, forM, forever)
import qualified Control.Monad.Random as Rand
import Control.Applicative
import Data.List (transpose)
import qualified System.MIDI as MIDI

newtype Production a = Production { runProduction :: Int -> [a] }
    deriving (Functor)

instance Applicative Production where
    pure = Production . pure . pure
    (<*>) = ap

instance Monad Production where
    Production m >>= f = Production $ \r -> m r >>= (`runProduction` r) . f

instance  Alternative Production where
    empty = Production (pure empty)
    Production p <|> Production q = Production (liftA2 (<|>) p q)

addP :: Int -> Int -> Production a -> Production b -> Production (a,b)
addP m n p q = Production $ \s ->
    if | (d,0) <- s `divMod` (m+n) -> (,) <$> runProduction p (m*d) <*> runProduction q (n*d)
       | otherwise -> []

unit :: a -> Production a
unit x = Production $ \s -> 
    if | s == 1 -> [x]
       | otherwise -> []

data RTree = RTree String [RTree]

instance Show RTree where
    show (RTree lab []) = lab
    show (RTree lab subs) = lab ++ "[" ++ concatMap show subs ++ "]"


node2 l (x,y) = RTree l [x,y]

example = fmap (node2 "a") (addP 3 4 example example)
      <|> fmap (node2 "b") (addP 1 1 example example)
      <|> fmap (node2 "c") (addP 2 1 example example)
      <|> unit (RTree "." [])


type Cloud = Rand.Rand Rand.StdGen

data Note = Note Int Int Int -- ch, note, vel


randomGrammar :: Int -> Int -> Cloud (Production [Int])  -- Int is an abstract label
randomGrammar nprods labels = do
    prods <- replicateM nprods randProd
    u <- Rand.uniform [0..labels-1]
    pure $ let r = choice (unit [u] : map ($ r) prods) in r
    
    where
    randProd = do
        m <- Rand.uniform [1..4]
        n <- Rand.uniform [1..4]
        lab <- Rand.uniform [0..labels-1]
        trace (show m ++ " + " ++ show n) (pure ())
        pure $ \rec -> fmap (prepend lab . uncurry (++)) (addP m n rec rec)

    choice = foldr (<|>) empty
    prepend x [] = []
    prepend x (_:xs) = x:xs



playSimple :: MIDI.Connection -> Double -> [[Note]] -> IO ()
playSimple conn timing = mapM_ $ \now -> do
    forM_ now $ \(Note ch note vel) -> do
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note vel))
    threadDelay (round (1000000 * timing))
    forM_ now $ \(Note ch note vel) -> do
        MIDI.send conn (MIDI.MidiMessage ch (MIDI.NoteOn note 0))

shuffle :: [a] -> Cloud [a]
shuffle [] = pure []
shuffle xs = do
    n <- Rand.uniform [0..length xs-1]
    ((xs !! n) :) <$> shuffle (take n xs ++ drop (n+1) xs)

hasAtLeast :: Int -> [a] -> Bool
hasAtLeast 0 _ = True
hasAtLeast _ [] = False
hasAtLeast n (_:xs) = hasAtLeast (n-1) xs

main :: IO ()
main = do
    grammar <- Rand.evalRandIO $ randomGrammar 5 (length vels * maximum (map length instrs))

    barsize <- Rand.evalRandIO $ head . filter (hasAtLeast 10 . runProduction grammar) <$> shuffle [16..48]
    putStrLn $ "bar size " ++ show barsize
    
    conn <- openConn
    MIDI.start conn

    msgs <- forM instrs $ \instr -> do
        bar <- Rand.evalRandIO $ Rand.uniform (take 10 (runProduction grammar barsize))
        let notebar = fmap (cycle (Note 1 <$> instr <*> vels) !!) bar
        pure notebar

    forever $ playSimple conn (1/6) (transpose msgs)
    where
    instrs = [ [36], [37,38,39,40], [42,44,46], [41,43,45,47], [50, 53] ]
    vels = [0,32,48,64]
    

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
