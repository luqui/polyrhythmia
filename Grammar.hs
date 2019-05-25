{-# LANGUAGE MultiWayIf, DeriveFunctor #-}

import Debug.Trace (trace)
import Control.Parallel (par)
import Control.Concurrent (threadDelay)
import Control.Monad (ap, filterM, replicateM, forM_, forM, forever, void)
import qualified Control.Monad.Random as Rand
import Control.Applicative
import Data.List (transpose, inits)
import qualified System.MIDI as MIDI
import System.Environment (getArgs)

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

type OpenGrammar = Production [Int] -> Production [Int]  -- Int is a strength
type Grammar = Production [Int]

randomProduction :: Cloud OpenGrammar
randomProduction = do
    m <- Rand.weighted [ (n, 1/fromIntegral n) | n <- [1..10] ]
    n <- Rand.weighted [ (n, 1/fromIntegral n) | n <- [1..10] ]
    trace (show m ++ " + " ++ show n) (pure ())
    pure $ \rec -> fmap (preinc . uncurry (++)) (addP m n rec rec)
    where 
    preinc [] = []
    preinc (n:xs) = (n+1):xs

fixGrammar :: [OpenGrammar] -> Grammar
fixGrammar prods = let r = choice (unit [0] : map ($ r) prods) in r
    where
    choice = foldr (<|>) empty


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

instruments :: [[Int]]
instruments = [ [36], [37,38,39,40], [42,44,46], [41,43,45,47], [50, 53] ]

genEnsemble :: Int -> Production [Int] -> Cloud [[Note]]
genEnsemble barsize grammar = do
    forM instruments $ \instr -> do
        let poss = take limit (runProduction grammar barsize)
        bar <- Rand.uniform poss
        trace ("possibilities " ++ show (length poss)) (pure ())
        trace (show bar) (pure ())
        notes <- replicateM 12 (Rand.uniform instr)
        let notebar = fmap (\strength -> Note 1 (notes !! (min strength 11)) (min 127 (strength * 10))) bar
        pure notebar

cat :: [[a]] -> [[a]] -> [[a]]
cat = zipWith (++)

limit :: Int
limit = 50000

main :: IO ()
main = do
    [tempo, barsize] <- map read <$> getArgs

    prods <- Rand.evalRandIO $ let ps = (:) <$> randomProduction <*> ps in ps

    let grammar = head [ fixGrammar ps
                       | ps <- inits prods
                       , let poss = length (take limit (runProduction (fixGrammar ps) barsize))
                       , trace (show poss) (poss > 100)
                       ]

    conn <- openConn
    MIDI.start conn

    let delay = 1/(4/60*fromIntegral tempo)
    let gotime ens = do
            ens' <- Rand.evalRandIO (genEnsemble barsize grammar)
            par ((sum . map length) ens') $ void . replicateM 4 $ playSimple conn delay . transpose $ ens
            gotime ens'
    gotime []

    

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
