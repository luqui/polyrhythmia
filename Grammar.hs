{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections, ViewPatterns #-}

import Debug.Trace (trace)
import Control.Concurrent (threadDelay)
import Control.Monad (ap, filterM, replicateM, forM_, forM, forever, void, join)
import qualified Control.Monad.Random as Rand
import Control.Applicative
import Data.List (transpose, inits)
import Data.List.Split (splitOn)
import qualified System.MIDI as MIDI
import System.Environment (getArgs)
import System.Console.ANSI (clearScreen, setCursorPosition)

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
    if -- | s == m + n -> (,) <$> runProduction p m <*> runProduction q n
       | (d,0) <- s `divMod` (m+n) -> (,) <$> runProduction p (m*d) <*> runProduction q (n*d)
       | otherwise -> []

unit :: a -> Production a
unit x = Production $ \s -> 
    if | s == 1 -> [x]
       | otherwise -> []

fillUnit :: Production [Int]
fillUnit = Production $ \s -> [replicate s 0]

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

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

randomProduction :: Cloud (Int,Int)
randomProduction = do
    m <- Rand.weighted [ (n, 1/fromIntegral n) | n <- [1..10] ]
    n <- Rand.weighted [ (n, 1/fromIntegral n) | n <- [1..10] ]
    pure (m,n)

production :: Int -> Int -> OpenGrammar
production m n = \rec -> fmap (preinc . uncurry (++)) (addP m n rec rec)
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

-- probability to rotate the accents of a line -- makes the rhythm a bit looser and more tapestry-like
rotationProbability :: Rational
rotationProbability = 0.4


-- probability to generate a new, not yet heard bar, as a function of the number of different
-- ones we've heard so far
noveltyProbability :: Int -> Rational
noveltyProbability n = 1 / fromIntegral n

genEnsemble :: Int -> Production [Int] -> Cloud ([([Int], [Note])])   -- returns the generating row and the rendered one.  (yuck, sorry)
genEnsemble barsize grammar = do
    forM instruments $ \instr -> do
        let poss = take limit (runProduction grammar barsize)
        bar <- Rand.uniform poss
        row <- rotate <$> join (Rand.weighted [ (pure 0, 1-rotationProbability), (Rand.uniform [0..barsize-1], rotationProbability) ]) <*> pure bar
        notes <- replicateM 12 (Rand.uniform instr)
        pure . (row,) $ fmap (\strength -> Note 1 (notes !! (min strength 11)) (min 127 (strength * 10))) row
        
showBar :: [Int] -> String
showBar = ("|"++) . (++"|") . map tochar
    where
    tochar 0 = ' '
    tochar 1 = '.'
    tochar 2 = ','
    tochar 3 = 'x'
    tochar 4 = 'X'
    tochar _ = '#'

cat :: [[a]] -> [[a]] -> [[a]]
cat = zipWith (++)

-- sometimes way too many possibilities are generated and things slow down. Cut it off after we 
-- have found this many phrasings.  (We take the first n of them -- so the order ends up mattering,
-- I think it interferes with the variety and we should come up with a way to choose more fairly)
limit :: Int
limit = 50000


-- Require at least this many different phrasings to ensure variety
minLimit :: Int
minLimit = 100

parseRule :: String -> (Int,Int)
parseRule str = (read a, read b)
    where
    [a,b] = splitOn "+" str

evolveGrammar :: Int -> [OpenGrammar] -> Cloud Grammar
evolveGrammar barsize rules = do
    let poss = length (take limit (runProduction (fixGrammar rules) barsize))
    trace (show poss ++ " | " ++ show (length rules)) (pure ())
    if | poss == limit -> do
        n <- Rand.uniform [0..length rules-1]
        evolveGrammar barsize (take n rules ++ drop (n+1) rules)
       | poss < 100 -> do
        newrule <- uncurry production <$> randomProduction
        evolveGrammar barsize (newrule : rules) 
       | otherwise -> pure (fixGrammar rules)

main :: IO ()
main = do
    (read -> tempo) : (read -> barsize) : (map parseRule -> rules) <- getArgs

    grammar <- Rand.evalRandIO $ evolveGrammar barsize (map (uncurry production) rules) 

    conn <- openConn
    MIDI.start conn

    let delay = 1/(4/60*fromIntegral tempo)
    let gotime hist = do
            (hist',ens) <- Rand.evalRandIO (join (Rand.weighted [ 
                        ((hist,) <$> Rand.uniform hist, 1 - noveltyProbability (length hist)),
                        ((\x -> (x:hist, x)) <$> genEnsemble barsize grammar, noveltyProbability (length hist))
                    ]))

            clearScreen
            setCursorPosition 0 0
            mapM_ (putStrLn . showBar . fst) ens

            void . replicateM 4 $ playSimple conn delay . transpose $ map snd ens
            gotime hist'
    gotime . (:[]) =<< Rand.evalRandIO (genEnsemble barsize grammar)

    

openConn :: IO MIDI.Connection
openConn = MIDI.openDestination =<< fmap head . filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations 
