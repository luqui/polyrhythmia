{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, TupleSections #-}

import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (filterM, forM, guard, forM_, void, replicateM, when)
import qualified Control.Monad.Random as Rand
import Data.Function (on)
import Data.List (nubBy, nub)
import Data.IORef
import qualified Data.Map as Map
import System.Environment (getArgs)
import qualified System.MIDI as MIDI
import qualified System.Process as Process
import qualified Text.Parsec as P

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

symLen :: Sym -> Int
symLen (Sym _ x) = x
symLen (Terminal _) = 1
         
data Production = Production Int [Sym]
    deriving (Show)

type Grammar = [Production]


type Parser = P.Parsec String ()

tok :: Parser a -> Parser a
tok p = p <* P.many (P.char ' ')

unique :: (Eq a) => [a] -> Bool
unique xs = nub xs == xs

parseProd :: Parser Production
parseProd = do
    len <- parseNat
    void $ tok (P.string "=")
    syms <- P.many parseSym
    when (len /= sum (map symLen syms)) $
        fail "Invalid production: lengths do not add up"
    when (not . unique . map fst $ nub [ (lab, symlen) | Sym lab symlen <- syms ]) $
        fail "Invalid production: symbol has multiple lengths"
    void P.newline
    pure $ Production len syms
    where
    parseSym = tok $ P.choice
        [ Sym <$> parseLabel <*> parseNat
        , Terminal <$> parseVel
        ]

    parseLabel :: Parser String
    parseLabel = (:[]) <$> P.letter

    parseVel :: Parser Int
    parseVel = P.choice
        [ 0 <$ P.char '.'
        , 1 <$ P.char '-'
        , 2 <$ P.char '+'
        , 3 <$ P.char '*'
        , 4 <$ P.char '#'
        ]

parseNat :: Parser Int
parseNat = read <$> tok (P.many1 P.digit)

parseGrammar :: Parser (Int, Grammar)
parseGrammar = do
    phrase <- tok (P.string "phrase ") *> tok parseNat <* P.newline
    prods <- P.many parseProd
    pure (phrase, prods)

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


type Instrument = Int -> Note

instruments :: [Cloud Instrument]
instruments = [ drumkit [36], drumkit [37,38,39,40], drumkit [42,44,46], drumkit [41,43,45,47], drumkit [50, 53] ]
    where
    drumkit notes = do
        chosen <- replicateM 5 (Rand.uniform notes)
        pure $ \i -> Note 1 (cycle chosen !! i) (if i == 0 then 0 else min 127 (i * 15 + 30))


watchConfig :: FilePath -> IORef (Int, [Phrase Int]) -> IO ()
watchConfig config ref = do
    contents <- readFile config
    case P.parse parseGrammar config contents of
        Left err -> print err
        Right (len, grammar) -> do
            let phrases = genRhythms grammar len
            putStrLn $ "possible phrases: " ++ show (length phrases)
            writeIORef ref (len, phrases)
    Process.callProcess "/usr/local/bin/fswatch" ["fswatch", "-1", config]
    watchConfig config ref

main :: IO ()
main = do
    [config] <- getArgs
    phrasesRef <- newIORef (16, [])

    void . forkIO $ watchConfig config phrasesRef

    let tempo = 90
    let phraseScale = 60/(4*tempo)

    conn <- openConn
    MIDI.start conn

    let go = do
            (phraseLen, phrases) <- readIORef phrasesRef
            instrs <- Rand.evalRandIO (sequenceA instruments)
            now <- getCurrentTime conn
            forM_ instrs $ \instr -> void . forkIO $ do
                phraseMay <- (fmap.fmap) (scale phraseScale) $ Rand.evalRandIO (Rand.uniformMay phrases)
                forM_ phraseMay $ \phrase -> 
                    playPhrase conn (fmap instr (shift now phrase))
            waitUntil conn (now + phraseScale * fromIntegral phraseLen)
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
