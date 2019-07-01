{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, TupleSections #-}

import Control.Applicative
import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Exception (throwTo)
import Control.Monad (filterM, forM, forM_, void, replicateM, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Logic as Logic
import qualified Control.Monad.Random as Rand
import Data.Function (on)
import Data.List (nubBy, nub)
import Data.IORef
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess))
import qualified System.MIDI as MIDI
import qualified System.Posix.Signals as Sig
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
         
data Production = Production (TrackName -> Bool) Int [Sym]

type Grammar = [Production]


type Parser = P.Parsec String ()

tok :: Parser a -> Parser a
tok p = p <* P.many (P.char ' ')

unique :: (Eq a) => [a] -> Bool
unique xs = nub xs == xs

parseProd :: Parser Production
parseProd = do
    filt <- parseFilter
    len <- parseNat
    void $ tok (P.string "=")
    syms <- P.many parseSym
    --when (len /= sum (map symLen syms)) $
    --    fail "Invalid production: lengths do not add up"
    when (not . unique . map fst $ nub [ (lab, symlen) | Sym lab symlen <- syms ]) $
        fail "Invalid production: symbol has multiple lengths"
    void P.newline
    pure $ Production filt len syms
    where
    parseFilter = P.option (const True) $ 
        tok (P.string "[") *> ((\ts x -> any (x==) ts) <$> P.many parseTrack) <* tok (P.string "]")

    parseSym = tok $ P.choice
        [ Sym <$> parseLabel <*> parseNat
        , Terminal <$> parseVel
        ]

    parseTrack :: Parser String
    parseTrack = tok $ P.many1 P.alphaNum

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

comment :: Parser ()
comment = void $ P.string "--" *> P.many (P.satisfy (/= '\n')) *> P.newline

parseNat :: Parser Int
parseNat = read <$> tok (P.many1 P.digit)

parseGrammar :: Parser (Int, Int, Grammar)
parseGrammar = do
    tempo <- tok (P.string "tempo ") *> tok parseNat <* P.newline
    phrase <- tok (P.string "phrase ") *> tok parseNat <* P.newline
    prods <- concat <$> P.many (([] <$ comment) <|> ((:[]) <$> parseProd))
    pure (tempo, phrase, prods)

type Cloud = Rand.Rand Rand.StdGen

shuffle :: [a] -> Cloud [a]
shuffle [] = pure []
shuffle xs = do
    n <- Rand.getRandomR (0, length xs - 1)
    ((xs !! n) :) <$> shuffle (take n xs <> drop (n+1) xs)

genRhythms :: TrackName -> Int -> Grammar -> Int -> Logic.LogicT Cloud (Phrase Int)
genRhythms _ 0 _ _ = empty
genRhythms track depth prods time = do
    candprods <- lift . shuffle $ [ p | p@(Production filt t _) <- prods, filt track, t == time ]
    Production _ _ syms <- foldr (<|>) empty $ map pure candprods
    let subtime = sum (map symLen syms)

    let subgens = nubBy ((==) `on` fst) [ (label,len) | Sym label len <- syms ]
    subpats <- fmap Map.fromList . forM subgens $ \(label,len) -> do
        (label,) <$> genRhythms track (depth-1) prods len

    let renderSym (Sym label _) = subpats Map.! label
        renderSym (Terminal s) = Phrase 1 [(0, s)]

    pure $ scale (fromIntegral time / fromIntegral subtime) $ foldMap renderSym syms

type TrackName = String

data Instrument = Instrument TrackName (Int -> Note)

instruments :: [Cloud Instrument]
instruments = --[ drumkit [36], drumkit [37], drumkit [38,39], drumkit [40,41], drumkit [42,43,44,45] ]
    [ drumkit "kick" [36], drumkit "snare" [37,38,39,40], drumkit "hat" [42,44,46], drumkit "tom" [41,43,45,47], drumkit "ride" [50, 53] ]
    where
    drumkit name notes = do
        chosen <- replicateM 5 (Rand.uniform notes)
        pure $ Instrument name $ \i -> Note 1 (cycle chosen !! i) (if i == 0 then 0 else min 127 (i * 15 + 30))


data Config = Config 
    { cfgTempo :: Int
    , cfgPhraseLen :: Int
    , cfgPhrases :: TrackName -> Logic.LogicT Cloud (Phrase Int)
    }

watchConfig :: FilePath -> IORef Config -> IO ()
watchConfig config ref = do
    contents <- readFile config
    case P.parse (parseGrammar <* P.eof) config contents of
        Left err -> print err
        Right (tempo, len, grammar) -> do
            let phrases t = genRhythms t 10 grammar len
            putStrLn "Reloaded"
            writeIORef ref (Config tempo len phrases)
    void $ Process.withCreateProcess 
             (Process.proc "/usr/local/bin/fswatch" ["fswatch", "-1", config]) $ \_ _ _ p -> 
                Process.waitForProcess p
    watchConfig config ref

main :: IO ()
main = do
    mainThread <- myThreadId
    void $ Sig.installHandler Sig.sigINT (Sig.Catch (throwTo mainThread ExitSuccess)) Nothing

    [config] <- getArgs
    phrasesRef <- newIORef (Config 90 16 (\_ -> pure mempty))

    void . forkIO $ watchConfig config phrasesRef


    conn <- openConn
    MIDI.start conn

    let go phrcount = do
            cfg <- readIORef phrasesRef
            let phraseScale = 60/(4 * fromIntegral (cfgTempo cfg))
            instrs <- Rand.evalRandIO (sequenceA instruments)
            now <- getCurrentTime conn
            forM_ instrs $ \(Instrument track instr) -> void . forkIO $ do
                phraseMay <- Rand.evalRandIO $ Logic.observeManyT 1 (cfgPhrases cfg track)
                forM_ phraseMay $ \phrase -> 
                    playPhrase conn (fmap instr (shift now (scale phraseScale phrase)))
            when (phrcount `mod` 4 == 0) . void . forkIO $ do
               MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn 55 100))
               threadDelay 100000
               MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn 55 0))
            waitUntil conn (now + phraseScale * fromIntegral (cfgPhraseLen cfg))
            go (phrcount+1)
    go (0 :: Int)


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
