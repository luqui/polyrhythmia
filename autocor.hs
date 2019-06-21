import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, replicateM, void, forM_, forever)
import Data.Fixed (mod')
import Data.List (minimumBy, tails, sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.RTree as RT
import qualified Data.RTree.MBB as RT
import qualified System.MIDI as MIDI

type Connections = (MIDI.Connection, MIDI.Connection)

getConn :: IO Connections
getConn = do
    dest:_ <- filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations
    src:_ <- filterM (fmap (== "Scarlett 18i8 USB") . MIDI.getName) =<< MIDI.enumerateSources
    destconn <- MIDI.openDestination dest
    srcconn <- MIDI.openSource src Nothing
    MIDI.start srcconn
    pure (srcconn,destconn)

main :: IO ()
main = do
    conns <- getConn
    record conns

δ :: Double
δ = 0.05 -- 50 msec

velSensitivity :: Double
velSensitivity = 5

updateTree :: Double -> Double -> Double -> RT.RTree Double -> RT.RTree Double
updateTree l φ α tree 
    | null cands = RT.insert (RT.mbb (l - δ) (φ - δ) (l + δ) (φ + δ)) α tree
    | otherwise = RT.insert (RT.mbb (newl - δ) (newφ - δ) (newl + δ) (newφ + δ)) (candmag + α) (RT.delete candbb tree)
    where
    cands = filter (( < δ) . dist2) $ RT.lookupRangeWithKey (RT.mbb (l - 2*δ) (φ - 2*δ) (l + 2*δ) (φ + 2*δ)) tree
    (candbb, candmag) = minimumBy (comparing dist2) cands
    dist2 (bb,_) = max (abs (RT.getUlx bb + RT.getBrx bb)/2 - l) (abs (RT.getUly bb + RT.getBry bb)/2 - φ)
    norm = 1 / (candmag + α)
    newl = norm * (candmag * (RT.getUlx candbb + RT.getBrx candbb)/2 + α * l)
    newφ = norm * (candmag * (RT.getUly candbb + RT.getBry candbb)/2 + α * φ)

record :: Connections -> IO ()
record (src, dest) = do
    log <- replicateM 100 waitNote
    putStrLn "Calculating autocorrelation"
    let autocorr = foldr (.) id 
                       [ updateTree l (ts `mod'` l) (1/((velSensitivity*(vel-vel'))^2+1)) 
                       | ((ts,vel):es) <- tails log
                       , (ts',vel') <- es
                       , let l = abs (ts' - ts)
                       , tempolike l ]
                    RT.empty
    forM_ (take 3 . sortBy (comparing (Down . snd)) $ RT.toList autocorr) $ \(bb,mag) -> do
        let l = (RT.getUlx bb + RT.getBrx bb) / 2
        let φ = (RT.getUly bb + RT.getBry bb) / 2
        print (l, φ)
        void . forkIO $ playPeriod (src,dest) l φ
    forever $ threadDelay (10^6)
    where

    waitNote = do
        e <- getNextEvent (src,dest)
        case e of
            Just (MIDI.MidiEvent ts (MIDI.MidiMessage _ (MIDI.NoteOn _ vel))) -> pure (convts ts,fromIntegral vel/127)
            Just (MIDI.MidiEvent ts (MIDI.MidiMessage _ (MIDI.NoteOff _ vel))) -> pure (convts ts,0)
            Nothing -> threadDelay 1000 >> waitNote
            _ -> waitNote
    
    convts ts = fromIntegral ts * 0.001

    tempolike l = l > 0.5 && l < 4

playPeriod :: Connections -> Double -> Double -> IO ()
playPeriod (src, dest) l φ = do
    t <- (0.001*).fromIntegral <$> MIDI.currentTime src
    let waitt = l - ((t - φ) `mod'` l)
    threadDelay (round (10^6 * waitt))
    MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 42 75))
    threadDelay 50000
    playPeriod (src,dest) l φ

getNextEvent :: Connections -> IO (Maybe (MIDI.MidiEvent))
getNextEvent (src, dest) = do
    e <- MIDI.getNextEvent src
    {-
    case e of
        Just (MIDI.MidiEvent ts msg) -> MIDI.send dest msg
        _ -> pure ()
    -}
    pure e
