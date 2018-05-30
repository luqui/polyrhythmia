module APC40 where

import qualified System.MIDI as MIDI
import Control.Monad (filterM)

data Devs = Devs { dSource :: MIDI.Connection, dDest :: MIDI.Connection }

openDevs :: IO (Maybe Devs)
openDevs = do
    srcs <- filterM (fmap ("APC40 mkII" ==) . MIDI.getName) =<< MIDI.enumerateSources
    dests <- filterM (fmap ("APC40 mkII" ==) . MIDI.getName) =<< MIDI.enumerateDestinations

    case (srcs, dests) of
        (src:_, dest:_) -> do
            srcconn <- MIDI.openSource src Nothing
            destconn <- MIDI.openDestination dest

            MIDI.sendSysEx destconn [
                0x47, 0x7f, 0x29, 0x60, 0x00, 0x04,
                0x40 + 0x00,     -- Generic Mode
                0x00, 0x00, 0x00
              ]

            return . Just $ Devs srcconn destconn
        _ -> return Nothing

lightOn :: Int -> Int -> Int -> Devs -> IO ()
lightOn x y vel devs = MIDI.send (dDest devs) $ MIDI.MidiMessage 1 (MIDI.NoteOn note vel)
    where
    note = 8*(4-(y-1))+x-1


