module APC40 
    ( Devs
    , openDevs
    , lightOn
    , coordToNote
    , noteToCoord
    , pollNotes
    , RGB
    , rgbToVel
    )
where

import qualified System.MIDI as MIDI
import Control.Monad (filterM)
import Control.Arrow (first)
import Data.Bits ((.&.), shift)
import Data.List (minimumBy)
import Data.Function (on)

data Devs = Devs { dSource :: MIDI.Connection, dDest :: MIDI.Connection }

openDevs :: IO (Maybe Devs)
openDevs = do
    srcs <- filterM (fmap ("APC40 mkII" ==) . MIDI.getName) =<< MIDI.enumerateSources
    dests <- filterM (fmap ("APC40 mkII" ==) . MIDI.getName) =<< MIDI.enumerateDestinations

    case (srcs, dests) of
        (src:_, dest:_) -> do
            srcconn <- MIDI.openSource src Nothing
            destconn <- MIDI.openDestination dest

            MIDI.start srcconn

            MIDI.sendSysEx destconn [
                0x47, 0x7f, 0x29, 0x60, 0x00, 0x04,
                0x40 + 0x02,     -- Ableton Live mode 2 (all changes must be sent over midi)
                0x00, 0x00, 0x00
              ]

            return . Just $ Devs srcconn destconn
        _ -> return Nothing

pollNotes :: Devs -> IO [(Int, Int)]
pollNotes devs = do
    events <- MIDI.getEvents (dSource devs)
    return [ coord | MIDI.MidiEvent _ (MIDI.MidiMessage _ (MIDI.NoteOn note v)) <- events
                   , v /= 0
                   , Just coord <- pure (noteToCoord note)
                   ]

lightOn :: (Int, Int) -> Int -> Devs -> IO ()
lightOn (x,y) vel devs = MIDI.send (dDest devs) $ MIDI.MidiMessage 1 (MIDI.NoteOn note vel)
    where
    note = coordToNote (x,y)

coordToNote :: (Int, Int) -> Int
coordToNote (x, y) = 8*(4-(y-1))+x-1

noteToCoord :: Int -> Maybe (Int, Int)
noteToCoord n
    | 0 <= n && n < 40 = Just (n `mod` 8 + 1, 4 - n `div` 8 + 1)
    | otherwise = Nothing

type RGB = (Double, Double, Double)

rgbToVel :: RGB -> Int
rgbToVel rgb = snd $ minimumBy (compare `on` (distance2 rgb . fst)) rgbTable
    where
    distance2 (r,g,b) (r',g',b') = (r-r')^2 + (g-g')^2 + (b-b')^2

rgbTable :: [((Double, Double, Double), Int)]
rgbTable = map (first toRGB)
    [ --(0x000000, 0)
      (0x1E1E1E, 1)
    , (0x7F7F7F, 2)
    , (0xFFFFFF, 3)
    , (0xFF4C4C, 4)
    , (0xFF0000, 5)
    , (0x590000, 6)
    , (0x190000, 7)
    , (0xFFBD6C, 8)
    , (0xFF5400, 9)
    , (0x591D00, 10)
    , (0x271B00, 11)
    , (0xFFFF4C, 12)
    , (0xFFFF00, 13)
    , (0x595900, 14)
    , (0x191900, 15)
    , (0x88FF4C, 16)
    , (0x54FF00, 17)
    , (0x1D5900, 18)
    , (0x142B00, 19)
    , (0x4CFF4C, 20)
    , (0x00FF00, 21)
    , (0x005900, 22)
    , (0x001900, 23)
    , (0x4CFF5E, 24)
    , (0x00FF19, 25)
    , (0x00590D, 26)
    , (0x001902, 27)
    , (0x4CFF88, 28)
    , (0x00FF55, 29)
    , (0x00591D, 30)
    , (0x001F12, 31)
    , (0x4CFFB7, 32)
    , (0x00FF99, 33)
    , (0x005935, 34)
    , (0x001912, 35)
    , (0x4CC3FF, 36)
    , (0x00A9FF, 37)
    , (0x004152, 38)
    , (0x001019, 39)
    , (0x4C88FF, 40)
    , (0x0055FF, 41)
    , (0x001D59, 42)
    , (0x000819, 43)
    , (0x4C4CFF, 44)
    , (0x0000FF, 45)
    , (0x000059, 46)
    , (0x000019, 47)
    , (0x874CFF, 48)
    , (0x5400FF, 49)
    , (0x190064, 50)
    , (0x0F0030, 51)
    , (0xFF4CFF, 52)
    , (0xFF00FF, 53)
    , (0x590059, 54)
    , (0x190019, 55)
    , (0xFF4C87, 56)
    , (0xFF0054, 57)
    , (0x59001D, 58)
    , (0x220013, 59)
    , (0xFF1500, 60)
    , (0x993500, 61)
    , (0x795100, 62)
    , (0x436400, 63)
    , (0x033900, 64)
    , (0x005735, 65)
    , (0x00547F, 66)
    , (0x0000FF, 67)
    , (0x00454F, 68)
    , (0x2500CC, 69)
    , (0x7F7F7F, 70)
    , (0x202020, 71)
    , (0xFF0000, 72)
    , (0xBDFF2D, 73)
    , (0xAFED06, 74)
    , (0x64FF09, 75)
    , (0x108B00, 76)
    , (0x00FF87, 77)
    , (0x00A9FF, 78)
    , (0x002AFF, 79)
    , (0x3F00FF, 80)
    , (0x7A00FF, 81)
    , (0xB21A7D, 82)
    , (0x402100, 83)
    , (0xFF4A00, 84)
    , (0x88E106, 85)
    , (0x72FF15, 86)
    , (0x00FF00, 87)
    , (0x3BFF26, 88)
    , (0x59FF71, 89)
    , (0x38FFCC, 90)
    , (0x5B8AFF, 91)
    , (0x3151C6, 92)
    , (0x877FE9, 93)
    , (0xD31DFF, 94)
    , (0xFF005D, 95)
    , (0xFF7F00, 96)
    , (0xB9B000, 97)
    , (0x90FF00, 98)
    , (0x835D07, 99)
    , (0x392b00, 100)
    , (0x144C10, 101)
    , (0x0D5038, 102)
    , (0x15152A, 103)
    , (0x16205A, 104)
    , (0x693C1C, 105)
    , (0xA8000A, 106)
    , (0xDE513D, 107)
    , (0xD86A1C, 108)
    , (0xFFE126, 109)
    , (0x9EE12F, 110)
    , (0x67B50F, 111)
    , (0x1E1E30, 112)
    , (0xDCFF6B, 113)
    , (0x80FFBD, 114)
    , (0x9A99FF, 115)
    , (0x8E66FF, 116)
    , (0x404040, 117)
    , (0x757575, 118)
    , (0xE0FFFF, 119)
    , (0xA00000, 120)
    , (0x350000, 121)
    , (0x1AD000, 122)
    , (0x074200, 123)
    , (0xB9B000, 124)
    , (0x3F3100, 125)
    , (0xB35F00, 126)
    , (0x4B1502, 127)
    ]
    where
    toRGB :: Int -> RGB
    toRGB z = ( fromIntegral ((z .&. 0xff0000) `shift` (-16)) / 0xff
              , fromIntegral ((z .&. 0x00ff00) `shift` (-8)) / 0xff
              , fromIntegral ((z .&. 0x0000ff) `shift` 0) / 0xff)
