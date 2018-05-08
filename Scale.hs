{-# OPTIONS_GHC -Wall #-}

module Scale (
    MIDINote, Scale, Range,
    apply, applyShift,
    numNotes,
    transposeChr,
    cMinorPentatonic, cMajor, cMinor
) where

import Data.Ord (comparing)
import Data.List (minimumBy)
import Control.Arrow ((&&&))

type MIDINote = Int

-- A scale has a root note in [0,12) (where 0 is C like in MIDI), and an
-- ascending set of degrees also in [0,12) which are offsets from the root.
data Scale = Scale { sRoot :: Int, sDegrees :: [Int] }
    deriving (Eq, Ord, Show)

-- A range is a range of absolute MIDI notes, inclusive.
type Range = (MIDINote, MIDINote) 

numNotes :: Scale -> Int
numNotes = length . sDegrees

apply :: Scale -> Range -> Int -> MIDINote
apply scale (lo,hi) deg = octaveClamp (lo,hi) (root + indexDegrees (sDegrees scale) deg)
    where
    root = 
        -- head [ n | n <- [sRoot scale, sRoot scale + 12 ..], n >= lo ]
        -- head [ n + sRoot scale | n <- [0,12..], n + sRoot scale >= lo ]
        -- sRoot scale + head [ n | n <- [0,12..], n >= lo - sRoot scale ]
        sRoot scale + 12 * ((lo - sRoot scale) `ceilDiv` 12)

        -- (TODO maybe) This mechanism means that the notes below the lowest
        -- root in the range will never be used.

-- Like apply, but uses a "reference pitch" (0,12) to locate degree 0.  That is, 0
-- will be assigned to the degree in the scale nearest to the referunce pitch.
applyShift :: Scale -> Range -> Int -> Int -> MIDINote
applyShift scale (lo,hi) ref deg = apply scale (lo,hi) (deg + refdeg) + octShift
    where
    refdeg = fst $ minimumOn (toneDistance ref . snd)
        [ (i, sRoot scale + d) | (i,d) <- zip [0..] (sDegrees scale) ]
    octShift | apply scale (lo,hi) refdeg - 12 >= lo = -12
             | otherwise = 0

toneDistance :: Int -> Int -> Int
toneDistance x y = min d1 (12 - d1)
    where
    d1 = abs (x - y) `mod` 12
 
minimumOn :: (Ord b) => (a -> b) -> [a] -> a
minimumOn measure = fst . minimumBy (comparing snd) . map (id &&& measure)

indexDegrees :: [Int] -> Int -> Int
indexDegrees degs n
    | n >= 0    = let r = degs ++ map (+12) r in r !! n
    | otherwise = let r = reverse degs ++ map (subtract 12) r in r !! (-n-1)

octaveClamp :: Range -> MIDINote -> MIDINote
octaveClamp (lo,hi) n
    | n < lo = octaveClamp (lo,hi) (n+12)
    | n > hi = octaveClamp (lo,hi) (n-12)
    | otherwise = n

ceilDiv :: Int -> Int -> Int
ceilDiv a b = -((-a) `div` b)

transposeChr :: Int -> Scale -> Scale
transposeChr offset (Scale root degrees) = Scale ((root + offset) `mod` 12) degrees

cMinorPentatonic :: Scale
cMinorPentatonic = Scale 0 [0,3,5,7,10]

cMajor, cMinor :: Scale
cMajor = Scale 0 [0,2,4,5,7,9,11]
cMinor = Scale 0 [0,2,3,5,7,8,10]


