{-# OPTIONS_GHC -Wall #-}

module Scale (
    MIDINote, Scale, Range,
    apply,
    numNotes,
    transposeChr,
    cMinorPentatonic
) where

type MIDINote = Int

-- A scale has a root note in [0,12) (where 0 is C like in MIDI), and an
-- ascending set of degrees also in [0,12) which are offsets from the root.
data Scale = Scale { sRoot :: Int, sDegrees :: [Int] }

-- A range is a range of absolute MIDI notes, inclusive.
type Range = (MIDINote, MIDINote) 

numNotes :: Scale -> Int
numNotes = length . sDegrees

apply :: Scale -> Range -> Int -> MIDINote
apply scale (lo,hi) deg = octaveClamp (lo,hi) (root + (wrapDegrees (sDegrees scale) !! deg))
    where
    root = 
        -- head [ n | n <- [sRoot scale, sRoot scale + 12 ..], n >= lo ]
        -- head [ n + sRoot scale | n <- [0,12..], n + sRoot scale >= lo ]
        -- sRoot scale + head [ n | n <- [0,12..], n >= lo - sRoot scale ]
        sRoot scale + 12 * ((lo - sRoot scale) `ceilDiv` 12)

wrapDegrees :: [Int] -> [Int]
wrapDegrees degs = let r = degs ++ map (+12) r in r

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
