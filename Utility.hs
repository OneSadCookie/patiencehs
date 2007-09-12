module Utility (
    slice,
    randomElement,
    shuffle,
    reversePrepend,
    showNL,
    enumFromBounded,
    enumFromThenBounded
) where

import Random

slice _ [] = []
slice n l =
    let (a, b) = splitAt n l
    in a : (slice n b)

randomElement list gen =
    let l = length list
        (n, g) = next gen
        i = n `mod` l
        (as, (b:bs)) = splitAt i list
    in (as ++ bs, b, g)

shuffle [] _ = []
shuffle list gen =
    let (l, e, g) = randomElement list gen
    in e : (shuffle l g)

reversePrepend [] l = l
reversePrepend (h:t) l = reversePrepend t (h:l)

showNL l = unlines (map show l)

enumFromBounded emin =
    map toEnum [ fromEnum emin .. fromEnum (asTypeOf maxBound emin)]
enumFromThenBounded emin ethen =
    map toEnum [fromEnum emin, fromEnum ethen .. fromEnum last]
    where last = if ethen < emin then (asTypeOf minBound emin) else maxBound
