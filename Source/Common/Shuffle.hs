module Common.Shuffle (
    shuffle
) where

import System.Random

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
