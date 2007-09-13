module Layout (
    PileType ({- Act, -} Interact)
) where

import Card
import Rule

data PileType n a =
    Interact n (Take n, Give n) [ (Int, Card -> FacingCard) ] -- |
    --Act      n a                [ (Int, Card -> FacingCard) ]
