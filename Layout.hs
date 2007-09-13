module Layout (
    PileType (Act, Interact)
) where

import Card
import Rule

data PileType n a =
    Interact n (Give n, Take n) [ (Int, Card -> FacingCard) ] |
    Act      n a                [ (Int, Card -> FacingCard) ]
