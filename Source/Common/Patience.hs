module Patience (
    PileType ({- Act, -} Interact),
    Patience (deckFilter, pileNames, layout, rules, won),
) where

import Deck
import Layout
import Pile
import Rule

data PileType =
    Interact (Take, Give) -- |
    -- Act .........

class Patience p where
    deckFilter :: p -> Deck -> Deck
    pileNames  :: p -> [ PileName ]
    layout     :: p -> PileName -> [ DealRule ]
    rules      :: p -> PileName -> PileType
    won        :: p -> [ (PileName, Pile) ] -> Bool
