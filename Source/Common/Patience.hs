module Patience (
    Patience (deckFilter, pileNames, layout, moves, won),
    wonIfFoundationCountIs,
) where

import Deck
import Layout
import Pile
import Rule

class Patience p where
    deckFilter :: p -> Deck -> Deck
    pileNames  :: p -> [ PileName ]
    layout     :: p -> PileName -> [ DealRule ]
    moves      :: p -> [ (PileName, Pile) ] -> [ [ (PileName, Pile) ] ]
    won        :: p -> [ (PileName, Pile) ] -> Bool

wonIfFoundationCountIs n = ((n ==) . countFoundationCards)
