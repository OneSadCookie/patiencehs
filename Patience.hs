module Patience (
    DeckFilter,
    Patience (Patience),
) where

import Deck
import Layout

type DeckFilter = Deck -> Deck

data Patience n a = Patience DeckFilter [ PileType n a ]
