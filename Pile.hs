module Pile (
    Pile (Pile, pileName, cards)
) where

import Card

data Pile name = Pile {
    pileName :: name,
    cards :: [ FacingCard ]
}
