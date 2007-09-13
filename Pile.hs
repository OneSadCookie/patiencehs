module Pile (
    Pile (Pile, pileName, cards)
) where

import Card

data Pile name = Pile {
    pileName :: name,
    cards :: [ FacingCard ]
}

instance Show name => Show (Pile name) where
    show (Pile n c) = show n ++ ": " ++ show c
