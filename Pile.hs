module Pile (
    Hand,
    Pile (Pile, pileName, cards),
    hands
) where

import Data.List

import Card

type Hand = [ FacingCard ]

data Pile name = Pile {
    pileName :: name,
    cards :: [ FacingCard ]
}

instance Show name => Show (Pile name) where
    show (Pile n c) = show n ++ ": " ++ show c

hands (Pile _ c) = inits c
