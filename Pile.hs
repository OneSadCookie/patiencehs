module Pile (
    Hand,
    Pile (Pile, pileName, cards),
    hands,
    takeHand,
    giveHand,
) where

import Data.List

import Card

type Hand = [ FacingCard ]

data Pile name = Pile {
    pileName :: name,
    cards :: [ FacingCard ]
} deriving (Eq, Ord)

instance Show name => Show (Pile name) where
    show (Pile n c) = show n ++ ": " ++ show c

hands (Pile _ c) = map reverse (inits c)

takeHand (Pile n c) h = Pile n (drop (length h) c)

giveHand (Pile n c) h = Pile n ((reverse h) ++ c)
