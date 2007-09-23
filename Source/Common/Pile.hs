module Pile (
    Hand,
    PileName (Foundation, Stock, Tableau, Waste),
    isFoundation,
    isTableau,
    foundations,
    tableaux,
    Pile,
    hands,
    takeHand,
    giveHand,
) where

import Control.Parallel.Strategies
import Data.List

import Card

type Hand = [ FacingCard ]

-- these are used for all games, to avoid lots of type parameters everywhere
data PileName =
    Foundation Int |
    Stock |
    Tableau Int |
    Waste deriving (Eq, Ord, Show)

instance NFData PileName

isFoundation (Foundation _) = True
isFoundation _              = False

isTableau (Tableau _) = True
isTableau _           = False

foundations n = map Foundation [0..n-1]
tableaux    n = map Tableau    [0..n-1]

type Pile = [ FacingCard ]

--hands = map reverse . inits
hands = reverse . tails . reverse -- this is like 10x faster

takeHand = flip (drop . length)

giveHand = flip ((++) . reverse)
