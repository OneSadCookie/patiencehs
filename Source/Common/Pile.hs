{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Common.Pile (
    PileName (Foundation, Stock, Tableau, Waste),
    isFoundation,
    isTableau,
    isStock,
    foundations,
    tableaux,
    
    Pile,
    
    Hand,
    hands,
    takeHand,
    giveHand,
    
    allPairs,
    pileIsAllOneRank,
) where

import GHC.Generics
import Control.Parallel.Strategies
import Data.List

import Common.Card
import Common.Helpers

-- these are used for all games, to avoid lots of type parameters everywhere
data PileName =
    Foundation Int |
    Stock |
    Tableau Int |
    Waste deriving (Eq, Ord, Show, Generic, NFData)

isFoundation (Foundation _) = True
isFoundation _              = False

isTableau (Tableau _) = True
isTableau _           = False

isStock Stock = True
isStock _     = False

foundations n = map Foundation [0..n-1]
tableaux    n = map Tableau    [0..n-1]



type Pile = [ FacingCard ]



type Hand = [ FacingCard ]

--hands = map reverse . inits
hands = reverse . tails . reverse -- this is like 10x faster

takeHand :: Pile -> Hand -> Pile
takeHand = flip (drop . length)

giveHand :: Pile -> Hand -> Pile
giveHand = flip ((++) . reverse)



allPairs f cards = and $ zipWith f cards (tail cards)

pileIsAllOneRank :: Pile -> Bool
pileIsAllOneRank = allPairs ((==) `on` rank)
