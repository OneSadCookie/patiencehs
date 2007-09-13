module Game (
    deal
) where

import qualified Data.Map as Map
import List
import Random

import Deck
import Layout
import Patience
import Pile
import Shuffle

data Game n = Game (Map.Map n (Pile n))

instance Show n => Show (Game n) where
    show (Game m) = unlines $ map show $ map snd $ Map.assocs m

run :: (state -> input -> (state, output)) -> state -> [input] -> [output]
run = ((snd .) .) . mapAccumL

dealPile :: Ord n => Deck -> (PileType n a) -> (Deck, (n, Pile n))
dealPile deck (Interact name rules positioning) = (deck, (name, Pile name []))

deal :: Ord n => (Patience n a) -> StdGen -> (Game n)
deal (Patience deckFilter layout) gen =
    let d = standardDeck
        fd = deckFilter d
        shuffled = shuffle fd gen
    in Game (Map.fromList (run dealPile shuffled layout))
