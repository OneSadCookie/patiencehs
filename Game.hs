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

dealPart deck (number, facing) =
    let (cards, deck') = splitAt number deck
        part = map facing cards
    in (deck', part)

dealPile :: Ord n => Deck -> (PileType n a) -> (Deck, (n, Pile n))
dealPile deck (Interact name rules positioning) =
    let (deck', parts) = mapAccumL dealPart deck positioning
        cards = concat parts
    in (deck', (name, Pile name cards))

deal :: Ord n => (Patience n a) -> StdGen -> (Game n)
deal (Patience deckFilter layout) gen =
    let d = standardDeck
        fd = deckFilter d
        shuffled = shuffle fd gen
    in Game (Map.fromList $ snd $ mapAccumL dealPile shuffled layout)
