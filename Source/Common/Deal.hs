module Deal (
    DealRule,
    Layout,
    DeckFilter,
    deal,
    place,
    defaultBegin,
) where

import Data.List
import qualified Data.Map as Map
import Random

import Card
import Deck
import Patience
import Pile
import Rule
import Shuffle

type DealRule = Deck -> (Deck, [ FacingCard ])
type Layout = PileName -> [ DealRule ]
type DeckFilter = Deck -> Deck

deal :: (Card -> FacingCard) -> Int -> DealRule
deal facing i deck =
    let (cards, deck') = splitAt i deck
    in (deck', map facing cards)

place :: FacingCard -> DealRule
place card deck = (deck, [ card ])

dealPart = flip ($)

dealPile layout deck name =
    let (deck', parts) = mapAccumL dealPart deck (layout name)
    in (deck', (name, concat $ reverse parts))

defaultBegin ::
    Layout -> DeckFilter -> [ PileName ] -> StdGen -> PileMap
defaultBegin layout deckFilter names gen = 
    let d = shuffle (deckFilter standardDeck) gen
        piles = snd $ mapAccumL (dealPile layout) d names
    in Map.fromList piles
