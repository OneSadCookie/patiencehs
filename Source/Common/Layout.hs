module Layout (
    DealRule,
    deal,
    place,
) where

import Card
import Deck
import Pile
import Rule

type DealRule = Deck -> (Deck, [ FacingCard ])

deal :: (Card -> FacingCard) -> Int -> DealRule
deal facing i deck =
    let (cards, deck') = splitAt i deck
    in (deck', map facing cards)

place :: FacingCard -> DealRule
place card deck = (deck, [ card ])
