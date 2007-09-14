module Layout (
    deal,
    place,
    PileType ({- Act, -} Interact)
) where

import Card
import Deck
import Rule

type DealRule = Deck -> (Deck, [ FacingCard ])

deal :: (Card -> FacingCard) -> Int -> DealRule
deal facing i deck =
    let (cards, deck') = splitAt i deck
    in (deck', map facing cards)

place :: FacingCard -> DealRule
place card deck = (deck, [ card ])

data PileType n a =
    Interact n (Take n, Give n) [ DealRule ] -- |
    --Act      n a                [ DealRule ]
