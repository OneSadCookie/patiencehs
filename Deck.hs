module Deck (
    Deck,
    standardDeck,
    removeAces,
) where

import Card

type Deck = [ Card ]

standardDeck = do
    rank <- ranks
    suit <- suits
    return (Card rank suit)

removeAces :: Deck -> Deck
removeAces = filter ((Ace /=) . rank)
