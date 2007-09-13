module Deck (
    Deck,
    standardDeck,
) where

import Card

type Deck = [ Card ]

standardDeck = do
    rank <- ranks
    suit <- suits
    return (Card rank suit)
