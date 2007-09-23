module Deck (
    Deck,
    standardDeck,
    
    -- filters
    removeAces,
    doubleDeck,
) where

import Card

type Deck = [ Card ]

standardDeck = do
    rank <- ranks
    suit <- suits
    return (Card rank suit)

removeAces :: Deck -> Deck
removeAces = filter ((Ace /=) . rank)

doubleDeck :: Deck -> Deck
doubleDeck d = d ++ d
