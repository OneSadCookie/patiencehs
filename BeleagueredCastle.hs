module BeleagueredCastle (
    beleagueredCastle
) where

import Necessities

data PileName = Tableau Int | Foundation Suit deriving (Eq, Ord, Show)

foundationRules = (
    Take never,
    Give (destinationIsRankUnderTopOfHand <&&>
        destinationIsSameSuitAsTopOfHand))

tableauRules = (
    Take singleCardInHand,
    Give (destinationIsEmpty <||> destinationIsRankOverTopOfHand))

foundationLayout suit = Interact
    (Foundation suit)
    foundationRules
    [ place (FaceUp (Card Ace suit)) ]

tableauLayout i = Interact
    (Tableau i)
    tableauRules
    [ deal FaceUp 6 ]

beleagueredCastle = Patience
    (filter ((Ace /=) . rank))
    ((map foundationLayout suits) ++ (map tableauLayout [ 0..7 ]))
