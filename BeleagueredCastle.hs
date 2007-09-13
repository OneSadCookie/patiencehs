module BeleagueredCastle (
    beleagueredCastle
) where

import Necessities

data PileName = Tableau Int | Foundation Suit

foundationRules = (
    Take never,
    Give (destinationIsRankUnderTopOfHand <&&>
        destinationIsSameSuitAsTopOfHand))

tableauRules = (
    Take singleCardInHand,
    Give (destinationIsEmpty <||> destinationIsRankOverTopOfHand))

beleagueredCastle = Patience
    (filter ((Ace /=) . rank))
    [Interact (Foundation Hearts  ) foundationRules [],
     Interact (Foundation Clubs   ) foundationRules [],
     Interact (Foundation Diamonds) foundationRules [],
     Interact (Foundation Spades  ) foundationRules [],
     Interact (Tableau 0)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 1)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 2)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 3)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 4)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 5)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 6)           tableauRules    [(6, FaceUp)],
     Interact (Tableau 7)           tableauRules    [(6, FaceUp)]]
