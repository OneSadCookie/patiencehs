module Klondike (
    klondike
) where

import Necessities

data PileName = Stock | Waste | Tableau Int | Foundation Int

stockAction = (deal3To Waste)

wasteRules = (
    Take singleCardInHand,
    Give never)

foundationRules = (
    Take never,
    Give
        ((destinationIsEmpty <&&> topOfHandIsRank Ace) <||>
        (singleCardInHand <&&> destinationIsRankUnderTopOfHand)))

tableauRules = (
    Take (handIsAlternatingColors <&&> handIsDescendingRank),
    Give
        ((destinationIsEmpty <&&> topOfHandIsRank King) <||>
        (destinationIsDifferentColorFromTopOfHand <&&>
            destinationIsRankOverTopOfHand)))

klondike = Patience
    id
    [Act      Stock          stockAction     [(24, FaceDown)            ],
     Interact Waste          wasteRules      [                          ],
     Interact (Foundation 0) foundationRules [                          ],
     Interact (Foundation 1) foundationRules [                          ],
     Interact (Foundation 2) foundationRules [                          ],
     Interact (Foundation 3) foundationRules [                          ],
     Interact (Tableau    0) tableauRules    [               (1, FaceUp)],
     Interact (Tableau    1) tableauRules    [(1, FaceDown), (1, FaceUp)],
     Interact (Tableau    2) tableauRules    [(2, FaceDown), (1, FaceUp)],
     Interact (Tableau    3) tableauRules    [(3, FaceDown), (1, FaceUp)],
     Interact (Tableau    4) tableauRules    [(4, FaceDown), (1, FaceUp)],
     Interact (Tableau    5) tableauRules    [(5, FaceDown), (1, FaceUp)],
     Interact (Tableau    6) tableauRules    [(6, FaceDown), (1, FaceUp)]]
