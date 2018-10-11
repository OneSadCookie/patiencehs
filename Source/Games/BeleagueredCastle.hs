{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Games.BeleagueredCastle (
    beleagueredCastle
) where

import Common.Necessities

data BeleagueredCastle = BeleagueredCastle PileMap deriving (Eq, Ord, Generic, NFData)

instance Show BeleagueredCastle where
    show (BeleagueredCastle m) = showPileMap m

foundationNames = foundations 4

tableauxNames = tableaux 8

names = foundationNames ++ tableauxNames

instance PilePatience BeleagueredCastle where
    pileNames = const names
    
    pileMap (BeleagueredCastle m) = m

instance MovePatience BeleagueredCastle where
    -- takeRule _ (Foundation _) = never
    takeRule _ (Tableau _) = singleCardInHand
    
    giveRule _ (Foundation _) =
        destinationIsRankUnderTopOfHand <&&>
        destinationIsSameSuitAsTopOfHand
    giveRule _ (Tableau _) =
        destinationIsEmpty <||> destinationIsRankOverTopOfHand
    
    applyMove (BeleagueredCastle m) v = BeleagueredCastle (applyMoveToMap m v)
    
    fromPiles = const tableauxNames
    
    toPiles = onlyOneEmptyTableau

instance Patience BeleagueredCastle where
    successors = defaultSuccessors
    
    won = wonIfFoundationCountIs 52

instance GridPatience BeleagueredCastle where
    grid _ =
        let tc i = Cell (Tableau i) FillsRight
            fc i = Cell (Foundation i) PilesUp
            x = CellIgnored
        in [[tc 0, x, x, fc 0, tc 4, x, x],
            [tc 1, x, x, fc 1, tc 5, x, x],
            [tc 2, x, x, fc 2, tc 6, x, x],
            [tc 3, x, x, fc 3, tc 7, x, x]]

instance UIPatience BeleagueredCastle where
    cardLocations = defaultCardLocations

layout (Foundation i) = [ place $ FaceUp $ Card Ace $ toEnum i ]
layout (Tableau    _) = [ deal FaceUp 6 ]

beleagueredCastle = BeleagueredCastle . defaultBegin layout removeAces names
