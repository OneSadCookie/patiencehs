module BeleagueredCastle (
    beleagueredCastle
) where

import Necessities

data BeleagueredCastle = BeleagueredCastle PileMap deriving (Eq, Ord)

-- this isn't nearly correct, but there's no default NFData for PileMap
--     and for my usage, it shouldn't matter...
instance NFData BeleagueredCastle

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

layout (Foundation i) = [ place $ FaceUp $ Card Ace $ toEnum i ]
layout (Tableau    _) = [ deal FaceUp 6 ]

beleagueredCastle = BeleagueredCastle . defaultBegin layout removeAces names
