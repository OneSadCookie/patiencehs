module BeleagueredCastle (
    beleagueredCastle
) where

import Control.Parallel.Strategies
import Necessities

data BeleagueredCastle = BeleagueredCastle deriving (Eq, Ord)

instance NFData BeleagueredCastle

instance Patience BeleagueredCastle where
    deckFilter = const removeAces
    
    pileNames  = const (foundations 4 ++ tableaux 8)
    
    layout _ (Foundation i) = [ place $ FaceUp $ Card Ace $ toEnum i ]
    layout _ (Tableau    _) = [ deal FaceUp 6 ]
    
    moves = ruleMoves rules fromPiles toPiles where
        rules _ (Foundation _) = Interact (
            Take never,
            Give (destinationIsRankUnderTopOfHand <&&>
                  destinationIsSameSuitAsTopOfHand))
        rules _ (Tableau _) = Interact (
            Take singleCardInHand,
            Give (destinationIsEmpty <||> destinationIsRankOverTopOfHand))
        fromPiles piles = filter isTableau $ map fst piles
        toPiles = onlyOneEmptyTableau
    
    won = const (wonIfFoundationCountIs 52)

beleagueredCastle = BeleagueredCastle
