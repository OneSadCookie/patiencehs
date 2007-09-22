module BeleagueredCastle (
    BeleagueredCastle (BeleagueredCastle)
) where

import Necessities

data BeleagueredCastle = BeleagueredCastle deriving (Eq, Ord)

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
        toPiles [] = []
        toPiles ((name, []):piles) = name : (map fst $ filter ((0 /=) . length . snd) piles)
        toPiles ((name, _):piles) = name : toPiles piles
    
    won _ piles = 52 == sum [
        length pile | (name, pile) <- piles, isFoundation name ]
