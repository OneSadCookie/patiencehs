module Spider (
    spider,
    spiderette
) where

import Control.Parallel.Strategies
import qualified Data.Map as Map
import Data.Map ((!))
import Necessities

data Spider = Spider | Spiderette deriving (Eq, Ord)

instance NFData Spider

instance Patience Spider where
    deckFilter Spider = doubleDeck
    deckFilter Spiderette = id
    
    pileNames Spider = (foundations 8 ++ tableaux 10 ++ [ Stock ])
    pileNames Spiderette = (foundations 4 ++ tableaux 8 ++ [ Stock ])
    
    layout _ (Foundation _) = []
    layout Spider (Tableau i)
        | i < 4     = [ deal FaceDown 5, deal FaceUp 1 ]
        | otherwise = [ deal FaceDown 4, deal FaceUp 1 ]
    layout Spider Stock = [ deal FaceDown 50 ]
    layout Spiderette (Tableau 0) = []
    layout Spiderette (Tableau i) = [ deal FaceDown (i - 1), deal FaceUp 1 ]
    layout Spiderette Stock = [ deal FaceDown 24 ]
    
    moves s p = (ruleMoves rules fromPiles toPiles s p) ++ [ dealFromStock p ] where
        rules _ (Foundation _) = Interact (
            Take never,
            Give (destinationIsEmpty <&&> handIsDescendingFullSuit))
        rules _ (Tableau _) = Interact (
            Take (handIsDescendingInSuit <&&> handIsAllFaceUp),
            Give (destinationIsEmpty <||> destinationIsRankOverTopOfHand))
        rules _ Stock = Interact (Take never, Give never)
        fromPiles piles = filter isTableau $ map fst piles
        toPiles = onlyOneEmptyTableau
        dealFromStock piles =
            let pileMap = Map.fromList piles
                stock = pileMap ! Stock
                tableauCount = length $ filter isTableau $ Map.keys pileMap
                (cards, stock') = splitAt tableauCount stock
                dealOneToTableau (h:t) (Tableau _) pile = (t, (turnUp h):pile)
                dealOneToTableau hand _ pile = (hand, pile)
                pileMap' = snd $ Map.mapAccumWithKey dealOneToTableau cards pileMap
                pileMap'' = Map.insert Stock stock' pileMap'
            in Map.assocs pileMap''
    
    won Spider     = wonIfFoundationCountIs 104
    won Spiderette = wonIfFoundationCountIs 52

spider = Spider
spiderette = Spiderette
