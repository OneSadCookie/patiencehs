module Move (
    PileType ({- Act, -} Interact),
    ruleMoves,
) where

import qualified Data.Map as Map
import Data.Map ((!))

import Patience
import Pile
import Rule

data PileType =
    Interact (Take, Give) -- |
    -- Act .........

data Move = Move PileName PileName Hand

instance Show Move where
    show (Move f t h) =
        "move " ++ show h ++ " from " ++ show f ++ " to " ++ show t

movesFromTo rules piles fromName toName =
    let fromPile = piles ! fromName
        toPile = piles ! toName
        Interact (Take fromRule, _) = rules fromName
        Interact (_, Give toRule) = rules toName
        hs = hands fromPile
        legalFromHands = filter (fromRule fromPile) hs
        legalToHands = filter (toRule toPile) legalFromHands
    in map (Move fromName toName) legalToHands

allMoves rules piles = do
    let names = Map.keys piles
    fromName <- names
    toName <- names
    movesFromTo rules piles fromName toName

applyMove p piles (Move fromName toName hand) =
    let fromPile = piles ! fromName
        toPile = piles ! toName
        fromPile' = takeHand fromPile hand
        toPile' = giveHand toPile hand
        piles' = Map.insert fromName fromPile' piles
        piles'' = Map.insert toName toPile' piles'
    in piles''

ruleMoves :: Patience p => (p -> PileName -> PileType) -> p -> [ (PileName, Pile) ] -> [ [ (PileName, Pile) ] ]
ruleMoves rules p piles =
    let pileMap = Map.fromList piles
    in map Map.assocs $ map (applyMove p pileMap) (allMoves (rules p) pileMap)
