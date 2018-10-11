{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Games.PileOn (
    pileOn
) where

import qualified Data.Map as Map
import Data.Map ((!))

import Common.Necessities

data PileOn = PileOn PileMap deriving (Eq, Ord, Generic, NFData)

instance Show PileOn where
    show (PileOn m) = showPileMap m

names = tableaux 15

instance PilePatience PileOn where
    pileNames = const names
    
    pileMap (PileOn m) = m

pileOK [] = True
pileOK pile = pileIsAllOneRank pile && (length pile == 4)

turnDownWonPiles :: PileMap -> PileMap
turnDownWonPiles = Map.map turnDownWonPile where
    turnDownWonPile [] = []
    turnDownWonPile pile@(h:t)
        | pileOK pile = turnDown h : t
        | otherwise   = pile

instance MovePatience PileOn where
    takeRule _ _ = singleCardInHand <&&> handIsAllFaceUp
    giveRule _ _ = destinationIsEmpty <||>
        (destinationHasFewerThan 4 <&&> destinationIsSameRankAsTopOfHand)
    
    applyMove (PileOn m) v = PileOn (turnDownWonPiles $ applyMoveToMap m v)
        
    toPiles = onlyOneEmptyTableau
    
instance Patience PileOn where
    successors = defaultSuccessors
    
    won (PileOn m) = Map.foldr ((&&) . pileOK) True m

instance GridPatience PileOn where
    grid _ =
        let tc i = Cell (Tableau i) FillsRight
            x = CellIgnored
        in [[tc 0, x, tc 4, x, tc  8, x, tc 12, x],
            [tc 1, x, tc 5, x, tc  9, x, tc 13, x],
            [tc 2, x, tc 6, x, tc 10, x, tc 14, x],
            [tc 3, x, tc 7, x, tc 11, x, x,     x]]

instance UIPatience PileOn where
    cardLocations = defaultCardLocations

layout (Tableau i)
    | i < 13    = [ deal FaceUp 4 ]
    | otherwise = []

pileOn = PileOn . defaultBegin layout id names
