module Games.PileOn (
    pileOn
) where

import qualified Data.Map as Map
import Data.Map ((!))

import Common.Necessities

data PileOn = PileOn PileMap deriving (Eq, Ord)

instance NFData PileOn

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
    turnDownWonPile pile
        | pileOK pile = map turnDown pile
        | otherwise   = pile

instance MovePatience PileOn where
    takeRule _ _ = singleCardInHand <&&> handIsAllFaceUp
    giveRule _ _ = destinationIsEmpty <||>
        (destinationHasFewerThan 4 <&&> destinationIsSameRankAsTopOfHand)
    
    applyMove (PileOn m) v = PileOn (turnDownWonPiles $ applyMoveToMap m v)
        
    toPiles = onlyOneEmptyTableau
    
instance Patience PileOn where
    successors = defaultSuccessors
    
    won (PileOn m) = Map.fold ((&&) . pileOK) True m
    
layout (Tableau i)
    | i < 13    = [ deal FaceUp 4 ]
    | otherwise = []

pileOn = PileOn . defaultBegin layout id names
