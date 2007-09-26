module MovePatience (
    Move (Move),
    MovePatience (
        takeRule,
        giveRule,
        applyMove,
        fromPiles,
        toPiles,
        pilePairs,
        legalHand,
        legalMoves,
        allMoves),
    defaultFromPiles,
    defaultToPiles,
    defaultPilePairs,
    defaultLegalHand,
    defaultLegalMoves,
    defaultAllMoves,
    defaultSuccessors,
    applyMoveToMap,
    turnUpBottomTableauCards,
) where

import qualified Data.Map as Map
import Data.Map ((!))

import Card
import Patience
import Pile
import Rule

data Move = Move PileName PileName Hand

instance Show Move where
    show (Move f t h) =
        "move " ++ show h ++ " from " ++ show f ++ " to " ++ show t

class PilePatience p => MovePatience p where
    takeRule :: p -> PileName -> Rule
    giveRule :: p -> PileName -> Rule
    
    applyMove :: p -> Move -> p
    
    fromPiles :: p -> [ PileName ]
    fromPiles = defaultFromPiles
    
    toPiles :: p -> [ PileName ]
    toPiles = defaultToPiles
    
    pilePairs :: p -> [ (PileName, PileName) ]
    pilePairs = defaultPilePairs
    
    legalHand :: p -> PileName -> PileName -> Hand -> Bool
    legalHand = defaultLegalHand
    
    legalMoves :: p -> (PileName, PileName) -> [ Move ]
    legalMoves = defaultLegalMoves
    
    allMoves :: p -> [ Move ]
    allMoves = defaultAllMoves

defaultFromPiles :: MovePatience p => p -> [ PileName ]
defaultFromPiles = pileNames

defaultToPiles :: MovePatience p => p -> [ PileName ]
defaultToPiles = pileNames

defaultPilePairs :: MovePatience p => p -> [ (PileName, PileName) ]
defaultPilePairs p = [ (f, t) | f <- fromPiles p, t <- toPiles p, f /= t ]

defaultLegalHand :: MovePatience p =>
    p -> PileName -> PileName -> Hand -> Bool
defaultLegalHand p fromName toName hand =
    let fromPile = pileNamed p fromName
        toPile   = pileNamed p toName
        fromRule = takeRule p fromName
        toRule   = giveRule p toName
    in (fromRule fromPile hand) && (toRule toPile hand)

defaultLegalMoves :: MovePatience p => p -> (PileName, PileName) -> [ Move ]
defaultLegalMoves p (fromName, toName) =
    let fromPile = pileNamed p fromName
        legalHands = filter (legalHand p fromName toName) (hands fromPile)
    in map (Move fromName toName) legalHands

defaultAllMoves :: MovePatience p => p -> [ Move ]
defaultAllMoves p = concatMap (legalMoves p) (pilePairs p)

defaultSuccessors :: MovePatience p => p -> [ p ]
defaultSuccessors p = map (applyMove p) (allMoves p)

applyMoveToMap :: PileMap -> Move -> PileMap
applyMoveToMap m (Move fromName toName hand) =
    let fromPile = m ! fromName
        toPile = m ! toName
        fromPile' = takeHand fromPile hand
        toPile' = giveHand toPile hand
    in Map.insert toName toPile' (Map.insert fromName fromPile' m)

turnUpBottomTableauCards :: PileMap -> PileMap
turnUpBottomTableauCards = Map.mapWithKey turnUpTableauCard where
    turnUpTableauCard _ [] = []
    turnUpTableauCard (Tableau _) (h:t) = turnUp h : t
    turnUpTableauCard _ p = p
