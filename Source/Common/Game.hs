module Game (
    Game (Game),
    begin,
    moves,
    applyMove,
    wonGame,
) where

import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Random

import Deck
import Patience
import Pile
import Rule
import Shuffle

data Game p = Game p (Map.Map PileName Pile) deriving (Eq, Ord)

instance Show (Game p) where
    show (Game _ m) = unlines $ map showPile $ Map.assocs m where
        showPile (name, cards) = show name ++ ": " ++ show cards

dealPart = flip ($)

dealPile p deck name =
    let l = layout p name
        (deck', parts) = mapAccumL dealPart deck l
    in (deck', (name, concat parts))

begin p gen = 
    let d = shuffle (deckFilter p standardDeck) gen
        piles = snd $ mapAccumL (dealPile p) d (pileNames p)
    in Game p (Map.fromList piles)

data Move = Move PileName PileName Hand

instance Show Move where
    show (Move f t h) =
        "move " ++ show h ++ " from " ++ show f ++ " to " ++ show t

movesFromTo (Game p piles) fromName toName =
    let fromPile = piles ! fromName
        toPile = piles ! toName
        Interact (Take fromRule, _) = rules p fromName
        Interact (_, Give toRule) = rules p toName
        hs = hands fromPile
        legalFromHands = filter (fromRule fromPile) hs
        legalToHands = filter (toRule toPile) legalFromHands
    in map (Move fromName toName) legalToHands

moves game@(Game p _) = do
    fromName <- pileNames p
    toName <- pileNames p
    movesFromTo game fromName toName

applyMove game@(Game p piles) (Move fromName toName hand) =
    let fromPile = piles ! fromName
        toPile = piles ! toName
        fromPile' = takeHand fromPile hand
        toPile' = giveHand toPile hand
        piles' = Map.insert fromName fromPile' piles
        piles'' = Map.insert toName toPile' piles'
    in Game p piles''

wonGame (Game p piles) = won p (Map.assocs piles)
