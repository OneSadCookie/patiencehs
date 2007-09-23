module Game (
    Game (Game),
    begin,
    nextPositions,
    wonGame,
) where

import Control.Parallel.Strategies
import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Random

import Deck
import Patience
import Pile
import Rule
import Shuffle

data Game p = Game p [ (PileName, Pile) ] deriving (Eq, Ord)

instance NFData p => NFData (Game p) where
    rnf (Game p piles) = rnf p `seq` rnf piles

instance Show (Game p) where
    show (Game _ ps) = unlines $ map showPile $ ps where
        showPile (name, cards) = show name ++ ": " ++ show cards

dealPart = flip ($)

dealPile p deck name =
    let l = layout p name
        (deck', parts) = mapAccumL dealPart deck l
    in (deck', (name, concat $ reverse parts))

begin p gen = 
    let d = shuffle (deckFilter p standardDeck) gen
        piles = snd $ mapAccumL (dealPile p) d (pileNames p)
    in Game p piles

nextPositions (Game p piles) = map (Game p) (moves p piles)

wonGame (Game p piles) = won p piles
