module Main (
    main
) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import List
import Random

import Common.Card
import Common.Deal
import Common.Deck
import Common.Helpers
import Common.MovePatience
import Common.Necessities
import Common.Patience
import Common.Pile
import Common.Rule
import Common.Search
import Common.Shuffle

import Games.BeleagueredCastle
import Games.PileOn
import Games.Spider

best :: (a -> a -> Bool) -> [a] -> [a]
best bpred (first:rest) = first : (best' first bpred rest) where
    best' _ _ [] = []
    best' state bpred (h:t)
        | state `bpred` h = h : (best' h bpred t)
        | otherwise       = best' state bpred t

countCardsUp ps = foldl' count 0 ps where
    count n (Foundation _, pile) = n + length pile
    count n (_           , _   ) = n

betterGame :: PilePatience p => p -> p -> Bool
betterGame = (<) `on` (countCardsUp . piles)

main = do
    gen <- newStdGen
    let p = pileOn gen
        tree = dfs p successors
        (progress, final) = break won tree
    putStr $ show {- $ best betterGame -} $ progress
    if not (null final)
        then putStr $ show (head final)
        else return ()
