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

import Card
import Deal
import Deck
import Helpers
import MovePatience
import Necessities
import Patience
import Pile
import Rule
import Search
import Shuffle

import BeleagueredCastle
import Spider

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
    let bc = (spiderette False) gen
        tree = dfs bc successors
    putStr $ show {- $ best betterGame -} $ takeWhile (not . won) tree

--main = do
--    gen <- newStdGen
--    let bc = begin BeleagueredCastle gen
--        ms = moves bc
--        after = map (applyMove bc) ms
--    putStr $ show bc
--    putStr $ show ms
--    putStr $ show after
