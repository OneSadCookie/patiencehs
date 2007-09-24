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

--import Action
import Card
import Deck
import Game
import Layout
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

betterGame (Game _ piles0) (Game _ piles1) =
    (countCardsUp piles0) < (countCardsUp piles1)

main = do
    gen <- newStdGen
    let bc = begin spiderette gen
        tree = dfs bc nextPositions
    putStr $ show {- $ best betterGame -} $ takeWhile (not . wonGame) tree

--main = do
--    gen <- newStdGen
--    let bc = begin BeleagueredCastle gen
--        ms = moves bc
--        after = map (applyMove bc) ms
--    putStr $ show bc
--    putStr $ show ms
--    putStr $ show after
