module Main (
    main
) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
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

best :: (a -> a -> Bool) -> [a] -> [a]
best bpred (first:rest) = first : (best' first bpred rest) where
    best' _ _ [] = []
    best' state bpred (h:t)
        | state `bpred` h = h : (best' h bpred t)
        | otherwise       = best' state bpred t

countCardsUp ps = foldr count 0 ps where
    count (Foundation _, pile) n = n + length pile
    count (_           , _   ) n = n

betterGame (Game _ piles0) (Game _ piles1) =
    (countCardsUp piles0) < (countCardsUp piles1)

main = do
    gen <- newStdGen
    let bc = begin BeleagueredCastle gen
        tree = dfs bc nextPositions
    putStr $ show $ best betterGame $ takeWhile (not . wonGame) tree

--main = do
--    gen <- newStdGen
--    let bc = begin BeleagueredCastle gen
--        ms = moves bc
--        after = map (applyMove bc) ms
--    putStr $ show bc
--    putStr $ show ms
--    putStr $ show after
