module Main (
    main
) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Sequence
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
--import Klondike

best :: (a -> a -> Bool) -> [a] -> [a]
best bpred (first:rest) = first : (best' first bpred rest) where
    best' _ _ [] = []
    best' state bpred (h:t)
        | state `bpred` h = h : (best' h bpred t)
        | otherwise       = best' state bpred t

betterGame g0 g1 =
    (countCardsUp $ pileCount g0) < (countCardsUp $ pileCount g1)

main = do
    gen <- newStdGen
    let bc = begin beleagueredCastle gen
        tree = dfs bc (map (uncurry applyMove) . liftM2 map (,) moves)
    putStr $ show $ best betterGame tree
