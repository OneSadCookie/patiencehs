--{- GHC_OPTIONS = -fglasgow-exts -}
-- hugs needs command-line arg -98

module Patience (
    Move (Move, fromPile, toPile, hand),
    Patience (applyMove, allPileNames, pileNamed, deal, won),
    allMoves,
    step
) where

import Control.Monad
import Control.Monad.Reader
import Random

import Pile

data Move n = Move {
    fromPile :: n,
    toPile :: n,
    hand :: Hand
}

class Patience p n | p -> n where
    applyMove :: p -> (Move n) -> p
    allPileNames :: p -> [ n ]
    pileNamed :: p -> n -> Pile
    deal :: StdGen -> p
    won :: p -> Bool

allMoves :: Patience p n => p -> [ (Move n) ]
allMoves p = do
    from <- (allPileNames p)
    to <- (allPileNames p)
    let hands = handsFromTo (pileNamed p from) (pileNamed p to)
    map (Move from to) hands

step :: Patience p n => p -> [p]
step = map (uncurry applyMove) . liftM2 map (,) allMoves
