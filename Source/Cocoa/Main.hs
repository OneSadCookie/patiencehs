{-# LANGUAGE ForeignFunctionInterface, ExistentialQuantification #-}

module Main (main) where

import Data.List
import Foreign
import Foreign.C
import System.Random

import Common.Card
import Common.Helpers
import Common.Patience
import Common.Pile
import Common.Search
import Common.UIPatience

import Games.BeleagueredCastle
import Games.PileOn

data AnyUIPatience = forall p. UIPatience p => AnyUIPatience p

instance Patience AnyUIPatience where
    won (AnyUIPatience x) = won x
    successors (AnyUIPatience x) = map AnyUIPatience (successors x)

instance UIPatience AnyUIPatience where
    cardLocations (AnyUIPatience p) = cardLocations p

data AppState = AppState [AnyUIPatience]

breakAfter p [] = []
breakAfter p (h:t)
    | p h       = [ h ]
    | otherwise = h : breakAfter p t

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

makeAppState start =
    let tree = dfs start successors
        states = map AnyUIPatience {- $ best betterGame -} $ breakAfter won tree
    in AppState states

eventHandler f oldPointer = do
    oldState <- deRefStablePtr oldPointer
    let newState = f oldState
    freeStablePtr oldPointer
    newPointer <- newStablePtr newState
    return newPointer

foreign export ccall stepState :: StablePtr AppState ->  IO (StablePtr AppState)
stepState = eventHandler stepState' where
    stepState' a@(AppState [e]) = a
    stepState' (AppState (h:t)) = AppState t

foreign import ccall "PatienceHS.h PlaceCard" placeCard ::
    Ptr () -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()

foreign export ccall placeCards ::
    StablePtr AppState -> Ptr () -> Double -> Double -> IO ()
placeCards oldPointer userData width height = do
    state <- deRefStablePtr oldPointer
    (AppState (p:_)) <- return state
    let locations = cardLocations p width height
    mapM (\(c, x, y) -> placeCard
        userData
        (fromIntegral $ fromEnum $ isFaceUp c)
        (fromIntegral $ fromEnum $ suit c)
        (fromIntegral $ fromEnum $ rank c)
        (realToFrac x)
        (realToFrac y)) locations
    return ()

foreign import ccall "PatienceHS.h PatienceStart" patienceStart ::
    StablePtr AppState -> IO ()

main = do
    gen <- newStdGen
    let state = makeAppState (pileOn gen)
    newPointer <- newStablePtr state
    patienceStart newPointer
