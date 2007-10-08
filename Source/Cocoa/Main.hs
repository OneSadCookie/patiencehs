{-# OPTIONS -ffi -fglasgow-exts #-}

module Main (main) where

import Foreign
import CForeign
import Random

import Common.Card
import Common.Patience
import Common.Search
import Common.UIPatience

import Games.BeleagueredCastle

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

makeAppState start =
    let tree = dfs start successors
        states = map AnyUIPatience $ breakAfter won tree
    in AppState states

eventHandler f oldVoidPtr = do
    let oldPointer = castPtrToStablePtr oldVoidPtr
    oldState <- deRefStablePtr oldPointer
    let newState = f oldState
    freeStablePtr oldPointer
    pointer <- newStablePtr newState
    return (castStablePtrToPtr pointer)

foreign export ccall stepState :: Ptr () ->  IO (Ptr ())
stepState = eventHandler stepState where
    stepState a@(AppState [e]) = a
    stepState (AppState (h:t)) = AppState t

foreign import ccall "PatienceHS.h PlaceCard" placeCard ::
    Ptr () -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()

foreign export ccall placeCards ::
    Ptr () -> Ptr () -> Double -> Double -> IO ()
placeCards oldVoidPtr userData width height = do
    let oldPointer = castPtrToStablePtr oldVoidPtr
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
    Ptr () -> IO ()

main = do
    gen <- newStdGen
    let state = makeAppState (beleagueredCastle gen)
    stable <- newStablePtr state
    let pointer = castStablePtrToPtr stable
    patienceStart pointer
