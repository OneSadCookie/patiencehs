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

breakAfter p l =
    let (l0, l1) = break p l
    in appendHead l0 l1 where
        appendHead xs [] = xs
        appendHead xs (y:ys) = xs ++ [y]

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
    --stepState a@(AppState [e]) = a
    stepState (AppState (h:t)) = AppState t

foreign import ccall "PatienceHS.h PlaceCard" placeCard ::
    Ptr () -> CInt -> CInt -> CInt -> CDouble -> CDouble -> IO ()

foreign export ccall placeCards ::
    Ptr () -> Ptr () -> Double -> Double -> IO ()
placeCards oldVoidPtr userData width height = do
    putStrLn (show oldVoidPtr)
    putStrLn "Placing cards"
    let oldPointer = castPtrToStablePtr oldVoidPtr
    putStrLn "got old stable"
    state <- deRefStablePtr oldPointer
    putStrLn "deref'd"
    (AppState (p:_)) <- return state
    putStrLn "going to place cards"
    let locations = cardLocations p width height
    mapM (\(c, x, y) -> placeCard
        userData
        (fromIntegral $ fromEnum $ isFaceUp c)
        (fromIntegral $ fromEnum $ suit c)
        (fromIntegral $ fromEnum $ rank c)
        (realToFrac x)
        (realToFrac y)) (take 3 locations)
    putStrLn "done"
    return ()

foreign import ccall "PatienceHS.h PatienceStart" patienceStart ::
    Ptr () -> IO ()

main = do
    gen <- newStdGen
    let state = makeAppState (beleagueredCastle gen)
    (AppState (p:_)) <- return state
    putStrLn "bwahahahaa"
    stable <- newStablePtr state
    let pointer = castStablePtrToPtr stable
    putStrLn (show pointer)
    patienceStart pointer
