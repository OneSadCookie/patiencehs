{-# OPTIONS -ffi -fglasgow-exts #-}

module Main (main) where

import Foreign
import CForeign
import Random

import Common.Patience
import Common.UIPatience

import Games.BeleagueredCastle

data AnyUIPatience = forall p. UIPatience p => AnyUIPatience p

instance Patience AnyUIPatience where
    won (AnyUIPatience x) = won x
    successors (AnyUIPatience x) = map AnyUIPatience (successors x)

instance UIPatience AnyUIPatience where
    cardLocations (AnyUIPatience p) = cardLocations p

bc :: StdGen -> AnyUIPatience
bc = AnyUIPatience . beleagueredCastle

main = do
    gen <- newStdGen
    let p = bc gen
    putStr $ concatMap show (cardLocations p 960.0 600.0)
