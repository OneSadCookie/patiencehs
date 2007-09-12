module BeleagueredCastle (
    BeleagueredCastle,
) where

import Control.Monad

import Card
import Patience
import Pile
import Utility

data BeleagueredCastle = BeleagueredCastle [ Pile ] [ Pile ]

instance Patience BeleagueredCastle PileName where
    applyMove = applyMoveBC
    allPileNames = allTableauNames ++ allFoundationNames
    pileNamed = pileNamedBC
    deal = dealBC
    won = wonBC

data PileName = Tableau Int | Foundation Suit deriving (Eq, Show)

showBC (BeleagueredCastle t f) = (showNL f) ++ "\n" ++ (showNL t)

instance Show BeleagueredCastle where
    show = showBC

pileNamedBC (BeleagueredCastle t _) (Tableau n) = t !! n
pileNamedBC (BeleagueredCastle _ f) (Foundation n) = f !! (fromEnum n)

filterAces = filter (\c -> ((rank c) /= ace))

foundationTakeRule = never
foundationGiveRule = sameSuit `rAnd` destinationIsRankUnderTop
foundationPile = Pile foundationTakeRule foundationGiveRule

tableauTakeRule = singleCard
tableauGiveRule = destinationIsEmpty `rOr` destinationIsRankOverTop
tableauPile = Pile tableauTakeRule tableauGiveRule

dealBC gen =
    let d = filterAces (deck FaceUp)
        s = shuffle d gen
        t = map tableauPile (slice 6 s)
        f = map foundationPile (map return (aces FaceUp))
    in BeleagueredCastle t f

allTableauNames = (map Tableau [ 0 .. 7 ])
allFoundationNames = (map Foundation suits)

alterPile getPile move name 
    | name == (fromPile move) = tail (getPile name)
    | name == (toPile move)   = (hand move) ++ (getPile name)
    | otherwise               = getPile name

applyMoveBC bc@(BeleagueredCastle t f) move =
    let getPile = (pileNamed bc)
        alter = (alterPile getPile move)
    in BeleagueredCastle
        (map alter allTableauNames)
        (map alter allFoundationNames)

wonBC (BeleagueredCastle _ f) = all (== king) (map (rank . head) f)
