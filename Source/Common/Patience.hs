module Common.Patience (
    PileMap,
    showPileMap,
    Patience (won, successors),
    PilePatience (pileNames, pileNamed, pileMap),
    piles,
    wonIfFoundationCountIs,
    onlyOneEmptyTableau,
) where

import Data.List
import qualified Data.Map as Map
import Data.Map ((!))

import Common.Pile
import Common.Rule

type PileMap = Map.Map PileName Pile

showPileMap m =
    let list = Map.assocs m
        list' = sort list
    in unlines $ map showPile list' where
        showPile (name, pile) = show name ++ ": " ++ show pile

class Patience p where
    won        :: p -> Bool
    successors :: p -> [ p ]

class PilePatience p where
    pileNames :: p -> [ PileName ]
    -- could define a default for this in terms of pileMap but it's
    -- probably a bad idea!
    
    pileNamed :: p -> PileName -> Pile
    pileNamed p n = (pileMap p) ! n

    pileMap :: p -> PileMap
    pileMap = undefined -- in case you don't want to work this way...

piles :: PilePatience p => p -> [ (PileName, Pile) ]
piles p = map (\n -> (n, pileNamed p n)) (pileNames p)

wonIfFoundationCountIs :: PilePatience p => Int -> p -> Bool
wonIfFoundationCountIs n p =
    n == sum [ length $ (pileNamed p) name |
        name <- filter isFoundation (pileNames p) ]

onlyOneEmptyTableau :: PilePatience p => p -> [ PileName ]
onlyOneEmptyTableau p = onlyOneEmptyTableau' (piles p) where
    onlyOneEmptyTableau' [] = []
    onlyOneEmptyTableau' ((name@(Tableau _), []):piles) =
        name : noEmptyTableaux piles
    onlyOneEmptyTableau' ((name, _):piles) = name : onlyOneEmptyTableau' piles
    
    noEmptyTableaux [] = []
    noEmptyTableaux ((Tableau _, _):piles) = noEmptyTableaux piles
    noEmptyTableaux ((name, _):piles) = name : noEmptyTableaux piles
