module Spider (
    Spider,
) where

import Control.Monad
import Random

import Card
import Patience
import Pile
import Utility

data Spider = Spider [Pile] [Pile] Pile

instance Patience Spider Move where
    allMoves = allMovesSp
    applyMove = applyMoveSp
    deal = dealSp
    won = wonSp

showSpider (Spider t f s) =
    (show (length s)) ++ " cards in stock\n" ++
    showNL f ++ "\n" ++
    showNL t

instance Show Spider where
    show = showSpider

data PileName = Tableau Int | Foundation Int | Stock deriving (Eq, Show)

pileNamed (Spider t _ _) (Tableau n) = t !! n
pileNamed (Spider _ f _) (Foundation n) = f !! n
pileNamed (Spider _ _ s) Stock = s

dealSp gen =
    let d = deck FaceDown
        decks = shuffle (d ++ d) gen
        (cards6, decks2) = splitAt 24 decks
        (cards5, stock) = splitAt 30 decks2
        t = (slice 6 cards6) ++ (slice 5 cards5)
        tableaux = map flipTop t
        foundations = take 8 (repeat [])
    in Spider tableaux foundations stock

allTableauNames = map Tableau [ 0 .. 7 ]
allFoundationNames = map Foundation [ 0 .. 7 ]
allStockNames = [ Stock ]

allPileNames = allTableauNames ++ allFoundationNames ++ allStockNames

data Move = Move Pile PileName PileName | Deal deriving Show

mapStrings f p = mapStrings' f [] p where
    mapStrings' f _ [] = []
    mapStrings' f s (c:p)
        | faceDown c = []
        | otherwise  =
            let s' = c:s
            in (f s') : (mapStrings' f s' p)

oneTableauPileMove getPile fromName toName string = do
    guard (suitAndRankConsistent string)
    (bottom:rest) <- [ getPile toName ]
    let top = head string
    guard (bottom `isOneRankHigherThan` top)
    return (Move string fromName toName)

allTableauMoves getPile fromName toName = 
    concat (mapStrings (oneTableauPileMove getPile fromName toName) (getPile fromName))

allMovesSp sp = Deal : do
    fromPileName <- allTableauNames
    toPileName <- allTableauNames ++ allFoundationNames
    allTableauMoves (pileNamed sp) fromPileName toPileName

alterPile getPile (Move string fromName toName) name
    | fromName == name = drop (length string) (getPile fromName)
    | toName   == name = reversePrepend string (getPile toName)
    | otherwise        = getPile name

applyMoveSp sp@(Spider tableaux foundations stock) Deal =
    let (deal, stock') = splitAt 8 stock
        tableaux' = zipWith (:) (map flipCard deal) tableaux
    in Spider tableaux' foundations stock'
applyMoveSp sp@(Spider tableaux foundations stock) move =
    let getPile = pileNamed sp
        alter = alterPile getPile move
    in Spider (map alter allTableauNames) foundations stock

wonSp sp@(Spider _ foundations _) =
    all (== 13) (map length foundations)
