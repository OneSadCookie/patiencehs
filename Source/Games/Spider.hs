module Games.Spider (
    spider,
    spiderette,
) where

import qualified Data.Map as Map
import Data.Map ((!))

import Common.Necessities

data Spider =
    Spider { pilesS :: PileMap } |
    Spiderette { has8thTableau :: Bool, pilesS :: PileMap }
    deriving (Eq, Ord)

isSpiderette (Spiderette _ _) = True
isSpiderette _                = False

instance NFData Spider

instance Show Spider where
    show s = showPileMap (pilesS s)

spiderFoundationNames = foundations 8
spiderTableauxNames = tableaux 10
spiderNames = Stock : (spiderFoundationNames ++ spiderTableauxNames)

spideretteFoundationNames = foundations 4
spideretteTableauxNames e = tableaux (7 + fromEnum e)
spideretteNames e =
    Stock : (spideretteFoundationNames ++ spideretteTableauxNames e)

foundationNames (Spider _) = spiderFoundationNames
foundationNames (Spiderette _ _) = spideretteFoundationNames

tableauxNames (Spider _) = spiderTableauxNames
tableauxNames (Spiderette e _) = spideretteTableauxNames e

names s = Stock : (foundationNames s ++ tableauxNames s)

instance PilePatience Spider where
    pileNames = names
    
    pileMap s = pilesS s

instance MovePatience Spider where
    -- takeRule _ (Foundation _) = never
    takeRule _ (Tableau _) = handIsDescendingInSuit <&&> handIsAllFaceUp
    
    giveRule _ (Foundation _) =
        destinationIsEmpty <&&> handIsDescendingFullSuit
    giveRule _ (Tableau _) =
        destinationIsEmpty <||> destinationIsRankOverTopOfHand
    
    applyMove (Spider m) v =
        Spider (turnUpBottomTableauCards $ applyMoveToMap m v)
    applyMove (Spiderette e m) v =
        Spiderette e (turnUpBottomTableauCards $ applyMoveToMap m v)
    
    fromPiles = tableauxNames
    
    toPiles = filter (not . isStock) . onlyOneEmptyTableau

stockRow c pileMap =
    let stock = pileMap ! Stock
        tableauCount = length $ filter isTableau $ Map.keys pileMap
        (cards, stock') = splitAt tableauCount stock
        dealOneToTableau [] _ pile = ([], pile)
        dealOneToTableau (h:t) (Tableau _) pile = (t, (turnUp h):pile)
        dealOneToTableau hand _ pile = (hand, pile)
        pileMap' = snd $ Map.mapAccumWithKey dealOneToTableau cards pileMap
        pileMap'' = Map.insert Stock stock' pileMap'
    in c pileMap''

instance Patience Spider where
    successors s = defaultSuccessors s ++ [ dealFromStock s ] where
        dealFromStock (Spider m) = stockRow Spider m
        dealFromStock (Spiderette e m) = stockRow (Spiderette e) m
    
    won s@(Spider _) = wonIfFoundationCountIs 104 s
    won s@(Spiderette _ _) = wonIfFoundationCountIs 52 s

spiderLayout (Foundation i) = []
spiderLayout (Tableau i)
    | i < 4     = [ deal FaceDown 5, deal FaceUp 1 ]
    | otherwise = [ deal FaceDown 4, deal FaceUp 1 ]
spiderLayout Stock = [ deal FaceDown 50 ]

spideretteLayout (Foundation i) = []
spideretteLayout (Tableau i)
    | i == 7    = []
    | otherwise = [ deal FaceDown i, deal FaceUp 1 ]
spideretteLayout Stock = [ deal FaceDown 24 ]

spider = Spider . defaultBegin spiderLayout doubleDeck spiderNames
spiderette e =
    Spiderette e . defaultBegin spideretteLayout id (spideretteNames e)
