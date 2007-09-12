module Pile (
    Hand,
    TakeRule,
    GiveRule,
    Pile (Pile, takeRule, giveRule, cards),
    canTakeFrom,
    canGiveTo,
    topCard,
    hands,
    handsFrom,
    handsFromTo,
    
    always,
    never,
    allFaceUp,
    singleCard,
    sameSuit,
    rankDescending,
    destinationIsRankOverTop,
    destinationIsRankUnderTop,
    destinationIsEmpty,
    
    rComb,
    rAnd,
    rOr,
    
    flipTop,
    aces,
    fullSuit,
    deck,
    suitAndRankConsistent,
) where

import Card

type Hand = [ Card ]

type TakeRule = [ Card ] -> Hand -> Bool
type GiveRule = [ Card ] -> Hand -> Bool

data Pile = Pile {
    takeRule :: TakeRule,
    giveRule :: GiveRule,
    cards :: [ Card ]
}

canTakeFrom p = ((takeRule p) (cards p))
canGiveTo p = ((giveRule p) (cards p))

showPile (Pile _ _ l) = show l

instance Show Pile where
    show = showPile

topCard (Pile _ _ l) = head l

allHands p = allHands' [] p where
    allHands' _ [] = []
    allHands' h (c:cs) =
        let h' = c:h
        in h' : (allHands' h' cs)

hands (Pile tr _ p) = filter (tr p) (allHands p)
handsFrom from = filter (canTakeFrom from) (hands from)
handsFromTo from to = filter (canGiveTo to) (handsFrom from)

always _ _ = True

never _ _ = False

allFaceUp _ h = all faceUp h

singleCard _ [c] = True
singleCard _ _ = False

sameSuit _ [] = True
sameSuit _ (c:cs) = all (== (suit c)) (map suit cs)

rankDescending _ l = rankDescending' l where
    rankDescending' [] = True
    rankDescending' [c] = True
    rankDescending' (c0:c1:cs) =
        c0 `isOneRankHigherThan` c1 &&
        rankDescending' (c1:cs)

destinationIsRankOverTop (c1:_) (c0:_) = c1 `isOneRankHigherThan` c0
destinationIsRankOverTop _ _ = False

destinationIsRankUnderTop (c1:_) (c0:_) = c0 `isOneRankHigherThan` c1
destinationIsRankUnderTop _ _ = False

destinationIsEmpty [] _ = True
destinationIsEmpty _ _ = False

rComb f r0 r1 p h = f (r0 p h) (r1 p h)
rAnd = rComb (&&)
rOr = rComb (||)













flipTop [] = []
flipTop (c:p) = (flipCard c) : p

aces f = map (\s -> Card ace s f) suits

fullSuit f s = map (\r -> Card r s f) ranks

deck f = concat (map (fullSuit f) suits)

suitAndRankConsistent [] = True
suitAndRankConsistent [c] = True
suitAndRankConsistent (c0:c1:s) =
    c0 `isOneRankHigherThan` c1 &&
    c0 `isSameSuitAs` c1
