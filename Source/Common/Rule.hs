module Rule (
    Rule,
    combineRules,
    (<&&>),
    (<||>),
    
    -- general rules
    never,
    always,
    
    -- hand rules
    singleCardInHand,
    handIsAlternatingColors,
    topOfHandIsRank,
    handIsDescendingRank,
    handIsDescendingInSuit,
    handIsDescendingFullSuit,
    handIsAllFaceUp,
    
    -- destination rules
    destinationIsEmpty,
    
    -- hand + destination rules
    destinationIsRankOverTopOfHand,
    destinationIsRankUnderTopOfHand,
    destinationIsDifferentColorFromTopOfHand,
    destinationIsSameSuitAsTopOfHand,
) where

import Control.Arrow
import Control.Monad

import Card
import Helpers
import Pile

type Rule = Pile -> Hand -> Bool

combineRules f rule0 rule1 pile hand = f (rule0 pile hand) (rule1 pile hand)

(<&&>) = combineRules (&&)
(<||>) = combineRules (||)

combinedRule f (dc:_) (hc:_) = f dc hc
combinedRule _ _ _           = False


-- helpers

allPairs f cards = and $ zipWith f cards (tail cards)

isAlternatingColors = allPairs $ (/=) `on` color

isDescendingRank = allPairs $ (\a b -> fromEnum a == fromEnum b + 1) `on` rank

isConsistentSuit = allPairs $ (==) `on` suit


-- generic rules

never :: Rule
never _ _ = False

always :: Rule
always _ _ = True

-- hand rules

singleCardInHand :: Rule
singleCardInHand _ [c] = True
singleCardInHand _ _ = False

handIsAlternatingColors :: Rule
handIsAlternatingColors _ = isAlternatingColors

topOfHandIsRank :: Rank -> Rule
topOfHandIsRank r _ (c:_) = ((rank c) == r)
topOfHandIsRank r _ _ = False

handIsDescendingRank :: Rule
handIsDescendingRank _ = isDescendingRank

handIsDescendingInSuit :: Rule
handIsDescendingInSuit _ = liftM2 (&&) isDescendingRank isConsistentSuit

handIsDescendingFullSuit :: Rule
handIsDescendingFullSuit _ [] = False
handIsDescendingFullSuit p h@(k:_) =
    ((rank k) == King) &&
    (length h == 13) &&
    (isDescendingRank h) &&
    (isConsistentSuit h)

handIsAllFaceUp _ = all isFaceUp

-- destination rules

destinationIsEmpty :: Rule
destinationIsEmpty [] _ = True
destinationIsEmpty _  _ = False

-- hand + destination rules

destinationIsRankOverTopOfHand :: Rule
destinationIsRankOverTopOfHand = combinedRule (\dc hc ->
    fromEnum (rank dc) == fromEnum (rank hc) + 1)

destinationIsRankUnderTopOfHand :: Rule
destinationIsRankUnderTopOfHand = combinedRule (\dc hc ->
    fromEnum (rank dc) == fromEnum (rank hc) - 1)

destinationIsDifferentColorFromTopOfHand :: Rule
destinationIsDifferentColorFromTopOfHand = combinedRule (\dc hc ->
    (color dc) /= (color hc))

destinationIsSameSuitAsTopOfHand :: Rule
destinationIsSameSuitAsTopOfHand = combinedRule (\dc hc ->
    (suit dc) == (suit hc))
