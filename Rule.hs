module Rule (
    Rule,
    combineRules,
    (<&&>),
    (<||>),
    Give (Give),
    Take (Take),
    
    never,
    always,
    
    singleCardInHand,
    handIsAlternatingColors,
    topOfHandIsRank,
    handIsDescendingRank,
    
    destinationIsEmpty,
    
    destinationIsRankOverTopOfHand,
    destinationIsRankUnderTopOfHand,
    destinationIsDifferentColorFromTopOfHand
) where

import Card
import Hand
import Pile

type Rule name = (Pile name) -> Hand -> Bool

combineRules f rule0 rule1 pile hand = f (rule0 pile hand) (rule1 pile hand)

(<&&>) = combineRules (&&)
(<||>) = combineRules (||)

data Give name = Give (Rule name)
data Take name = Take (Rule name)



-- generic rules

never _ _ = False

always _ _ = True

-- hand rules

singleCardInHand _ [c] = True
singleCardInHand _ _ = False

handIsAlternatingColors _ = isAlternatingColors

topOfHandIsRank r _ (c:_) = ((rank c) == r)
topOfHandIsRank r _ _ = False

handIsDescendingRank _ = isDescendingRank

-- destination rules

destinationIsEmpty (Pile _ []) _ = True
destinationIsEmpty _ _ = False

-- combined rules

destinationIsRankOverTopOfHand (Pile _ (dc:_)) (hc:_) =
    fromEnum (rank dc) == fromEnum (rank hc) + 1
destinationIsRankOverTopOfHand _ _ = False

destinationIsRankUnderTopOfHand (Pile _ (dc:_)) (hc:_) =
    fromEnum (rank dc) == fromEnum (rank hc) - 1
destinationIsRankUnderTopOfHand _ _ = False

destinationIsDifferentColorFromTopOfHand (Pile _ (dc:_)) (hc:_) =
    (color dc) /= (color hc)
destinationIsDifferentColorFromTopOfHand _ _ = False
