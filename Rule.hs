module Rule (
    Rule,
    combineRules,
    (<&&>),
    (<||>),
    Give (Give),
    Take (Take),
    
    -- general rules
    never,
    always,
    
    -- hand rules
    singleCardInHand,
    handIsAlternatingColors,
    topOfHandIsRank,
    handIsDescendingRank,
    
    -- destination rules
    destinationIsEmpty,
    
    -- hand + destination rules
    destinationIsRankOverTopOfHand,
    destinationIsRankUnderTopOfHand,
    destinationIsDifferentColorFromTopOfHand,
    destinationIsSameSuitAsTopOfHand,
) where

import Card
import Pile

type Rule name = (Pile name) -> Hand -> Bool

combineRules f rule0 rule1 pile hand = f (rule0 pile hand) (rule1 pile hand)

(<&&>) = combineRules (&&)
(<||>) = combineRules (||)

data Give name = Give (Rule name)
data Take name = Take (Rule name)



combinedRule f (Pile _ (dc:_)) (hc:_) = f dc hc
combinedRule _ _ _                    = False



-- generic rules

never :: Rule n
never _ _ = False

always :: Rule n
always _ _ = True

-- hand rules

singleCardInHand :: Rule n
singleCardInHand _ [c] = True
singleCardInHand _ _ = False

handIsAlternatingColors :: Rule n
handIsAlternatingColors _ = isAlternatingColors

topOfHandIsRank :: Rank -> Rule n
topOfHandIsRank r _ (c:_) = ((rank c) == r)
topOfHandIsRank r _ _ = False

handIsDescendingRank :: Rule n
handIsDescendingRank _ = isDescendingRank

-- destination rules

destinationIsEmpty :: Rule n
destinationIsEmpty (Pile _ []) _ = True
destinationIsEmpty _ _ = False

-- hand + destination rules

destinationIsRankOverTopOfHand :: Rule n
destinationIsRankOverTopOfHand = combinedRule (\dc hc ->
    fromEnum (rank dc) == fromEnum (rank hc) + 1)

destinationIsRankUnderTopOfHand :: Rule n
destinationIsRankUnderTopOfHand = combinedRule (\dc hc ->
    fromEnum (rank dc) == fromEnum (rank hc) - 1)

destinationIsDifferentColorFromTopOfHand :: Rule n
destinationIsDifferentColorFromTopOfHand = combinedRule (\dc hc ->
    (color dc) /= (color hc))

destinationIsSameSuitAsTopOfHand :: Rule n
destinationIsSameSuitAsTopOfHand = combinedRule (\dc hc ->
    (suit dc) == (suit hc))
