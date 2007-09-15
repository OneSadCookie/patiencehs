module Game (
    begin,
    moves,
    applyMove,
    pileCount,
) where

import qualified Data.Map as Map
import List
import Random

import Deck
import Layout
import Patience
import Pile
import Rule
import Shuffle

data Game n = Game (Map.Map n (Pile n)) (Map.Map n (Take n, Give n))

instance Eq n => Eq (Game n) where
    (==) (Game pm0 _) (Game pm1 _) = pm0 == pm1

instance Show n => Show (Game n) where
    show (Game pm _) = unlines $ map show $ map snd $ Map.assocs pm

dealPart = flip ($)

dealPile deck (Interact name rules positioning) =
    let (deck', parts) = mapAccumL dealPart deck positioning
        cards = concat parts
    in (deck', (name, (rules, Pile name cards)))

begin :: Ord n => (Patience n a) -> StdGen -> (Game n)
begin (Patience deckFilter layout) gen =
    let d = standardDeck
        fd = deckFilter d
        shuffled = shuffle fd gen
        pileDescs = mapAccumL dealPile shuffled layout
        mapFromPileDescs sel = (Map.fromList $ map extract $ snd $ pileDescs)
            where extract t = (fst t, sel $ snd t)
    in Game (mapFromPileDescs snd) (mapFromPileDescs fst)

pileNames (Game pm _) = Map.keys pm

pileNamed (Game pm _) n = (Map.!) pm n

rulesForPileNamed (Game _ rm) n = (Map.!) rm n

piles game = map (pileNamed game) (pileNames game)

pileCount game = zip (pileNames game) (map length $ map cards $ piles game)

data Move n = Move n n Hand

instance Show n => Show (Move n) where
    show (Move f t h) =
        "move " ++ show h ++ " from " ++ show f ++ " to " ++ show t

movesFromTo game fromName toName =
    let fromPile = pileNamed game fromName
        toPile = pileNamed game toName
        (Take fromRule, _) = rulesForPileNamed game fromName
        (_, Give toRule) = rulesForPileNamed game toName
        hs = hands fromPile
        legalFromHands = filter (fromRule fromPile) hs
        legalToHands = filter (toRule toPile) legalFromHands
    in map (Move fromName toName) legalToHands

moves game = do
    fromName <- pileNames game
    toName <- pileNames game
    movesFromTo game fromName toName

applyMove game@(Game pm rm) (Move fromName toName hand) =
    let fromPile = pileNamed game fromName
        toPile = pileNamed game toName
        fromPile' = takeHand fromPile hand
        toPile' = giveHand toPile hand
        pm' = (Map.insert fromName fromPile' pm)
        pm'' = (Map.insert toName toPile' pm')
    in Game pm'' rm
