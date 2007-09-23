module Card (
    Rank (Ace, Two, Three, Four, Five, Six, Seven,
          Eight, Nine, Ten, Jack, Queen, King),
    ranks,
    Suit (Hearts, Clubs, Diamonds, Spades),
    suits,
    AbstractCard (rank, suit),
    color,
    Card (Card),
    FacingCard (FaceUp, FaceDown, abstractCard),
    turnUp,
) where

import Control.Parallel.Strategies

data Rank = Ace | Two | Three | Four | Five | Six | Seven |
            Eight | Nine | Ten | Jack | Queen | King
    deriving (Bounded, Enum, Eq, Ord)

instance Show Rank where
    show Ace   = "A"
    show Ten   = "X"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show r     = show ((fromEnum r) + 1)

instance NFData Rank

ranks :: [ Rank ]
ranks = [ minBound .. maxBound ]

data Color = Red | Black deriving (Eq)

data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Bounded, Enum, Eq, Ord)

instance Show Suit where
    show Hearts   = "H"
    show Clubs    = "C"
    show Diamonds = "D"
    show Spades   = "S"

instance NFData Suit

colorOfSuit Hearts   = Red
colorOfSuit Clubs    = Black
colorOfSuit Diamonds = Red
colorOfSuit Spades   = Black

suits :: [ Suit ]
suits = [ minBound .. maxBound ]

class AbstractCard c where
    rank :: c -> Rank
    suit :: c -> Suit

color card = colorOfSuit (suit card)

data Card = Card Rank Suit deriving (Eq, Ord)

instance AbstractCard Card where
    rank (Card r _) = r
    suit (Card _ s) = s

instance Show Card where
    show (Card r s) = (show r) ++ (show s)

instance NFData Card where
    rnf (Card r s) = rnf r `seq` rnf s

data FacingCard =
    FaceUp   { abstractCard :: Card } |
    FaceDown { abstractCard :: Card }
    deriving (Eq, Ord)

instance AbstractCard FacingCard where
    rank = rank . abstractCard
    suit = suit . abstractCard

instance Show FacingCard where
    show (FaceUp   card) = "|" ++ show card ++ "|"
    show (FaceDown card) = "<" ++ show card ++ ">"

instance NFData FacingCard where
    rnf (FaceUp   card) = rnf card
    rnf (FaceDown card) = rnf card

turnUp (FaceDown card) = FaceUp card
turnUp c = c
