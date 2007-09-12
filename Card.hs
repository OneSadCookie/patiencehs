
module Card (
    Suit (Hearts, Clubs, Diamonds, Spades),
    suits,
    Rank (Rank),
    ranks,
    Face (FaceUp, FaceDown),
    ace,
    jack,
    queen,
    king,
    Card (Card),
    rank,
    suit,
    black,
    red,
    faceUp,
    faceDown,
    flipCard,
    isOneRankHigherThan,
    isSameSuitAs
) where

import Utility

data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Bounded, Enum, Eq, Ord, Show)

suits :: [ Suit ]
suits = [ minBound .. ]

newtype Rank = Rank Int deriving (Eq, Ord)

showRank (Rank 1) = "Ace"
showRank (Rank 11) = "Jack"
showRank (Rank 12) = "Queen"
showRank (Rank 13) = "King"
showRank (Rank n) = (show n)

instance Show Rank where
    show = showRank

instance Enum Rank where
    fromEnum (Rank n) = n
    toEnum = Rank
    enumFrom = enumFromBounded
    enumFromThen = enumFromThenBounded

instance Bounded Rank where
    minBound = ace
    maxBound = king

ranks :: [ Rank ]
ranks = [ minBound .. ]

data Face = FaceUp | FaceDown

ace = Rank 1
jack = Rank 11
queen = Rank 12
king = Rank 13

data Card = Card Rank Suit Face

showCard c@(Card _ _ FaceDown) = "(" ++ (show (flipCard c)) ++ ")"
showCard (Card r s FaceUp) = (show r) ++ " of " ++ (show s)

instance Show Card where
    show = showCard

rank (Card r _ _) = r
suit (Card _ s _) = s

black (Card _ s _) = (s == Clubs || s == Spades)
red c = not (black c)

faceUp (Card _ _ FaceUp) = True
faceUp (Card _ _ FaceDown) = False

faceDown = not . faceUp

flipCard (Card r s FaceUp) = Card r s FaceDown
flipCard (Card r s FaceDown) = Card r s FaceUp

isOneRankHigherThan c0 c1 = (fromEnum (rank c0)) == 1 + (fromEnum (rank c1))
isSameSuitAs c0 c1 = (suit c0) == (suit c1)
