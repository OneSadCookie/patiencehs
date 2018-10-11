{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Common.Card (
    Rank (Ace, Two, Three, Four, Five, Six, Seven,
          Eight, Nine, Ten, Jack, Queen, King),
    ranks,
    Suit (Hearts, Clubs, Diamonds, Spades),
    suits,
    AbstractCard (rank, suit),
    color,
    Card (Card),
    FacingCard (FaceUp, FaceDown, abstractCard),
    isFaceUp,
    turnUp,
    turnDown,
) where

import GHC.Generics
import Control.DeepSeq

data Rank = Ace | Two | Three | Four | Five | Six | Seven |
            Eight | Nine | Ten | Jack | Queen | King
    deriving (Bounded, Enum, Eq, Ord, Generic, NFData)

instance Show Rank where
    show Ace   = "A"
    show Ten   = "X"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show r     = show ((fromEnum r) + 1)

ranks :: [ Rank ]
ranks = [ minBound .. maxBound ]

data Color = Red | Black deriving (Eq)

data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Bounded, Enum, Eq, Ord, Generic, NFData)

instance Show Suit where
    show Hearts   = "H"
    show Clubs    = "C"
    show Diamonds = "D"
    show Spades   = "S"

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

data Card = Card Rank Suit deriving (Eq, Ord, Generic, NFData)

instance AbstractCard Card where
    rank (Card r _) = r
    suit (Card _ s) = s

instance Show Card where
    show (Card r s) = (show r) ++ (show s)

data FacingCard =
    FaceUp   { abstractCard :: Card } |
    FaceDown { abstractCard :: Card }
    deriving (Eq, Ord, Generic, NFData)

instance AbstractCard FacingCard where
    rank = rank . abstractCard
    suit = suit . abstractCard

instance Show FacingCard where
    show (FaceUp   card) = "|" ++ show card ++ "|"
    show (FaceDown card) = "<" ++ show card ++ ">"

isFaceUp (FaceUp _) = True
isFaceUp _          = False

turnUp (FaceDown card) = FaceUp card
turnUp c = c

turnDown (FaceUp card) = FaceDown card
turnDown c = c
