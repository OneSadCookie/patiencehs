module Necessities (
    NFData (rnf),

    module Card,
    module Deal,
    module Deck,
    module MovePatience,
    module Patience,
    module Pile,
    module Rule,
) where

import Control.Parallel.Strategies

import Card
import Deal
import Deck
import MovePatience
import Patience
import Pile
import Rule
