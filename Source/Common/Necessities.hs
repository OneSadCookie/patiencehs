module Common.Necessities (
    NFData (rnf),
    Generic,

    module Common.Card,
    module Common.Deal,
    module Common.Deck,
    module Common.MovePatience,
    module Common.Patience,
    module Common.Pile,
    module Common.Rule,
    module Common.UIPatience,
) where

import GHC.Generics
import Control.DeepSeq

import Common.Card
import Common.Deal
import Common.Deck
import Common.MovePatience
import Common.Patience
import Common.Pile
import Common.Rule
import Common.UIPatience
