import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Sequence
import List
import Random

--import Action
import Card
import Deck
import Game
import Layout
import Patience
import Pile
import Rule
import Search
import Shuffle

import BeleagueredCastle
--import Klondike

go = do
    gen <- newStdGen
    let bc = begin beleagueredCastle gen
        tree = dfs bc (map (uncurry applyMove) . liftM2 map (,) moves)
    putStr $ show tree
