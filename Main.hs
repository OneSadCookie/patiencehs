import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
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
import Shuffle

import BeleagueredCastle
--import Klondike

go = do
    gen <- newStdGen
    let bc = begin beleagueredCastle gen;
        ms = moves bc;
        after = map (applyMove bc) ms
    putStr (show bc)
    putStrLn (show ms)
    putStr (show after)
