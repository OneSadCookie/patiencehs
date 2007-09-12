import Control.Arrow
import Control.Monad
import List
import Random

import Card
import Patience
import Pile
import BeleagueredCastle
import Spider

bfs f [] = []
bfs f l =
    let l' = concatMap f l
    in l' ++ (bfs f l')
