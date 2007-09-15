module Search (
    dfs,
) where

import qualified Data.Set as Set
import Data.Sequence

push es e = e <| es
pop seq = case viewr seq of es :> e -> (es, e)

pushMany = (. fromList) . (><)

dfs :: (Eq a, Ord a) => a -> (a -> [a]) -> [a]
dfs start kids = dfs' Set.empty (singleton start) kids where
    dfs' seen fringe kids
        | Data.Sequence.null fringe = []
        | otherwise                 =
            let (fringe', e) = pop fringe
                newKids = kids e
                newKids' = filter ((flip Set.notMember) seen) newKids
                fringe'' = pushMany fringe' newKids'
            in e : (dfs' (Set.insert e seen) fringe'' kids)
