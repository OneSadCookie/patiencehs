module Search (
    dfs,
) where

import Data.List
import Data.Sequence

push es e = e <| es
pop seq = case viewr seq of es :> e -> (es, e)

pushMany = (. fromList) . (><)

dfs :: Eq a => a -> (a -> [a]) -> [a]
dfs start kids = dfs' [] (singleton start) kids where
    dfs' seen fringe kids
        | Data.Sequence.null fringe = []
        | otherwise                 =
            let (fringe', e) = pop fringe
                newKids = kids e
                newKids' = filter ((flip notElem) seen) newKids
                fringe'' = pushMany fringe' newKids'
            in e : (dfs' (e:seen) fringe'' kids)
