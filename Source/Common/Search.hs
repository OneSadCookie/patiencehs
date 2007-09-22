module Search (
    bfs,
) where

import qualified Data.Set as Set

data Queue a = Queue [a] [a]

emptyQueue = Queue [] []

isEmptyQueue (Queue [] []) = True
isEmptyQueue _             = False

enqueue (Queue front back) e = Queue front (e:back)

enqueueMany (Queue front back) l = (Queue front (l ++ back))

dequeue (Queue [] []) = error "Dequeue from empty queue"
dequeue (Queue [] back) = dequeue $ Queue (reverse back) []
dequeue (Queue (e:front) back) = (Queue front back, e)

bfs :: Ord a => a -> (a -> [a]) -> [a]
bfs start kids = bfs' Set.empty (enqueue emptyQueue start) kids where
    bfs' seen fringe kids
        | isEmptyQueue fringe = []
        | otherwise           =
            let (fringe', e) = dequeue fringe
                newKids = kids e
                fringe'' = enqueueMany fringe' newKids
            in  if e `Set.member` seen
                then bfs' seen fringe' kids
                else e : (bfs' (Set.insert e seen) fringe'' kids)
