module Common.Search (
    bfs,
    dfs,
) where

import Control.DeepSeq
import qualified Data.Set as Set

data Queue a = Queue [a] [a]

emptyQueue = Queue [] []

isEmptyQueue (Queue [] []) = True
isEmptyQueue _             = False

enqueue (Queue front back) e = Queue front (e:back)

enqueueMany (Queue front back) l = (Queue front (reverse l ++ back))

dequeue (Queue [] []) = error "Dequeue from empty queue"
dequeue (Queue [] back) = dequeue $ Queue (reverse back) []
dequeue (Queue (e:front) back) = (Queue front back, e)

data Stack a = Stack [a]

emptyStack = Stack []

isEmptyStack (Stack []) = True
isEmptyStack _          = False

push (Stack es) e = Stack (e:es)

pushMany (Stack es) l = Stack (l ++ es)

pop (Stack []) = error "Pop from empty stack"
pop (Stack (e:es)) = (Stack es, e)

class Fringe f where
    emptyFringe :: f a
    isEmptyFringe :: f a -> Bool
    add :: f a -> a -> f a
    addMany :: f a -> [a] -> f a
    remove :: f a -> (f a, a)

instance Fringe Queue where
    emptyFringe = emptyQueue
    isEmptyFringe = isEmptyQueue
    add = enqueue
    addMany = enqueueMany
    remove = dequeue

instance Fringe Stack where
    emptyFringe = emptyStack
    isEmptyFringe = isEmptyStack
    add = push
    addMany = pushMany
    remove = pop

search :: (NFData a, Ord a, Fringe f) => f a -> a -> (a -> [a]) -> [a]
search emptyFringe start kids =
    search' Set.empty (add emptyFringe start) kids where
        search' seen fringe kids
            | isEmptyFringe fringe = []
            | otherwise           =
                let (fringe', e) = remove fringe
                    newKids = force $ filter (not . (`Set.member` seen)) (kids e)
                    fringe'' = addMany fringe' newKids
                in e : (search' (Set.insert e seen) fringe'' kids)

bfs :: (NFData a, Ord a) => a -> (a -> [a]) -> [a]
bfs = search emptyQueue

dfs :: (NFData a, Ord a) => a -> (a -> [a]) -> [a]
dfs = search emptyStack
