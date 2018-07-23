module BinomialHeap
    ( Heap
    , empty
    , isEmpty
    , insert
    , findMin
    , deleteMin
    , meld
    ) where

data Ord a => Tree a = Tree
  { node     :: a
  , rank     :: Int
  , children :: [Tree a]
  }

newtype Ord a => Heap a = Heap [Tree a]

empty :: Ord a => Heap a
empty = Heap []

isEmpty :: Ord a => Heap a -> Bool
isEmpty (Heap li) = null li

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Tree n1 r1 c1) t2@(Tree n2 r2 c2) = 
  if n1 < n2 then Tree n1 (r1+1) (t2:c1) else Tree n2 (r2+1) (t1:c2)

ins :: Ord a => Tree a -> [Tree a] -> [Tree a]
ins ti []     = [ti]
ins ti (t:ts) = if rank ti < rank t then ti:t:ts else ins (link ti t) ts

insert :: Ord a => a -> Heap a -> Heap a
insert x (Heap h) = Heap $ ins (Tree x 0 []) h

findMin :: Ord a => Heap a -> Maybe a
findMin (Heap [])     = Nothing
findMin (Heap [t])    = Just (node t)
findMin (Heap (t:ts)) = do
  minRest <- findMin (Heap ts)
  return (if node t < minRest then node t else minRest)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin heap@(Heap []) = heap
deleteMin (Heap ts) =
  let (min, rest) = getMin ts in
  meld (Heap . reverse . children $ min) (Heap rest)
  where
    getMin :: Ord a => [Tree a] -> (Tree a, [Tree a])
    getMin [t] =  (t, [])
    getMin (t:ts) =
      let (minRest, rest) = getMin ts in
      if node t <= node minRest then (t, ts) else (minRest, t:rest)

meld :: Ord a => Heap a -> Heap a -> Heap a
meld (Heap h1) (Heap h2) = Heap $ go h1 h2
  where
    go :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
    go [] [] = []
    go ts [] = ts
    go [] ts = ts
    go (t1:ts1) (t2:ts2)
      | rank t1 < rank t2 = t1 : go ts1 (t2:ts2)
      | rank t2 < rank t1 = t2 : go (t1:ts1) ts2
      | otherwise         = ins (link t1 t2) (go ts1 ts2)
