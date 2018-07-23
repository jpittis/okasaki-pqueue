module BinomialHeap
    ( BinomialHeap
    , empty
    , isEmpty
    , insert
    , findMin
    , deleteMin
    , meld
    ) where

import Heap

data Ord a => Tree a = Tree
  { node     :: a
  , rank     :: Int
  , children :: [Tree a]
  }

newtype Ord a => BinomialHeap a = BinomialHeap [Tree a]

instance Heap BinomialHeap where
  empty = BinomialHeap []

  isEmpty (BinomialHeap li) = null li

  insert x (BinomialHeap h) = BinomialHeap $ ins (Tree x 0 []) h

  findMin (BinomialHeap [])     = Nothing
  findMin (BinomialHeap [t])    = Just (node t)
  findMin (BinomialHeap (t:ts)) = do
    minRest <- findMin (BinomialHeap ts)
    return (if node t < minRest then node t else minRest)

  deleteMin heap@(BinomialHeap []) = heap
  deleteMin (BinomialHeap ts) =
    let (min, rest) = getMin ts in
    meld (BinomialHeap . reverse . children $ min) (BinomialHeap rest)
    where
      getMin :: Ord a => [Tree a] -> (Tree a, [Tree a])
      getMin [t] =  (t, [])
      getMin (t:ts) =
        let (minRest, rest) = getMin ts in
        if node t <= node minRest then (t, ts) else (minRest, t:rest)

  meld (BinomialHeap h1) (BinomialHeap h2) = BinomialHeap $ go h1 h2
    where
      go :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
      go [] [] = []
      go ts [] = ts
      go [] ts = ts
      go (t1:ts1) (t2:ts2)
        | rank t1 < rank t2 = t1 : go ts1 (t2:ts2)
        | rank t2 < rank t1 = t2 : go (t1:ts1) ts2
        | otherwise         = ins (link t1 t2) (go ts1 ts2)

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Tree n1 r1 c1) t2@(Tree n2 r2 c2) = 
  if n1 < n2 then Tree n1 (r1+1) (t2:c1) else Tree n2 (r2+1) (t1:c2)

ins :: Ord a => Tree a -> [Tree a] -> [Tree a]
ins ti []     = [ti]
ins ti (t:ts) = if rank ti < rank t then ti:t:ts else ins (link ti t) ts
