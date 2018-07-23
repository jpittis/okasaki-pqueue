module SkewBinomialHeap
    ( SkewBinomialHeap
    , empty
    , isEmpty
    ) where

import Heap

data Ord a => Tree a = Tree
  { node     :: a
  , rank     :: Int
  , children :: [Tree a]
  }

newtype Ord a => SkewBinomialHeap a = SkewBinomialHeap [Tree a]

instance Heap SkewBinomialHeap where
  empty = SkewBinomialHeap []

  isEmpty (SkewBinomialHeap li) = null li
