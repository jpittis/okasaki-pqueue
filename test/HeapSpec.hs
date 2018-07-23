module HeapSpec (spec) where

import Test.Hspec
import Control.Monad.State

import Heap
import BinomialHeap
import SkewBinomialHeap

spec :: Spec
spec = do
  describe "BinomialHeap" $
    heapSpec (empty :: BinomialHeap Int)

  describe "SkewBinomialHeap" $
    heapSpec (empty :: SkewBinomialHeap Int)

heapSpec :: (Heap h) => h Int -> Spec
heapSpec heap = do
    it "an empty heap is empty" $
      emptyHeapIsEmpty heap
    it "a heap behaves as a priority queue" $
      heapBehavesAsPriorityQueue heap

emptyHeapIsEmpty :: (Ord a, Heap h) => h a -> IO ()
emptyHeapIsEmpty heap = isEmpty heap `shouldBe` True

heapBehavesAsPriorityQueue :: (Heap h) => h Int -> IO ()
heapBehavesAsPriorityQueue heap =
  withHeap heap $ do
    modify (insert 1)
    modify (insert 2)
    modify (insert 3)
    gets isEmpty >>= lift . (`shouldBe` False)
    gets findMin >>= lift . (`shouldBe` Just 1)
    modify deleteMin
    gets findMin >>= lift . (`shouldBe` Just 2)
    modify deleteMin
    modify (insert 2)
    gets findMin >>= lift . (`shouldBe` Just 2)
    modify deleteMin
    gets findMin >>= lift . (`shouldBe` Just 3)
    modify deleteMin
    gets findMin >>= lift . (`shouldBe` Nothing)
    gets isEmpty >>= lift . (`shouldBe` True)

withHeap :: Monad m => s -> StateT s m a -> m ()
withHeap s f = void $ runStateT f s
