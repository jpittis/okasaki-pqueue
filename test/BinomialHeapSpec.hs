module BinomialHeapSpec (spec) where

import Test.Hspec
import BinomialHeap
import Control.Monad.State

spec :: Spec
spec =
  describe "BinomialHeap" $ do
    it "an empty heap is empty" $
      isEmpty (empty :: Heap Int) `shouldBe` True

    it "a heap acts as a priority queue" $
      withHeap empty $ do
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
