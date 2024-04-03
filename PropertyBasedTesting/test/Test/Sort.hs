module Test.Sort where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Sort
import qualified Data.List as L

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)

genList :: Int -> Int -> Gen [Int]
genList minLength maxLength = Gen.list (Range.constant minLength maxLength) genInt

prop_sorted :: Property 
prop_sorted = property $ do 
  list <- forAll $ genList 1 100 
  let sorted = sort list 
  assert (isSorted sorted)

props :: [TestTree]
props =
  [ testProperty "The sorted list is ordered" prop_sorted
  ]