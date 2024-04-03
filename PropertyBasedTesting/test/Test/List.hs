module Test.List where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.List (sort)
import List

-- This is an example of using Hedgehog for property-based testing. 
-- In this framework, testing is done in three steps: 
-- * The input is generated 
-- * The function under test is run on the generated input and a property is checked. 
-- * If a property is not held then the input is shrunk in such a way that the test 
--   still fails, but the input is easy to use in Debugging. 

-- Here we test several functions over lists

-- Generator of integer numbers from 0 to 100. 
genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)
-- Range.constant generates values disregarding the size parameter. 

-- If we want to generate some values more often than the other 
-- we can make them depend on the Size parameter. 
-- Use Range.linear, Range.linearFromSource, Range.exponential and others (Hoogle them). 

-- Generator of lists of integer numbers with the length from minLength to maxLength
genList :: Int -> Int -> Gen [Int]
genList minLength maxLength = Gen.list (Range.constant minLength maxLength) genInt

-- Every element of a list is not greater than its maximum. 
prop_maximum :: Property
prop_maximum = property $ do
  list <- forAll $ genList 1 100
  let maxValue = maximumValue list
  assert (all (<= maxValue) list)

-- Every element of a list is not less than the maximum. 
prop_minimum :: Property
prop_minimum = property $ do
  list <- forAll $ genList 1 100
  let minValue = minimumValue list
  assert (all (>= minValue) list)

-- To generate a sorted list, generate a random list and sort it (with a trusted library function)
genSortedList :: Int -> Int -> Gen (SortedList Int)
genSortedList minLength maxLength = Sorted . sort <$> genList minLength maxLength

-- MinimumValue works the same on normal and sorted lists. 
prop_minimumSorted :: Property
prop_minimumSorted = property $ do
  list <- forAll $ genSortedList 1 100
  minimumValue list === minimumValue (getSorted list)

-- MaximumValue works the same on normal and sorted lists. 
prop_maximumSorted :: Property
prop_maximumSorted = property $ do
  list <- forAll $ genSortedList 1 100
  maximumValue list === maximumValue (getSorted list)

-- By reversing a list twice we get the same result.
prop_reverseList :: Property
prop_reverseList = property $ do
  list <- forAll $ genList 1 100
  reverseList (reverseList list) === list

-- Fast reversal works as efficient as the trivial one.
prop_fastReverseList :: Property
prop_fastReverseList = property $ do
  list <- forAll $ genList 1 100
  reverseList list === fastReverseList list

props :: [TestTree]
props =
  [ testProperty "Maximum value is not less than all elements of the list" prop_maximum
  , testProperty "Minimum value is not more than all elements of the list" prop_minimum
  , testProperty "Maximum value is not less than all elements of the sorted list" prop_maximumSorted
  , testProperty "Minimum value is not more than all elements of the sorted list" prop_minimumSorted
  , testProperty "Reversing list twice gets the input list" prop_reverseList
  , testProperty "Fast reverse gives the same " prop_fastReverseList
  ]