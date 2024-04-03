{-# LANGUAGE InstanceSigs #-}
module List where

-- Type class for containers with known minimum and maximum values 
class Ranged t where
  maximumValue :: Ord a => t a -> a
  minimumValue :: Ord a => t a -> a

-- When working with a simple list, we traverse it to find the min and max values
instance Ranged [] where
  maximumValue :: Ord a => [a] -> a
  maximumValue xs =
      go (head xs) (tail xs)
    where
      go curMax [] = curMax
      go curMax (h : t) | h > curMax = go h t
                        | otherwise = go curMax t

  minimumValue :: Ord a => [a] -> a
  minimumValue xs =
      go (head xs) (tail xs)
    where
      go curMax [] = curMax
      go curMax (h : t) | h < curMax = go h t
                        | otherwise = go curMax t

-- Data types for sorted lists
newtype SortedList a = Sorted { getSorted :: [a] } deriving (Show, Eq)

-- When a list is sorted, its min values is the first element, and the max value is in the last position
instance Ranged SortedList where
  maximumValue :: Ord a => SortedList a -> a
  maximumValue = last . getSorted
  
  minimumValue :: Ord a => SortedList a -> a
  minimumValue = head . getSorted

-- Etalon reversal
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h : t) = reverseList t ++ [h]

-- More efficient reversal
fastReverseList :: [a] -> [a]
fastReverseList =
    go []
  where
    go acc [] = acc
    go acc (h : t) = go (h : acc) t