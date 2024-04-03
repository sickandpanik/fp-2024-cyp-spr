module Sort where 

sort :: [Int] -> [Int]
sort [] = []
sort (h:t) =
  let smaller = sort [x | x <- t, x < h]
      greater = sort [x | x <- t, x >= h]
  in smaller ++ h : greater

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:t) = x <= y && isSorted (y:t) 
isSorted _ = True 
