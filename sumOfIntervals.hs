module SumOfIntervals
  ( sumOfIntervals
  )
where

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (a1, a2) (b1, b2) | a1 == b1 || a2 == b2 = True
                           | a1 < b1 && a2 > b1   = True
                           | a1 > b1 && a2 < b2   = True
                           | a1 < b2 && a2 > b2   = True
                           | otherwise            = False

joint :: (Int, Int) -> (Int, Int) -> Bool
joint a b = overlaps a b || overlaps b a

joinIntv :: (Int, Int) -> (Int, Int) -> (Int, Int)
joinIntv (a1, a2) (b1, b2) = (min a1 b1, max a2 b2)

reduceToDisjoint :: [(Int, Int)] -> [(Int, Int)]
reduceToDisjoint []     = []
reduceToDisjoint ranges = join joints : reduceToDisjoint disjoints
 where
  x         = head ranges
  xs        = tail ranges
  join      = foldl joinIntv x
  joints    = filter (joint x) xs
  disjoints = filter (not . joint x) xs

repeatDisjoin :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
repeatDisjoin prev next | prev == next = next
                        | otherwise = repeatDisjoin next (reduceToDisjoint next)

sumIntvs :: [(Int, Int)] -> Int
sumIntvs = foldl (\total (low, high) -> total + (high - low)) 0

sumOfIntervals :: [(Int, Int)] -> Int
sumOfIntervals = sumIntvs . repeatDisjoin []
