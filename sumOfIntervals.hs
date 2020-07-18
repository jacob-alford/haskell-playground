module SumOfIntervals
  ( sumOfIntervals
  )
where

import           Data.List                      ( sort )

joint :: (Int, Int) -> (Int, Int) -> Bool
joint (a1, a2) (b1, b2) = (b1 > a1 && b1 < a2) || (b2 < a2 && b2 > a1)

properIntv :: (Int, Int) -> (Int, Int) -> (Int, Int)
properIntv (a1, a2) (b1, b2) = (min a1 b1, max a2 b2)

reduceToDisjoint :: [(Int, Int)] -> [(Int, Int)]
reduceToDisjoint intvs = foldl (\acc next -> if joint (last acc) next then (acc) : (properIntv (last acc) (next)) else )

sumOfIntervals :: [(Int, Int)] -> Int
sumOfIntervals intervals = undefined
