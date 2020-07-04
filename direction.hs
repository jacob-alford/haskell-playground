module Codewars.Kata.Reduction where
-- import Codewars.Kata.Reduction.Direction

data Direction = North | East | West | South deriving (Eq, Show)

type Vector = (Int, Int)

toVectors :: [Direction] -> [Vector]
toVectors dirs = [ mapToVec x | x <- dirs ]
 where
  mapToVec a = case a of
    North -> (0, 1)
    South -> (0, -1)
    East  -> (1, 0)
    West  -> (-1, 0)

sumVectors :: [Vector] -> Vector
sumVectors = foldl (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)

getNS :: Int -> [Direction]
getNS a | a < 0     = [South]
        | a > 0     = [North]
        | otherwise = []

getWE :: Int -> [Direction]
getWE a | a < 0     = [West]
        | a > 0     = [East]
        | otherwise = []

toDirections :: Vector -> [Direction]
toDirections (we, ns) =
  (take (abs ns) $ concat (repeat (getNS ns)))
    ++ (take (abs we) $ concat (repeat (getWE we)))

dirReduce :: [Direction] -> [Direction]
dirReduce = toDirections . sumVectors . toVectors
