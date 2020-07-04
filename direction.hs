module Codewars.Kata.Reduction where
-- import Codewars.Kata.Reduction.Direction

data Direction = North | East | West | South deriving (Eq, Show)

isNS :: Direction -> Bool
isNS = (`elem` [North, South])

isWE :: Direction -> Bool
isWE = (`elem` [West, East])

isOpposite :: Direction -> Direction -> Bool
isOpposite a b | isNS a && isNS b = a /= b
               | isWE a && isWE b = a /= b
               | otherwise        = False

eliminateNeighbors :: [Direction] -> [Direction]
eliminateNeighbors []  = []
eliminateNeighbors [a] = [a]
eliminateNeighbors (d : ds)
  | d `isOpposite` (head ds) = eliminateNeighbors $ drop 1 ds
  | otherwise                = d : eliminateNeighbors ds

dirReduce :: [Direction] -> [Direction]
dirReduce lastDirs | nextDirs == lastDirs = nextDirs
                   | otherwise            = dirReduce nextDirs
  where nextDirs = eliminateNeighbors lastDirs
