module RangeExtractor.JorgeVS.Kata where
import           Data.List                      ( intercalate )

groupWhen :: (a -> a -> Bool) -> [a] -> [[a]]
groupWhen _ []  = []
groupWhen _ [a] = [[a]]
groupWhen p (x : xs) | p x (head y) = (x : y) : ys
                     | otherwise    = [x] : y : ys
  where (y : ys) = groupWhen p xs

precedes :: Integer -> Integer -> Bool
precedes = (==) . succ

rangeify :: [[Integer]] -> [String]
rangeify [] = []
rangeify (x : xs) | length x == 2 = [lb] ++ [ub] ++ rangeify xs
                  | length x > 1  = [lb ++ "-" ++ ub] ++ rangeify xs
                  | otherwise     = [lb] ++ rangeify xs
 where
  lb = show $ head x
  ub = show $ last x

solution :: [Integer] -> String
solution = (intercalate ",") . rangeify . (groupWhen precedes)
