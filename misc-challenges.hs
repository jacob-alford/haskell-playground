nextNeq :: Eq a => a -> [a] -> [a]
nextNeq y = dropWhile (\x -> x == y)

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder []  = []
uniqueInOrder [x] = [x]
uniqueInOrder (x : xs) | x == head xs = x : uniqueInOrder (nextNeq x xs)
                       | otherwise    = x : uniqueInOrder xs

readInts :: String -> [Int]
readInts = (map read) . words

space :: String -> String -> String
space a b = unwords [a, b]

highAndLow :: String -> String
highAndLow xs = (big xs) `space` (smol xs)
 where
  big  = show . maximum . readInts
  smol = show . minimum . readInts
