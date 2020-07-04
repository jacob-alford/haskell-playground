module RomanNumerals where

bigstNom :: Integer -> Integer
bigstNom nom = last $ takeWhile (<= nom) [1, 5, 10, 50, 100, 500, 1000]

romNoms :: Integer -> Char
romNoms nom = case nom of
  1    -> 'I'
  5    -> 'V'
  10   -> 'X'
  50   -> 'L'
  100  -> 'C'
  500  -> 'D'
  1000 -> 'M'
  _    -> error "Oops!  I forgot to write an error message!"

romanExpansion :: Integer -> [Integer]
romanExpansion nom | nom <= 0  = []
                   | otherwise = largestMatch : recurse
 where
  largestMatch = bigstNom nom
  recurse      = romanExpansion (nom - largestMatch)

isConsecutiveTo :: Integer -> Integer -> Bool
isConsecutiveTo b a =
  (fromIntegral b :: Double) / (fromIntegral a :: Double) == 5.0

groupToRomNom :: [Integer] -> [Integer]
groupToRomNom grp | grp == [1, 1, 1, 1]              = [1, 5]
                  | grp == [5, 1, 1, 1, 1]           = [1, 10]
                  | grp == [10, 10, 10, 10]          = [10, 50]
                  | grp == [50, 10, 10, 10, 10]      = [10, 100]
                  | grp == [100, 100, 100, 100]      = [100, 500]
                  | grp == [500, 100, 100, 100, 100] = [100, 1000]
                  | otherwise                        = grp

grpFsNns :: [Integer] -> [Integer]
grpFsNns arr
  | length arr < 4 = arr
  | isGrpOfFour    = groupToRomNom (x : take 3 xs) ++ grpFsNns (drop 3 xs)
  | isGrpOfFive    = groupToRomNom (x : take 4 xs) ++ grpFsNns (drop 4 xs)
  | otherwise      = x : grpFsNns xs
 where
  x           = head arr
  xs          = drop 1 arr
  isGrpOfFour = all (== x) (take 3 xs)
  isGrpOfFive =
    (all (== (head xs)) (take 4 xs)) && (x `isConsecutiveTo` (head xs))

toRomNom :: [Integer] -> [Char]
toRomNom = map romNoms

solution :: Integer -> String
solution = toRomNom . grpFsNns . romanExpansion
