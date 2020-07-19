module CamelCase where
import           Data.Char                      ( toUpper
                                                , toLower
                                                , isUpper
                                                )

fOr :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
fOr f g = (\a -> f a || g a)

fNot :: (a -> Bool) -> (a -> Bool)
fNot f = (\a -> not $ f a)

isPascal :: String -> Bool
isPascal []      = False
isPascal (x : _) = isUpper x

capitalize :: String -> String
capitalize []       = []
capitalize (x : xs) = toUpper x : map toLower xs

lowerize :: String -> String
lowerize []       = []
lowerize (x : xs) = toLower x : xs

isLetter :: Char -> Bool
isLetter = fOr (`elem` ['a' .. 'z']) (`elem` ['A' .. 'Z'])

restOfWord :: String -> String
restOfWord = takeWhile isLetter

afterWord :: String -> String
afterWord = dropWhile isLetter

pascal :: String -> String
pascal [] = []
pascal (x : xs)
  | isLetter x = toUpper x : restOfWord xs ++ pascal (afterWord xs)
  | otherwise  = pascal xs

toCamelCase :: String -> String
toCamelCase str | isPascal trimmedStr = pascal trimmedStr
                | otherwise           = lowerize $ pascal trimmedStr
  where trimmedStr = dropWhile (fNot isLetter) str
