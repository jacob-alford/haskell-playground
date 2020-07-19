module MiddlePermutation.JorgeVS.Kata where
import           Data.List                      ( delete
                                                , sort
                                                )

mId :: String -> Int
mId = floor . (/ 2) . fromIntegral . length

middlePermutation :: String -> String
middlePermutation []     = []
middlePermutation [x]    = [x]
middlePermutation [x, y] = [x, y]
middlePermutation xs | even $ length s = midEven : reverse restEven
                     | otherwise       = midOdd : middlePermutation restOdd
 where
  s        = sort xs
  midEven  = (!!) s $ (mId s - 1)
  midOdd   = (!!) s $ mId s
  restEven = delete midEven s
  restOdd  = delete midOdd s
