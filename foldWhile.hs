module Fold where

foldWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldWhile _ _ acc [] = acc
foldWhile p f acc arr
  | not (p next) = acc
  | otherwise = foldWhile p f next (tail arr)
  where next = f acc (head arr)