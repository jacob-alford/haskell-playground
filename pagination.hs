module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount coll pp = ceiling $ items / itemsPerPage
 where
  items        = fromIntegral $ itemCount coll :: Double
  itemsPerPage = fromIntegral pp :: Double

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount coll pp pageNum | pageNum + 1 > totPgs  = Nothing
                              | pageNum + 1 == totPgs = Just remaining
                              | pageNum >= 0          = Just actualItems
                              | otherwise             = Nothing
 where
  actualItems = length $ take pp coll
  totPgs      = pageCount coll pp
  remaining   = itemCount coll - (totPgs - 1) * actualItems

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex coll pp itemNo | itemNo + 1 > (pageCount coll pp) = Nothing
                         | otherwise = Just (floor $ index / pc)
 where
  index = fromIntegral itemNo :: Double
  pc    = fromIntegral $ pageCount coll pp :: Double
