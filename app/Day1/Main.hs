module Main where

import qualified Data.HashSet as HS
import qualified Data.SortedList as SL
import Text.Read (readMaybe)

year = 2020

main :: IO ()
main = do
  d <- readFile "app/Day1/input"
  print $ find2 HS.empty $ lines d
  print $ find3 (SL.toSortedList []) $ lines d

find2 :: HS.HashSet Int -> [String] -> Int
find2 _ [] = 0
find2 set (x : xs) = case (readMaybe x :: Maybe Int) of
  Just n ->
    let o = year - n
     in case HS.member o set of
          True -> n * o
          _ -> find2 (HS.insert n set) xs
  _ -> 0

find3 :: SL.SortedList Int -> [String] -> Int
find3 l (x : xs) = case (readMaybe x :: Maybe Int) of
  Just n -> case search n (SL.fromSortedList l) of
    Just a -> a
    _ -> find3 (SL.insert n l) xs
  _ -> 0

search :: Int -> [Int] -> Maybe Int
search _ [] = Nothing
search _ [_] = Nothing
search n (x : xs)
  | sum > year = search n $ (:) x $ init xs
  | sum < year = search n xs
  | otherwise = Just $ x * y * n
  where
    y = last xs
    sum = x + y + n