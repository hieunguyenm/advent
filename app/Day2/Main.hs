module Main where

import Data.Char (isDigit)
import Text.Read (readMaybe)

type Policy = (Int, Int, Char)

main :: IO ()
main = do
  d <- readFile "app/Day2/input"
  let f = \px s i ->
        let (pol, pswd) = parsePolicy s
         in case px pol pswd of
              True -> i + 1
              _ -> i
  print $ foldr (f passwordHasNChars) 0 $ lines d
  print $ foldr (f passwordHasPositionChars) 0 $ lines d

passwordHasNChars :: Policy -> String -> Bool
passwordHasNChars (mn, mx, char) pswd =
  let f ch cur x
        | ch == cur = x + 1
        | otherwise = x
      count = foldr (f char) 0 pswd
   in count <= mx && count >= mn

passwordHasPositionChars :: Policy -> String -> Bool
passwordHasPositionChars (mn, mx, char) pswd =
  (char == head (drop (mn - 1) pswd)) /= (char == head (drop (mx - 1) pswd))

parsePolicy :: String -> (Policy, String)
parsePolicy s =
  let (smn, rem1) = span isDigit s
      (smx, rem2) = span isDigit $ tail rem1
   in case readMaybe smn :: Maybe Int of
        Just mn -> case readMaybe smx :: Maybe Int of
          Just mx -> ((mn, mx, head $ tail rem2), drop 4 rem2)
          _ -> ((0, 0, '0'), "") -- Being lazy
        _ -> ((0, 0, '0'), "")
