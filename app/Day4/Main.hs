{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Char (isDigit, isSpace, ord)
import qualified Data.HashSet as HS
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

main :: IO ()
main = do
  d <- readFile "app/Day4/input"
  let keys = HS.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  let g = group $ lines d
  print $ foldr (checkRequired keys) 0 g
  print $ foldr (checkRequiredValidate keys) 0 g

checkRequired :: HS.HashSet String -> String -> Int -> Int
checkRequired allKeys s acc
  | missing == 0 = acc + 1
  | otherwise = acc
  where
    missing = HS.size $ HS.difference allKeys $ HS.delete "cid" $ HS.fromList $ map head $ splitOn ":" <$> words s

checkRequiredValidate :: HS.HashSet String -> String -> Int -> Int
checkRequiredValidate allKeys s acc
  | missing == 0 = acc + 1
  | otherwise = acc
  where
    missing = HS.size $ HS.difference allKeys $ HS.delete "cid" $ HS.fromList $ map head $ filter validate $ (splitOn ":") <$> words s

validate :: [String] -> Bool
validate [k, v]
  | k == "cid" = True
  | k == "byr" = years v 1920 2002
  | k == "iyr" = years v 2010 2020
  | k == "eyr" = years v 2020 2030
  | k == "hgt" = hgt v
  | k == "hcl" = hcl v
  | k == "ecl" = HS.member v $ HS.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  | k == "pid" = length v == 9 && all isDigit v
  | otherwise = False
  where
    years s mn mx = case readMaybe s :: Maybe Int of
      Just n -> n >= mn && n <= mx
      _ -> False
    hgt v =
      let (h, u) = span isDigit v
       in case readMaybe h :: Maybe Int of
            Just n ->
              if
                  | u == "cm" -> n >= 150 && n <= 193
                  | u == "in" -> n >= 59 && n <= 76
                  | otherwise -> False
            _ -> False
    hcl v = head v == '#' && (isValidHex $ tail v)
    isValidHex s = and $ f <$> s
      where
        f c =
          let n = ord c
           in if
                  | n >= 48 && n < 58 -> True
                  | n >= 97 && n < 103 -> True
                  | otherwise -> False
validate _ = False

group :: [String] -> [String]
group s = foldr f [""] s
  where
    f s (a : as)
      | all isSpace s = "" : a : as
      | otherwise = case all isSpace a of
        True -> s : as
        _ -> (a ++ " " ++ s) : as
