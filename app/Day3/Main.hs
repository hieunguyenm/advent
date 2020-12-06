module Main where

main :: IO ()
main = do
  d <- readFile "app/Day3/input"
  let a = (checkTrees 0 1 3 1 $ lines d)
  print a
  print $
    a
      * (checkTrees 0 1 1 1 $ lines d)
      * (checkTrees 0 1 5 1 $ lines d)
      * (checkTrees 0 1 7 1 $ lines d)
      * (checkTrees 0 1 1 2 $ lines d)

checkTrees :: Int -> Int -> Int -> Int -> [String] -> Int
checkTrees x i right down levels
  | null l = x
  | otherwise =
    let t = head l
        s = drop (mod (i * right) $ length t) t
     in case null s of
          False -> case '#' == head s of
            True -> checkTrees (x + 1) (i + 1) right down l
            _ -> checkTrees x (i + 1) right down l
          _ -> x
  where
    l = drop down levels