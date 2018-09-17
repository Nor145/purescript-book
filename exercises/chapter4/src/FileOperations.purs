module FileOperations where

import Prelude

import Data.Array (concatMap, (:))
import Data.Array.Partial (head, tail)
import Data.Path (Path, ls)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- 4.4

-- 1.
isEven :: Number -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven $ n - 2

-- 2.
countEven :: Array Number -> Number
countEven xs =
  if null xs
  then 0
  else if even $ head xs
    then 1 + countEven $ tail xs
    else countEven $ tail xs

-- 4.7

-- 1.
squares :: Array Number -> Array Number
squares = map (\n -> n * n)

-- 2.
removeNegatives :: Array Number -> Array Number
removeNegatives = filter (\n -> n >= 0 )

-- 3.
infix 8 filter as <$?>

removeNegativesInfix :: Array Number -> Array Number
removeNegativesInfix x = (\n -> n >= 0) <$?> x

-- 4.11
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

factors' :: Int -> Array Int
factors' n = do
  j <- 1 .. n
  guard $ n `mod` j == 0
  [j]

-- 1.
isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

-- 2.
cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]

-- 3.
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ (a * a) + (b * b) == (c * c)
  pure [a, b, c]

-- 4.
factorsX :: Int -> Array Int
factorsX n = do
  x <- 1 .. n
  guard $ (n `mod` x) == 0
  pure x

factorizations :: Int -> Array (Array Int)
factorizations n = [n] : do
  x <- factorsX n
  guard $ x > 1 && x < n
  xs <- factorizations $ n / x
  pure $ x : xs

-- 4.15

-- 1.
allTrue :: Array Boolean -> Boolean
allTrue = foldl (\a x -> a && x) true

-- 3.
count :: forall a. ( a -> Boolean ) -> Array a -> Int
count p xs = count' p xs 0 
  where
  count' _ [] acc     = acc
  count' p (x:xs) acc = if p x
                        then count' p xs (acc+1)
                        else count' p xs acc

-- 4.
reverse :: forall a. Array a -> Array a
reverse = foldl (\acc n -> n : acc) []

-- 4.17

-- 1.
onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

-- 2.
largestFile :: Path -> Maybe Path
largestFile = foldl largest Nothing <<< onlyFiles
  where
    largest Nothing path = Just path
    largest (Just acc) path = if size acc > size path
      then Just acc
      else Just path

smallestFile :: Path -> Maybe Path
smallestFile = foldl smallest Nothing <<< onlyFiles
  where
    smallest Nothing path = Just path
    smallest (Just acc) path = if size acc < size path
      then Just acc
      else Just path

-- 3.
whereIs :: String -> Maybe Path
whereIs file = head $ do
  path <- allFiles root
  child <- ls path
  guard $ filename child == file
  pure path

