{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char
import GHC.Float (fromRat'')
import System.Directory (removeDirectoryRecursive)

import Prelude hiding (reverse) 

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs

alwaysEven :: (Int -> Int) -> [Int] -> Bool
-- alwaysEven f xs = length (filter even (map f xs)) == length xs
-- alwaysEven f xs = andAll (map (even . f) xs)
alwaysEven f = andAll . map (even . f)

updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
-- updatePositivesOnly _ [] = []
-- updatePositivesOnly f (x : xs)
--   | x > 0 = f x : updatePositivesOnly f xs
--   | otherwise = x : updatePositivesOnly f xs
updatePositivesOnly f = map (\x -> if x > 0 then f x else x)

-- mult10 :: [Int] -> [Int]
-- mult10 = map (*10)

-- onlyLowerCase :: String -> String
-- onlyLowerCase = filter isLower

orAll :: [Bool] -> Bool
orAll = foldr (||) False

sumSquares :: [Int] -> Int
sumSquares = foldr (+) 0 . (map (^2))

-- zeroToTen :: [Int] -> [Int]
-- zeroToTen = filter (<=10) . filter (>=0)

squareRoots :: [Float] -> [Float]
-- squareRoots xs = map (\x -> sqrt x) ((filter (\x -> x >= 0) xs))
squareRoots = map sqrt . filter (>=0)

countBetween :: Float -> Float -> [Float] -> Int
countBetween a b xs = length (filter (\x -> x >= a && x <= b) xs)

-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f = andAll . map ((>0) . f)
-- alwaysPositive f xs = andAll (map ((>0) . f) xs)
-- alwaysPositive f xs = length (filter (>0) (map f xs)) == length xs

-- productSquareRoots :: [Float] -> Float
-- productSquareRoots = foldr (*) 1 . (map (sqrt) . (filter (>0)))

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x:xs) 
  | f x = xs
  | otherwise = x : removeFirst f xs

-- removeLast :: (a -> Bool) -> [a] -> [a]
-- removeLast _ [] = []
-- removeLast f (x:xs) = x : reverse (removeFirst f (reverse xs))

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> (x <= 10) && (x >= 0))

mult10 :: [Int] -> [Int]
mult10 = foldr (\x xs -> x * 10 : xs) []

onlyLowerCase :: String -> String
onlyLowerCase = foldr (\x xs -> if isLower x then x : xs else xs) []

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f xs = foldr (\x acc -> f x > 0 && acc) True xs

productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr (\x acc -> if x > 0 then sqrt x * acc else acc) 1 xs

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []


-- updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
-- -- updatePositivesOnly _ [] = []
-- -- updatePositivesOnly f (x : xs)
-- --   | x > 0 = f x : updatePositivesOnly f xs
-- --   | otherwise = x : updatePositivesOnly f xs
-- updatePositivesOnly f = map (\x -> if x > 0 then f x else x)