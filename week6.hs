{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char
import GHC.Float (fromRat'')

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

mult10 :: [Int] -> [Int]
mult10 = map (*10)

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower



