{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)
import GHC.Exts.Heap (GenClosure(stack_marking))

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

countSpaces :: String -> Int
-- countSpaces "" = 0
-- countSpaces (x:xs)
--     | x == ' ' = 1 + countSpaces xs
--     | otherwise = countSpaces xs
countSpaces xs = sum [1 | x <- xs, x == ' ']

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] [] = []
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
    | x <= y = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys


headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

removeSecond :: [a] -> [a]
removeSecond [] = []
removeSecond [x] = [x]
removeSecond (x : y : xs) = x : xs

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x : y : xs) = y : x : xs 

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs

countIntegers :: Int -> [Int] -> Int
-- countIntegers _ [] = 0
-- countIntegers x (y:ys)
--     | x == y = 1 + countIntegers x ys
--     | otherwise = countIntegers x ys
countIntegers x ys = sum [1 | y <- ys, x == y]

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll x (y:ys)
    | x == y = removeAll x ys
    | otherwise = y : removeAll x ys 

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst x (y:ys)
    | x == y = y : removeAll x ys
    | otherwise = y : removeAllButFirst x ys

type StudentMark = (String, Int)
testData :: [StudentMark]
testData =
    [ 
        ("John", 53),
        ("Sam", 16),
        ("Kate", 85),
        ("Jill", 65),
        ("Bill", 37),
        ("Amy", 22),
        ("Jack", 41),
        ("Sue", 71)
    ]

listMarks :: String -> [StudentMark] -> [Int]
-- listMarks _ [] = []
-- listMarks str (x:xs) 
--     | fst x == str = snd x : listMarks str xs
--     | otherwise = listMarks str xs
listMarks str stmks = [mk | (st, mk) <- stmks, st == str] 

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

prefix :: [Int] -> [Int] -> Bool
prefix [] [] = True
prefix [] _ = True
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

subSequence :: [Int] -> [Int] -> Bool
subSequence (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = subSequence (x:xs) ys





