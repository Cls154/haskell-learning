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

-- ******************** 
-- ANSWERS BEGIN HERE
-- ******************** 

-- QUESTION 1
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

-- QUESTION 2
removeSecond :: [a] -> [a]
removeSecond [] = []
removeSecond [x] = [x]
removeSecond (x : y : xs) = x : xs

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

-- QUESTION 3
rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x : y : xs) = y : x : xs 

-- QUESTION 4
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- QUESTION 5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

-- QUESTION 6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

-- QUESTION 7
orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs

-- QUESTION 8
countIntegers :: Int -> [Int] -> Int
-- countIntegers _ [] = 0
-- countIntegers x (y:ys)
--     | x == y = 1 + countIntegers x ys
--     | otherwise = countIntegers x ys
countIntegers x ys = sum [1 | y <- ys, x == y]

-- QUESTION 9
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll x (y:ys)
    | x == y = removeAll x ys
    | otherwise = y : removeAll x ys 

-- QUESTION 10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst x [] = []
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


-- QUESTION 11
listMarks :: String -> [StudentMark] -> [Int]
-- listMarks _ [] = []
-- listMarks str (x:xs) 
--     | fst x == str = snd x : listMarks str xs
--     | otherwise = listMarks str xs
listMarks str stmks = [mk | (st, mk) <- stmks, st == str] 

-- QUESTION 12
sorted :: [Int] -> Bool
-- sorted [] = True
-- sorted [x] = True
-- sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted xs = andAll [x <= y | (x, y) <- zip xs (tail xs)]

-- QUESTION 13
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = True
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

-- QUESTION 14
subSequence :: [Int] -> [Int] -> Bool
subSequence (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = subSequence (x:xs) ys





