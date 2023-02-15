import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumNumbersBetween :: Int -> Int -> Int 
sumNumbersBetween x y = sum [i | i <- [x .. y]]
-- sumNumbersBetween x y
--     | x > y = 0
--     | otherwise = x + sumNumbersBetween (x + 1) y

-- sumEvenNumbersBetween :: Int -> Int -> Int
-- sumEvenNumbersBetween x y
--     | x > y = 0
--     | mod x 2 == 0 = x + sumEvenNumbersBetween (x + 2) y
--     | otherwise = sumEvenNumbersBetween (x + 1) y

sumEvenNumberBetween :: Int -> Int -> Int
sumEvenNumberBetween x y = sum [i | i <- [x .. y], mod i 2 == 0]

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
    where
    sumMarks = sum [mk | (_ , mk) <- stmks]
    numberOfStudents = length stmks

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

grade :: [StudentMark] -> Char
grade stmk = 