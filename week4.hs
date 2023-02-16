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

-- ******************** 
-- ANSWERS BEGIN HERE
-- ******************** 

-- QUESTION 1
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

-- QUESTION 2
grade :: StudentMark -> Char
grade (st, mk)
    | mk > 100 = error "Mark cannot be above 100"
    | mk >= 70 = 'A'
    | mk >= 60 = 'B'
    | mk >= 50 = 'C'
    | mk >= 40 = 'D'
    | mk >= 0 = 'F'
    | otherwise = error "Mark cannot be below 0"

-- QUESTION 3
capMark :: StudentMark -> StudentMark
capMark (st, mk)
    | mk > 100 = error "Mark cannot be above 100"
    | mk >= 40 = (st, 40)
    | mk >= 0 = (st, mk)
    | otherwise = error "Mark cannot be below 0"

-- QUESTION 4
firstNumbers :: Int -> [Int]
firstNumbers x = [1 .. x]

-- QUESTION 5
firstSquares :: Int -> [Int]
firstSquares x = [i ^ 2 | i <- [1 .. x]]

-- QUESTION 6
capitalise :: String -> String
capitalise x = [toUpper i | i <- x]

-- QUESTION 7
onlyDigits :: String -> String
onlyDigits x = [i | i <- x, isDigit i]

-- QUESTION 8
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [capMark (st, mk) | (st, mk) <- stmks]

-- QUESTION 9
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(st, grade (st, mk))| (st, mk) <- stmks]

-- QUESTION 10
-- duplicate :: String -> Int -> String
-- duplicate str i
--     | i <= 1 = str
--     | otherwise = str ++ duplicate str (i - 1)

duplicate :: String -> Int -> String
duplicate str num = concat[str | i <- [1 .. num]]

-- QUESTION 11
divisors :: Int -> [Int]
divisors x = [i | i <- [1 .. x], mod x i == 0]

-- QUESTION 12
isPrime :: Int -> Bool
isPrime x = length (divisors x) == 2 

-- QUESTION 13
split :: [(a,b)] -> ([a], [b])
split fsl = ([fst fs | fs <- fsl], [snd fs | fs <- fsl])

-- split :: [(a,b)] -> ([a],[b])
-- split fstscd = (first, second)
--     where
--         first = [fst | (fst, _) <- fstscd]
--         second = [scd | (_, scd) <- fstscd]
