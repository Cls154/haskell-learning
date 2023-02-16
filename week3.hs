-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||), (&&), gcd) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2  ||
infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m


-- nor :: Bool -> Bool -> Bool
-- nor False False = True
-- nor False True = False
-- nor True False = False
-- nor True True = False

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

fibonacci :: Int -> Int
-- fibonacci n
    -- | n == 0 = 0
    -- | n == 1 = 1
    -- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- QUESTION 1
-- i)
-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False
-- ii)
-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False
-- iii)
(&&) :: Bool -> Bool -> Bool
True && x = x
False && x = False

-- QUESTION 2
-- exOr :: Bool -> Bool -> Bool
-- exOr False False = False
-- exOr False True = True
-- exOr True False = True
-- exOr True True = False

exOr :: Bool -> Bool -> Bool
exOr True x = not x
exOr False x = x

-- QUESTION 3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x
ifThenElse False _ y = y

-- QUESTION 4
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month -- && ?? && ?? && ??

-- QUESTION 5
-- GUARDS
-- sumNumbers :: Int -> Int
-- sumNumbers n
--     | n <= 0 = n
--     | otherwise = n + sumNumbers (n - 1)

-- PATTERN MATCHING
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- QUESTION 6
-- GUARDS
-- sumSquares :: Int -> Int
-- sumSquares n
--     | n <= 0 = n
--     | otherwise = n ^ 2 + sumSquares (n - 1)

-- PATTERN MATCHING
sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares (n - 1)

-- QUESTION 7
-- GUARDS
-- power :: Int -> Int -> Int
-- power x y
--     | y == 1 = x
--     | otherwise = x * power x (y - 1)

-- PATTERN MATCHING
power :: Int -> Int -> Int
power x 0 = 1 
power x y = x * power x (y - 1)

-- QUESTION 8
sumFromTo :: Int -> Int -> Int
sumFromTo x y
  --  | x == y = x
    | x > y = 0
    | otherwise = x + sumFromTo (x + 1) y

-- QUESTION 9
gcd :: Int -> Int -> Int
gcd x y
    | x == y = x
    | otherwise = if x < y then gcd x (y - x) else gcd y (x - y)
   -- where
    --    abs a b = if (a - b) < 0 then -(a - b) else a - b

-- QUESTION 10
intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot x y 
    | x * x == y = x       -- <=    means can delete next line
    | (x + 1) * (x + 1) > y && x * x < y = x
    | otherwise = findRoot (x - 1) y 