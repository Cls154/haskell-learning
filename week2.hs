import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import Data.Time.Format.ISO8601 (yearFormat)
-- heartMonitor :: Int -> Int -> String
-- heartMonitor age bpm
--     | age < 20 && bpm > 170 = "High Heart Rate for 0-20"
--     | age < 40 && bpm > 155 = "High Heart Rate for 20-40"
--     | age < 60 && bpm > 140 = "High Heart Rate for 40-60"
--     | age < 80 && bpm > 130 = "High Heart Rate for 60-80"
--     | age > 80 && bpm > 100 = "High Heart Rate for 80+!"

heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for +80!"
    | age > 60 && age <= 80 && bpm > 130 = "High heart rate for 60-80!"
    | age > 40 && age <= 60 && bpm > 140 = "High heart rate for 40-60!"
    | age > 20 && age <= 40 && bpm > 155 = "High heart rate for 20-40!"
    | age >= 0 && age <= 20 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal Heart Rate"

-- pizzaCalories :: Int -> String -> Float
-- pizzaCalories diameter toppings
--     | toppings == "pepperoni" = (11.5 + 6) * pi * (fromIntegral diameter / 2) ^ 2
--     | toppings == "tuna" = (11.5 + 4) * pi * (fromIntegral diameter / 2) ^ 2
--     | toppings == "veggie" = (11.5 + 2.5) * pi * (fromIntegral diameter / 2) ^ 2
--     | otherwise = 11.5 * pi * (fromIntegral diameter / 2) ^ 2

-- pizzaCalories :: Int -> String -> Float
-- pizzaCalories diameter toppings
--     | toppings == "pepperoni" = (11.5 + 6) * area
--     | toppings == "tuna" = (11.5 + 4) * area
--     | toppings == "veggie" = (11.5 + 2.5) * area
--     | otherwise = 11.5 * area
-- where
-- area = pi * (fromIntegral diameter / 2) ^ 2

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
    where
    area = pi * (fromIntegral diameter / 2) ^ 2
    toppingCalories
        | toppings == "pepperoni" = 6
        | toppings == "tuna" = 4
        | toppings == "veggie" = 2.5
        | otherwise = 0

-- QUESTION 1
absolute :: Int -> Int
absolute x
    | x <= 0 = -x
    | otherwise = x

-- QUESTION 2
sign :: Int -> Int
sign x
    | x < 0 = -1
    | x > 0 = 1
    | otherwise = 0

-- QUESTION 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | otherwise = 0

-- QUESTION 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagonalLength x + diagonalLength y + diagonalLength z
    where
        diagonalLength sideLength = sqrt (2 * sideLength ^ 2) 

-- QUESTION 5
taxiFares :: Int -> Float
taxiFares x = 2.20 + addKm x
  where
    addKm km
      | km <= 10 = fromIntegral km * 0.5
      | otherwise = 5 + ((fromIntegral km - 10) * 0.3)

-- QUESTION 6
-- howManyAboveAverage :: Int -> Int -> Int -> Int
-- howManyAboveAverage x y z
--   | fromIntegral x <= calcAvg x y z && fromIntegral y <= calcAvg x y z && fromIntegral z <= calcAvg x y z = 0
--   | fromIntegral x > calcAvg x y z && fromIntegral y > calcAvg x y z && fromIntegral z > calcAvg x y z = 3
--   | (fromIntegral x > calcAvg x y z && fromIntegral y > calcAvg x y z  && fromIntegral z < calcAvg x y z) 
--     || (fromIntegral x > calcAvg x y z && fromIntegral z > calcAvg x y z  && fromIntegral y < calcAvg x y z) 
--     || (fromIntegral y > calcAvg x y z && fromIntegral z > calcAvg x y z  && fromIntegral x < calcAvg x y z) = 2
--   | otherwise = 1
--   where
--     calcAvg x y z = fromIntegral (x + y + z) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | x == y && y == z = 3
  | f x > avg && f y > avg || f x > avg && f z > avg || f y > avg && f z > avg = 2 
  | otherwise = 1
  where
    avg = fromIntegral (x + y + z) / 3
    f a = fromIntegral a

-- QUESTION 7
validDate :: Int -> Int -> Bool
validDate day month = day <= monthDays
  where
    monthDays
      | month == 2 = 28
      | month == 4 = 30
      | month == 6 = 30
      | month == 9 = 30
      | month == 11 = 30
      | otheriwse = 31

-- QUESTION 8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
      | month ==  2 = if mod year 4 == 0 then 29 else 28
      | month < 8 = if even month then 30 else 31
      | otherwise = if even month then 31 else 30

-- QUESTION 9
{-
sumThree 3 5 7 
takes 3 and adds 5 then adds 7 and return 15

sumThree 8 (1 + 3) 2
takes 8 and adds (1 + 3) then add 2 and returns 14
-}

-- QUESTION 10
{-
threeDifferent 1 4 2
compares 1 to 4 this is false so it moves on and
compares 1 to 2 this is false so it moves on and
compares 4 to 2 this is false, all comparisions are false so return true stating all values are different

threeDifferent 1 7 7
compares 1 to 7 this is false so it moves on and
compares 1 to 7 this is false so it moves on and
compares 7 to 7 this is true, because one comparision is true returns false stating not all values are different
-}

-- QUESTION 11
{-
howManyEqual 3 5 2
compares 3 to 5 to see if they are equal they are not so it moves on
compares 3 to 5 to see if they are equal is it not so it then 
compares 3 to 2 to see if they are equal is it not so it then 
compares 5 to 2 to see if they are equal they are not
none of the values are equal to one another to it returns 0

howManyEqual 5 2 5
compares 5 to 2 to see if they are equal is it not so it then
compares 5 to 2 to see if they are equal is it not so it then 
compares 5 to 5 this is true so it returns 2
-}