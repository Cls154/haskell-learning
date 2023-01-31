circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink age1 age2 age3 = canDrink age1 && canDrink age2 && canDrink age3

-- QUESTION 1
timesTen :: Int -> Int
timesTen num = num * 10

-- QUESTION 2
sumThree :: Int -> Int -> Int -> Int
sumThree num1 num2 num3 = num1 + num2 + num3

-- QUESTION 3
areaOfCircle :: Float -> Float
areaOfCircle radius = pi * radius^2

-- QUESTION 4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder radius height = areaOfCircle radius * height

-- QUESTION 5
subtractTwoPoints :: Float -> Float -> Float
subtractTwoPoints x y = x - y;

distance2 :: Float -> Float -> Float -> Float -> Float
distance2 x1 y1 x2 y2 = sqrt ((y1-y2) ^ 2 + subtractTwoPoints x1 x2 ^ 2)

-- QUESTION 6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent numA numB numC = numA /= numB && numA /= numC && numC /= numB

-- QUESTION 7
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = mod a b == 0

-- QUESTION 8
isEven :: Int -> Bool
isEven num = divisibleBy num 2

-- QUESTION 9
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a + b + c) / 3

-- QUESTION 10
absolute :: Int -> Int
absolute num = if num >= 0 then -num else num