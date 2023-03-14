
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

type Make = String
type Model = String
type HorsePower = Int
type Price = Float

data EngineType = Petrol | Diesel | Electric
     deriving (Show, Eq)

data Engine = Engine EngineType HorsePower
     deriving (Show)

data CarName = CName Make Model
     deriving (Show)

data Car = Car CarName Engine Price
     deriving (Show)

testCars :: [Car]
testCars =
     [ 
          Car (CName "Ford" "Fiesta") (Engine Petrol 55) 10000.0,
          Car (CName "Ford" "Focus") (Engine Diesel 85) 15000.0,
          Car (CName "Vauxhall" "Corsa") (Engine Petrol 55) 8000.0,
          Car (CName "Vauxhall" "Astra") (Engine Diesel 81) 12000.0,
          Car (CName "Vauxhall" "Astra") (Engine Diesel 96) 14000.0,
          Car (CName "VolksWagen" "Golf") (Engine Electric 81) 20000.0
     ]

totalPrice :: [Car] -> Float
totalPrice [] = 0
totalPrice (Car _ _ price : cs) = price + totalPrice cs

filterByMake :: String -> [Car] -> [Car]
-- filterByMake manufacturer cs = [c | c <- cs, getMake c == manufacturer]
--      where
--           getMake (Car (CName make _) _ _) = make
filterByMake manufacturer = filter (\(Car (CName make _) _ _) -> make == manufacturer)

updatePriceAt :: Int -> Float -> [Car] -> [Car]
updatePriceAt _ _ [] = []
updatePriceAt 0 amount (c : cs) = updatePrice amount c : cs
updatePriceAt index amount (c : cs) = c : updatePriceAt (index - 1) amount cs

updatePrice :: Float -> Car -> Car
updatePrice newPrice (Car name engine _) = Car name engine newPrice


data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec 
     deriving (Show, Eq)
data Season = Winter | Summer | Autumn | Spring
     deriving (Show, Eq)

season :: Month -> Season
season month 
     | month `elem` [Dec, Jan, Feb] = Winter
     | month `elem` [Mar, Apr, May] = Spring
     | month `elem` [Jun, Jul, Aug] = Summer
     | otherwise = Autumn

numberOfDays :: Month -> Int -> Int
numberOfDays month year
     | month == Feb && mod year 4 == 0 = 29
     | month == Feb = 28
     | month `elem` [Apr, Jun, Sep, Nov] = 30
     | otherwise = 31

type XCoord = Float
type YCoord = Float

data Point = Point XCoord YCoord
     deriving(Show)

data PositionedShape = PositionedCircle Float Point | PositionedRectangle Float Float Point
     deriving(Show)

myCirc :: PositionedShape
myCirc = PositionedCircle 5.0 (Point 10.0 20.0)

myRect :: PositionedShape
myRect = PositionedRectangle 10.0 20.0 (Point 10.0 20.0)

move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedCircle r (Point x y)) dx dy = PositionedCircle r (Point (x + dx) (y + dy))
move (PositionedRectangle w h (Point x y)) dx dy = PositionedRectangle w h (Point (x + dx) (y + dy))

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ left right) = 1 + numberOfNodes left + numberOfNodes right

isMember :: Int -> Tree -> Bool
isMember x (Node n left right)
     | x == n = True
     | (isMember x left) (isMember x right)
     | otherwise = False 
