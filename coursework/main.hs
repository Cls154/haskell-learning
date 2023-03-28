import Data.List
--
-- MATHFUN
-- up2056835


--
-- Types (define City type here)
--
type Name = String
type North = Int
type East = Int

data Location = Location North East
  deriving(Show)

type Population = [Int]

data City = City Name Location Population
  deriving(Show)

testData :: [City]
testData = 
  [
    City "Amsterdam" (Location 52 5)  [1158, 1149, 1140, 1132],
    City "Athens"    (Location 38 24) [3153, 3153, 3154, 3156],
    City "Berlin"    (Location 53 13) [3567, 3562, 3557, 3552],
    City "Bucharest" (Location 44 26) [1794, 1803, 1812, 1821],
    City "London"    (Location 52  0) [9426, 9304, 9177, 9046],
    City "Madrid"    (Location 40  4) [6669, 6618, 6559, 6497],
    City "Paris"     (Location 49  2) [11079, 11017, 10958, 10901],
    City "Rome"      (Location 42 13) [4278, 4257, 4234, 4210],
    City "Vienna"    (Location 48 16) [1945, 1930, 1915, 1901],
    City "Warsaw"    (Location 52 21) [1790, 1783, 1776, 1768]
  ]

--
--  Your functional code goes here
--

-- **********************
-- ***** QUESTION 1 *****
-- **********************

-- Return a list of the names of all the cities

-- RUN TO TEST SOLUTION TO 1
-- cityNames testData

-- SOLUTION 1
cityNames :: [City] -> [String]
cityNames [] = []
cityNames (City name _ _ : cn) = name : cityNames cn



-- **********************
-- ***** QUESTION 2 *****
-- **********************

-- Given a city name and a number, return the population of the city that number of
-- years ago (or “no data” if no such record exists); the returned value should be a string
-- representing the population in millions to 3 decimal places with an ‘m’ suffix (e.g.
-- “5.123m”)

-- RUN TO TEST SOLUTION TO 2
-- citysPopulationOnYear testData "Berlin" 1

-- SOLUTION 2.1
-- yearsPopulation :: [City] -> Name -> Int -> String
-- yearsPopulation (City name location population : cn) inputName x
--   | name == inputName = if x >= 4 then "No Data" else show (fromIntegral (population !! x) / 1000) ++ "m"
--   | otherwise = yearsPopulation cn inputName x

-- SOLUTION 2.2
citysPopulationOnYear :: [City] -> Name -> Int -> String
citysPopulationOnYear (City name location population : cn) inputName x 
  | name == inputName = yearsPopulation (City name location population) x 
  | otherwise = citysPopulationOnYear cn inputName x
  where
    yearsPopulation (City _ _ population) x 
      | x >= 4 = "No Data"
      | otherwise = show (fromIntegral (population !! x) / 1000) ++ "m"



-- **********************
-- ***** QUESTION 3 *****
-- **********************

-- Return all the data as a single string which, when output using putStr, will display the
-- data formatted neatly into five columns giving the name, location (degrees N & E), this
-- year’s population and last year’s population. (The populations should be formatted as
-- for (ii).)

-- RUN TO TEST SOLUTION TO 3
-- putStr (citiesToString testData)

-- SOLUTION 3
citiesToString :: [City] -> String
citiesToString [] = []
citiesToString (City name (Location north east) population : cn) = name ++ " " ++ showLocation ++ " " ++ showPopulation ++ "\n" ++ citiesToString cn
  where
    showLocation = show north ++ "N" ++ " " ++ show east ++ "E"
    showPopulation = utilPop (head population) ++ " " ++ utilPop (head (tail population))
    utilPop pop = show (fromIntegral pop / 1000) ++ "m"



-- **********************
-- ***** QUESTION 4 *****
-- **********************

-- Update the data with a list of new (i.e. this year’s) population figures (one value for
-- each city); this should increase the length of each city’s population list so that what
-- was the current figure now becomes last year’s figure, and so on.

-- RUN TO TEST SOLUTION TO 4
-- putStr (citiesToString (addNewYearsPopulations testData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]))

-- SOLUTION 4
updatePopulations :: [City] -> [Int] -> [City]
updatePopulations [] _ = []
updatePopulations (City name location population : cn) (x : xs) = City name location (x : population) : updatePopulations cn xs



-- **********************
-- ***** QUESTION 5 *****
-- **********************

-- Add a new city (with a name and coordinates, and a population list of length equal to
-- those of the other cities) to the list, preserving its alphabetical ordering.

-- RUN TO TEST SOLUTION TO 5
-- putStr (citiesToString (addCity testData (City "Stockholm" (Location 59 18) [1657, 1633, 1608, 1583])))

-- SOLUTION 5
addCity :: [City] -> City -> [City]
addCity cities newCity = sortOn cityName (cities ++ [newCity])
  where
    cityName (City name _ _) = name



-- **********************
-- ***** QUESTION 6 *****
-- **********************

-- For a given city name, return a list of annual population growth figures (in thousands)
-- for that city (i.e., the result list should begin with the increase from last year’s figure to
-- this year’s; the second value should give the increase from two years ago to last year,
-- etc.). (The list will include negative values for shrinking populations.)

-- RUN TO TEST SOLUTION TO 6
-- annualGrowth testData "Athens"

-- SOLUTION 6
annualGrowth :: [City] -> Name -> [Int]
annualGrowth (City name location population : cn) inputName
  | name == inputName = [thousands (yrA - yrB) | (yrA, yrB) <- split population]
  | otherwise = annualGrowth cn inputName
    where
      split (fst : scd : rest) = if null rest then [(fst, scd)] else (fst, scd) : split (scd : rest)
      thousands x = x * 1000



-- **********************
-- ***** QUESTION 7 *****
-- **********************

-- Given a location and a number, return the name of the closest city with a population
-- bigger than the number, or “no city” if there are no such cities; use Pythagoras’ theorem
-- to calculate the distance between locations (i.e. assume the world is flat!)

-- RUN TO TEST SOLUTION TO 7
-- closestCity (citiesFromPopulation testData 5000) (Location 45 8)

-- SOLUTION 7
closestCity :: [City] -> Location -> String
closestCity [City name (Location north east) population] _ = name ++ " " ++ show north ++ "N" ++ " " ++ show east ++ "E" ++ " " ++ show (fromIntegral (head population) / 1000) ++ "M"
closestCity (City name1 (Location north1 east1) pop1 : City name2 (Location north2 east2) pop2 : cn) (Location x y) 
  | distance x y north1 east1 < distance x y north2 east2 = closestCity (City name1 (Location north1 east1) pop1 : cn) (Location x y)
  | otherwise = closestCity (City name2 (Location north2 east2) pop2 : cn) (Location x y)
    where
      distance x1 y1 x2 y2 = sqrt ((fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2)

citiesFromPopulation :: [City] -> Int -> [City]
citiesFromPopulation [] _ = []
citiesFromPopulation (City name location pop : cn) x
  | head pop > x = City name location pop : citiesFromPopulation cn x
  | otherwise = citiesFromPopulation cn x


--
--  Demo
-- *********************************
-- *********************************
-- ***** UNCOMMENT DONT FORGET *****
-- *********************************
-- *********************************
-- demo :: Int -> IO ()
-- demo 1 = -- output the names of all the cities
-- demo 2 = -- output the population of "Berlin" 1 year ago (i.e. last year)
-- demo 3 = putStrLn (citiesToString testData)
-- demo 4 = -- output the data (as for (iii)) after it has been updated with the
--          -- following new population figures (the first is for Amsterdam, etc.)
--          -- [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]
-- demo 5 = -- show the data (as for (iii)) after adding "Stockholm" (59N, 18E) 
--          -- with population figures [1657, 1633, 1608, 1583]
-- demo 6 = -- output a list of annual growth figures for "Athens"
-- demo 7 = -- output the nearest city to location (45N, 8E) with 
--          -- a population above 4m people
-- demo 8 = -- output the population map
demo :: Int -> IO ()
demo 1 = do
  print (cityNames testData)
demo 2 = do
  putStrLn (citysPopulationOnYear testData "Berlin" 1)
demo 3 = do
  putStr (citiesToString testData)
demo 4 = do
  putStr (citiesToString (updatePopulations testData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]))
demo 5 = do
  putStr (citiesToString (addCity testData (City "Stockholm" (Location 59 18) [1657, 1633, 1608, 1583])))
demo 6 = do
  print (annualGrowth testData "Athens")
demo 7 = do
  print (closestCity (citiesFromPopulation testData 5000) (Location 45 8))
demo _ = return ()

--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--