import Data.List
import Graphics.Win32 (restoreDC)
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

getCityName :: City -> Name
getCityName (City name _ _) = name

-- ***** QUESTION 1 *****
-- cityNames testData
cityNames :: [City] -> [String]
cityNames [] = []
cityNames (City name _ _ : cn) = name : cityNames cn

allCityNames :: [City] -> [String]
allCityNames [] = []
allCityNames cities = map getCityName cities


-- ***** QUESTION 2 *****
-- citysPopulationOnYear testData "Berlin" 1
citysPopulationOnYear :: [City] -> Name -> Int -> String
citysPopulationOnYear (City name location population : cn) inputName x 
  | name == inputName = yearsPopulation (City name location population) x 
  | otherwise = citysPopulationOnYear cn inputName x
  where
    yearsPopulation (City _ _ population) x 
      | x >= 4 = "No Data"
      | otherwise = show (fromIntegral (population !! x) / 1000) ++ "m"

getCityByName :: [City] -> Name -> [City]
getCityByName [] _ = []
getCityByName (city:rest) name =
  if getCityName city == name
    then [city]
  else getCityByName rest name

getCityPopulation :: City -> Population
getCityPopulation (City _ _ population) = population

getYearPopulation :: [City] -> Int -> String
getYearPopulation (city:rest) year = 
  if length (getCityPopulation city) < year
    then "No Data"
  else show (fromIntegral (getCityPopulation city !! year) / 1000) ++ "m"
-- getDiaryFromTitleDate :: [Diary] -> Title -> Date -> Maybe Diary
-- getDiaryFromTitleDate [] _ _ = Nothing 
-- getDiaryFromTitleDate (Diary title des rev date : rest) inTitle inDate =
--   if inTitle == title && inDate == date 
--     then Just (Diary title des rev date)
--   else getDiaryFromTitleDate rest inTitle inDate

-- ***** QUESTION 3 *****
-- putStr (citiesToString testData)
citiesToString :: [City] -> String
citiesToString [] = []
citiesToString (City name (Location north east) population : cn) = name ++ " " ++ showLocation ++ " " ++ showPopulation ++ "\n" ++ citiesToString cn
  where
    showLocation = show north ++ "N" ++ " " ++ show east ++ "E"
    showPopulation = utilPop (head population) ++ " " ++ utilPop (head (tail population))
    utilPop pop = show (fromIntegral pop / 1000) ++ "m"

-- ***** QUESTION 4 *****
-- putStr (citiesToString (addNewYearsPopulations testData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]))
updatePopulations :: [City] -> [Int] -> [City]
updatePopulations [] _ = []
updatePopulations (City name location population : cn) (x : xs) = City name location (x : population) : updatePopulations cn xs

-- ***** QUESTION 5 *****
-- putStr (citiesToString (addCity testData (City "Stockholm" (Location 59 18) [1657, 1633, 1608, 1583])))
addCity :: [City] -> City -> [City]
addCity cities newCity = sortOn cityName (cities ++ [newCity])
  where
    cityName (City name _ _) = name

-- ***** QUESTION 6 *****
-- annualGrowth testData "Athens"
annualGrowth :: [City] -> Name -> [Int]
annualGrowth (City name location population : cn) inputName
  | name == inputName = [thousands (yrA - yrB) | (yrA, yrB) <- split population]
  | otherwise = annualGrowth cn inputName
    where
      split (fst : scd : rest) = if null rest then [(fst, scd)] else (fst, scd) : split (scd : rest)
      thousands x = x * 1000

-- ***** QUESTION 7 *****
-- closestCity (citiesFromPopulation testData 5000) (Location 45 8)
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



--  Demo
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