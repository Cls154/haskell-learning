import Data.List
import Text.Printf
import System.IO

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
  deriving(Show, Eq, Read)

type Population = [Int]

data City = City Name Location Population
  deriving(Show, Read)

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

getCityPopulation :: City -> Population
getCityPopulation (City _ _ population) = population

getCityLocation :: City -> Location
getCityLocation (City _ loc _) = loc

getNorth :: Location -> North
getNorth (Location north _) = north

getEast :: Location -> East
getEast (Location _ east) = east

-- ***** QUESTION 1 *****
allCityNames :: [City] -> [String]
allCityNames [] = []
allCityNames cities = map getCityName cities

-- ***** QUESTION 2 *****
getCityByName :: [City] -> Name -> City
getCityByName [] _ = City "" (Location 0 0) []
getCityByName (city:rest) name =
  if getCityName city == name
    then city
  else getCityByName rest name

getYearPopulation :: City -> Int -> String
getYearPopulation city year = 
  if length (getCityPopulation city) < year
    then "No Data"
  else show (fromIntegral (getCityPopulation city !! year) / 1000) ++ "m"

-- ***** QUESTION 3 *****
formatCity :: City -> String
formatCity (City name (Location north east) population) =
  printf "%-10s %-5s %-5s %-9s %s"
    name
    (show north ++ "N")
    (show east ++ "E")
    (show (fromIntegral (head population) / 1000) ++ "m")
    (show (fromIntegral (head (tail population)) / 1000) ++ "m")

citiesToString :: [City] -> String
citiesToString [] = []
citiesToString (city:rest) = formatCity city ++ "\n" ++ citiesToString rest

-- ***** QUESTION 4 *****
updatePopulations :: [City] -> [Int] -> [City]
updatePopulations [] _ = []
updatePopulations (City name location population : rest) (x : xs) = 
  if length (City name location population : rest) == length (x:xs)
    then City name location (x : population) : updatePopulations rest xs
  else []

-- ***** QUESTION 5 *****
addCity :: [City] -> City -> [City]
addCity cities newCity
  | getCityName newCity `elem` map getCityName cities = []
  | getCityLocation newCity `elem` map getCityLocation cities = []
  | length (getCityPopulation newCity) /= populationLength = []
  | otherwise = sortOn getCityName (cities ++ [newCity])
  where
    populationLength = round ((sum [fromIntegral (length pop) | (City _ _ pop) <- cities]) / fromIntegral (length cities))

-- ***** QUESTION 6 *****
annualGrowth :: [City] -> Name -> [Int]
annualGrowth (city : rest) inputName
  | getCityName city == inputName = [thousands (yrA - yrB) | (yrA, yrB) <- split (getCityPopulation city)]
  | otherwise = annualGrowth rest inputName
    where
      split (fst : scd : rest) = if null rest then [(fst, scd)] else (fst, scd) : split (scd : rest)
      thousands x = x * 1000

-- ***** QUESTION 7 *****
-- closestCity (citiesFromPopulation testData 5000) (Location 45 8)
-- ERROR CHECKS NEEDED HERE AS WELL FOR EXAMPLE WHEN THERE IS NO CLOSEST CITY WITH A POPULATION OF OVER SMTH
closestCity :: [City] -> Location -> String
closestCity [city] _ = getCityName city ++ " " ++ show (getNorth (getCityLocation city)) ++ "N" ++ " " ++ show (getEast (getCityLocation city)) ++ "E" ++ " " ++ show (fromIntegral (head (getCityPopulation city)) / 1000) ++ "M"
closestCity (cityA : cityB : cn) (Location x y) 
  | distance x y (getNorth (getCityLocation cityA)) (getEast (getCityLocation cityA)) < distance x y (getNorth (getCityLocation cityB)) (getEast (getCityLocation cityB)) = closestCity (cityA : cn) (Location x y)
  | otherwise = closestCity (cityB : cn) (Location x y)
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
  print (allCityNames testData)
demo 2 = do
  putStrLn (getYearPopulation (getCityByName testData "Berlin") 1)
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
displayMenu :: IO ()
displayMenu = do
  putStrLn "Select one of the following options:"
  putStrLn "1. Display All Citys"
  putStrLn "2. Return the population of a city x number of years ago"
  putStrLn "Press any other key to exit"

askForInput :: String -> IO String
askForInput prompt = do
  putStrLn prompt
  getLine

parseCities :: String -> [City]
parseCities contents = map read (lines contents)

main :: IO ()
main = do
  let filename = "cities.txt"
  contents <- readFile filename 
  let cities = parseCities contents
  main2 (return cities)

main2 :: IO [City] -> IO ()
main2 citiesIO = do
  cities <- citiesIO
  displayMenu
  option <- askForInput "Enter your Choice: "
  case option of
    "1" -> do 
      print (allCityNames cities)
      main2 (return cities)
    _ -> return ()
