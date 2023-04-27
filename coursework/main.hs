import Data.List
import Text.Printf
import Text.Read
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
getCityByName :: [City] -> Name -> Maybe City
getCityByName [] _ = Nothing
getCityByName (city:rest) name =
  if getCityName city == name
    then Just city
  else getCityByName rest name

getYearPopulation :: Maybe City -> Int -> String
getYearPopulation Nothing _ = "No Data"
getYearPopulation (Just city) year = 
  if length (getCityPopulation city) <= year
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
  if length (City name location population : rest) == length (x : xs)
    then City name location (x : population) : updatePopulations rest xs
  else []

-- ***** QUESTION 5 *****
addCity :: [City] -> City -> [City]
addCity cities newCity = 
  if checkIfCityExists cities newCity
    then []
  else sortOn getCityName (cities ++ [newCity])

checkIfCityExists :: [City] -> City -> Bool
checkIfCityExists cities newCity
  | getCityName newCity `elem` map getCityName cities = True
  | getCityLocation newCity `elem` map getCityLocation cities = True
  | length (getCityPopulation newCity) /= meanPopulationLength cities = True
  | otherwise = False 

meanPopulationLength :: [City] -> Int
meanPopulationLength cities = round mean
  where
    sumOfTerms = sum [length pop | (City _ _ pop) <- cities]
    numOfTerms = length cities
    mean = fromIntegral sumOfTerms / fromIntegral numOfTerms

-- ***** QUESTION 6 *****
annualGrowth :: [City] -> Name -> [Int]
annualGrowth (city : rest) inputName =
  if getCityName city == inputName 
    then [yrA - yrB | (yrA, yrB) <- tuplePackArray (getCityPopulation city)]
  else annualGrowth rest inputName

tuplePackArray :: [a] -> [(a, a)]
tuplePackArray (fst : scd : rest) =
  if null rest 
    then [(fst, scd)]
  else (fst, scd) : tuplePackArray (scd : rest)

-- ***** QUESTION 7 *****
closestCity :: [City] -> Location -> String
closestCity [] _ = "No closest city"
closestCity [city] _ = formatClosestCity city
closestCity (cityA : cityB : rest) location
  | distance location cityA  < distance location cityB = closestCity (cityA : rest) location
  | otherwise = closestCity (cityB : rest) location

distance :: Location -> City -> Float
distance (Location x1 y1) city = pythagorus x1 y1 x2 y2
  where
    x2 = getNorth (getCityLocation city)
    y2 = getEast (getCityLocation city)
    pythagorus x1 y1 x2 y2 = sqrt ((fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2)

formatClosestCity :: City -> String
formatClosestCity (City name (Location north east) population) =
  printf "%-10s %-5s %-5s %s"
    name
    (show north ++ "N")
    (show east ++ "E")
    (show (fromIntegral (head population) / 1000) ++ "m")

citiesFromPopulation :: [City] -> Int -> [City]
citiesFromPopulation [] _ = []
citiesFromPopulation (city : rest) x
  | head (getCityPopulation city) > x = city : citiesFromPopulation rest x
  | otherwise = citiesFromPopulation rest x



--  Demo
demo :: Int -> IO ()
demo 1 = do
  print (allCityNames testData)
demo 2 = do
  let cityName = "Berlin"
  let year = 1
  putStrLn (getYearPopulation (getCityByName testData cityName) year)
demo 3 = do
  putStr (citiesToString testData)
demo 4 = do
  let populationFigs = [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]
  putStr (citiesToString (updatePopulations testData populationFigs))
demo 5 = do
  let newCity = City "Stockholm" (Location 59 18) [1657, 1633, 1608, 1583]
  putStr (citiesToString (addCity testData newCity))
demo 6 = do
  let cityName = "Athens"
  print (annualGrowth testData cityName)
demo 7 = do
  let population = 4000
  let location = Location 45 8
  putStrLn (closestCity (citiesFromPopulation testData population) location)
demo 8 = do
  drawMap (allCityData testData)
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

data CityMapData = CityMapData City ScreenPosition

locationToScrPos :: Location -> ScreenPosition
locationToScrPos (Location north east) = (xPos east, yPos north)
  where
    maxLongitude = 35
    minLongitude = -15
    maxLatitude = 66
    minLatitude = 24
    screenWidth = 80
    screenHeight = 50
    longitudePerPixel = fromIntegral screenWidth / fromIntegral (maxLongitude - minLongitude)
    latitudePerPixel = fromIntegral screenHeight / fromIntegral (maxLatitude - minLatitude)
    xPos east = (east - minLongitude) * round longitudePerPixel
    yPos north = screenHeight - ((north - minLatitude) * round latitudePerPixel)

createCityData :: City -> CityMapData
createCityData city = CityMapData city (locationToScrPos (getCityLocation city))

allCityData :: [City] -> [CityMapData]
allCityData = map createCityData

drawCity :: CityMapData -> IO ()
drawCity (CityMapData city (x, y)) = do
  writeAt (x, y) "+"
  writeAt (x, y + 1) (getCityName city)
  writeAt (x, y + 2) (show (fromIntegral (head (getCityPopulation city)) / 1000) ++ "m")

drawMap :: [CityMapData] -> IO ()
drawMap cities = do
  clearScreen
  mapM_ drawCity cities


--
-- Your user interface (and loading/saving) code goes here
--

displayMenu :: IO ()
displayMenu = do
  putStrLn ""
  putStrLn "Select one of the following options:"
  putStrLn "1. Display all citys"
  putStrLn "2. Return the population of a city x number of years ago"
  putStrLn "3. Display all city data"
  putStrLn "4. Update all city populations"
  putStrLn "5. Add a new city"
  putStrLn "6. Check a cities annual growth"
  putStrLn "7. Find closest city above a certain population"
  putStrLn "8. Draw a map of all the cities"
  putStrLn "Press any other key to save and exit"

askForInput :: String -> IO String
askForInput prompt = do
  putStrLn prompt
  getLine

askForCityNameCheckIfExists :: [City] -> IO String
askForCityNameCheckIfExists cities = do
  newName <- askForInput "Enter a city name: "
  if newName `notElem` map getCityName cities
    then do 
      putStrLn "City does not exist"
      return ""
  else do
    return newName

askForCityNameCheckNotExists :: [City] -> IO String
askForCityNameCheckNotExists cities = do
  newName <- askForInput "Enter a city name: "
  if newName `elem` [getCityName city | city <- cities]
    then do
      putStrLn "City already exists"
      return ""
  else do
    return newName

askForCityLocation :: [City] -> IO Location
askForCityLocation cities = do 
  putStrLn "Enter the cities location"
  newNorthStr <- askForInput "North value: "
  let newNorth = readMaybe newNorthStr :: Maybe Int
  case newNorth of
    Nothing -> do
      putStrLn "Invald north value"
      return (Location 99999 99999)
    Just newNorth -> do 
      newEastStr <- askForInput "East value: "
      let newEast = readMaybe newEastStr :: Maybe Int
      case newEast of
        Nothing -> do
          putStrLn "Invalid east value"
          return (Location 99999 99999)
        Just newEast -> do
          return (Location newNorth newEast)

askForCityPopulation :: [City] -> Int -> IO Population
askForCityPopulation cities x = do 
  newPopStr <- askForInput "Enter a list of the cities population figures: "
  let newPop = mapM readMaybe (words newPopStr) :: Maybe [Int]
  case newPop of
    Nothing -> do
      putStrLn "Invalid populations value"
      return []
    Just newPop -> do
      if length newPop /= x
        then do
          putStrLn "Invalid population length"
          putStrLn ("You entered " ++ show (length newPop) ++ " population figures")
          putStrLn ("You need to enter " ++ show x ++ " population figures")
          return []
      else return newPop

continue :: FilePath -> [City] -> IO ()
continue filename cities = do
  next <- askForInput "Press any \"m\" to continue"
  if next == "m"
    then do
      clearScreen
      main2 filename (return cities)
  else continue filename cities

main :: IO ()
main = do
  let filename = "cities.txt"
  contents <- readFile filename 
  let cities = map read (lines contents)
  main2 filename (return cities)

main2 :: FilePath -> IO [City] -> IO ()
main2 filename citiesIO = do
  cities <- citiesIO
  displayMenu
  option <- askForInput "Enter your Choice: "
  case option of
    "1" -> do 
      print (allCityNames cities)
      continue filename cities
    "2" -> do
      name <- askForCityNameCheckIfExists cities
      if null name then continue filename cities else do
        year <- askForInput "Enter a year: "
        putStrLn (getYearPopulation (getCityByName cities name) (read year))
        print (allCityNames cities)
        continue filename cities
    "3" -> do
      putStrLn (citiesToString cities)
      continue filename cities
    "4" -> do
      pop <- askForCityPopulation cities (length cities)
      if null pop then continue filename cities else do
        let newCities = updatePopulations cities pop
        putStrLn "Populations updated"
        continue filename newCities
    "5" -> do
      name <- askForCityNameCheckNotExists cities 
      if null name then continue filename cities else do
        loc <- askForCityLocation cities 
        if loc == Location 99999 99999 then continue filename cities else do 
          pop <- askForCityPopulation cities (meanPopulationLength cities)
          if null pop then continue filename cities else do 
            let newCity = City name loc pop
            let newCities = addCity cities newCity
            putStrLn "New city added"
            continue filename newCities
    "6" -> do
      name <- askForCityNameCheckIfExists cities
      if null name then continue filename cities else do
        print (annualGrowth cities name)
        print (allCityNames cities)
        continue filename cities
    "7" -> do
      popStr <- askForInput "Enter a population number (4 digits): "
      let pop = readMaybe popStr :: Maybe Int
      case pop of
        Nothing -> do
          putStrLn "Invalid input"
          continue filename cities
        Just pop -> do
          let citiesAbovePop = citiesFromPopulation cities pop 
          if null citiesAbovePop
            then do
              putStrLn "There are no cities above that population"
              continue filename cities
          else do 
              loc <- askForCityLocation cities 
              if loc == Location 99999 99999 then continue filename cities else do
                putStrLn (closestCity citiesAbovePop loc)
                continue filename cities
    "8" -> do
      clearScreen
      drawMap (allCityData cities)
      continue filename cities
    _ -> writeFile filename (intercalate "\n" (map show cities))
