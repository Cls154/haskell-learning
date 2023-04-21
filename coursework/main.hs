import Data.List
import Text.Printf
import Text.Read
import System.IO
import Graphics.Win32.Key (getCurrentKeyboardLayout)
import Control.Monad (forM)
import Language.Haskell.TH (location)

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

getYearPopulation :: Maybe City -> Int -> Maybe String
getYearPopulation Nothing _ = Nothing
getYearPopulation (Just city) year = 
  if length (getCityPopulation city) <= year
    then Nothing
  else Just (show (fromIntegral (getCityPopulation city !! year) / 1000) ++ "m")

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
closestCity :: [City] -> Location -> String
closestCity [] _ = "No closest city"
closestCity [city] _ = formatClosestCity city
closestCity (cityA : cityB : rest) location
  | distance location cityA  < distance location cityB = closestCity (cityA : rest) location
  | otherwise = closestCity (cityB : rest) location

distance :: Location -> City -> Float
distance (Location x y) city = pythagorus x y x2 y2
  where
    x2 = getNorth (getCityLocation city)
    y2 = getEast (getCityLocation city)

pythagorus :: Int -> Int -> Int -> Int -> Float 
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
  let population = getYearPopulation (getCityByName testData "Berlin") 1
  case population of
    Nothing -> putStrLn "No Data"
    Just population -> putStrLn population
demo 3 = do
  putStr (citiesToString testData)
demo 4 = do
  putStr (citiesToString (updatePopulations testData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]))
demo 5 = do
  putStr (citiesToString (addCity testData (City "Stockholm" (Location 59 18) [1657, 1633, 1608, 1583])))
demo 6 = do
  print (annualGrowth testData "Athens")
demo 7 = do
  putStrLn (closestCity (citiesFromPopulation testData 5000) (Location 45 8))
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
locationToScrPos (Location north east) = (xPos (fromIntegral east), yPos (fromIntegral north))
  where
    xPos east = round (3.076923077 * east)  
    yPos north = round ((north - 53) * (-1.724137931))

getCityData :: City -> CityMapData
getCityData city = CityMapData city (locationToScrPos (getCityLocation city))

allCityData :: [City] -> [CityMapData]
allCityData = map getCityData

drawCity :: CityMapData -> IO ()
drawCity (CityMapData city scrPos) = do
  writeAt scrPos ("+" ++ getCityName city ++ " " ++ show (fromIntegral (head (getCityPopulation city)) / 1000) ++ "m")

drawMap :: [CityMapData] -> IO ()
drawMap cities = do
  clearScreen
  mapM_ drawCity cities


--
-- Your user interface (and loading/saving) code goes here
--

-- Modified Core Functionality Function, specfically for the UI

updatePopulationsIO :: [City] -> [Int] -> Maybe [City]
updatePopulationsIO [] _ = Nothing
updatePopulationsIO (City name location population : rest) (x : xs) = 
  if length (City name location population : rest) == length (x : xs)
    then Just (City name location (x : population) : updatePopulations rest xs)
  else Nothing

addCityIO :: [City] -> City -> Maybe [City]
addCityIO cities newCity
  | getCityName newCity `elem` map getCityName cities = Nothing
  | getCityLocation newCity `elem` map getCityLocation cities = Nothing
  | length (getCityPopulation newCity) /= populationLength = Nothing
  | otherwise = Just (sortOn getCityName (cities ++ [newCity]))
  where
    populationLength = round ((sum [fromIntegral (length pop) | (City _ _ pop) <- cities]) / fromIntegral (length cities))

-- User Interface
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
      main2 filename (return cities)
    "2" -> do
      putStrLn "Enter a city name: "
      cityname <- getLine
      let city = getCityByName cities cityname
      putStrLn "Enter a year: "
      yearStr <- getLine
      let yearInt = read yearStr :: Int 
      let yearsPop = getYearPopulation city yearInt
      case yearsPop of
        Nothing -> putStrLn "No Data"
        Just yearsPop -> putStrLn yearsPop
      main2 filename (return cities)
    "3" -> do
      putStrLn (citiesToString cities)
      main2 filename (return cities)
    "4" -> do
      putStrLn "Enter a list of numbers to update populations: "
      list <- getLine
      let newPopFigures = mapM readMaybe (words list) :: Maybe [Int]
      case newPopFigures of 
        Nothing -> do
          putStrLn "Invalid populations value"
          main2 filename (return cities)
        Just newPopFigures -> do
          let newCities = updatePopulationsIO cities newPopFigures
          case newCities of
            Nothing -> do
              putStrLn "Unable to update populations"
              putStrLn ("You entered " ++ show (length newPopFigures) ++ " population figures")
              putStrLn ("You need to enter " ++ show (length cities) ++ " population figures")
              main2 filename (return cities)
            Just newCities -> do
              putStrLn "Populations updated"
              main2 filename (return newCities)
    "5" -> do
      putStrLn "Enter the city name: "
      newName <- getLine
      if newName `elem` [getCityName city | city <- cities]
        then do
          putStrLn "City already exists"
          main2 filename (return cities)
      else do
        putStrLn "Enter the cities location"
        putStrLn "North value: "
        newNorthStr <- getLine
        let newNorth = readMaybe newNorthStr :: Maybe Int
        case newNorth of
          Nothing -> do
            putStrLn "Invald north value"
            main2 filename (return cities)
          Just newNorth -> do
            putStrLn "East value: "
            newEastStr <- getLine
            let newEast = readMaybe newEastStr :: Maybe Int
            case newEast of
              Nothing -> do
                putStrLn "Invalid east value"
                main2 filename (return cities)
              Just newEast -> do
                putStrLn "Enter a list of the cities population figures: "
                newPopStr <- getLine
                let newPop = mapM readMaybe (words newPopStr) :: Maybe [Int]
                case newPop of
                  Nothing -> do
                    putStrLn "Invalid populations value"
                    main2 filename (return cities)
                  Just newPop -> do
                    let city = City newName (Location newNorth newEast) newPop
                    let newCities = addCityIO cities city 
                    case newCities of
                      Nothing -> do
                        putStrLn "Unable to add city"
                        main2 filename (return cities)
                      Just newCities -> do 
                        putStrLn "City added"
                        main2 filename (return newCities)
    "6" -> do
      putStrLn "Enter the city name: "
      newName <- getLine
      if newName `elem` [getCityName city | city <- cities]
        then do
          print (annualGrowth cities newName)
      else do
        putStrLn "City does not exists"
      main2 filename (return cities)
    "7" -> do
      putStrLn "Enter a population number (4 digits): "
      popStr <- getLine
      let pop = readMaybe popStr :: Maybe Int
      case pop of
        Nothing -> do
          putStrLn "Invalid input"
          main2 filename (return cities)
        Just pop -> do
          let citiesAbovePop = citiesFromPopulation cities pop 
          if null citiesAbovePop
            then do
              putStrLn "There are no cities above that population"
              main2 filename (return cities)
          else do 
            putStrLn "Enter the cities location"
            putStrLn "North value: "
            newNorthStr <- getLine
            let newNorth = readMaybe newNorthStr :: Maybe Int
            case newNorth of
              Nothing -> do
                putStrLn "Invald north value"
                main2 filename (return cities)
              Just newNorth -> do
                putStrLn "East value: "
                newEastStr <- getLine
                let newEast = readMaybe newEastStr :: Maybe Int
                case newEast of
                  Nothing -> do
                    putStrLn "Invalid east value"
                    main2 filename (return cities)
                  Just newEast -> do
                    putStrLn (closestCity citiesAbovePop (Location newNorth newEast))
      main2 filename (return cities)
    "8" -> do
      drawMap (allCityData cities)
      putStrLn ""
      main2 filename (return cities)
    _ -> writeFile filename (intercalate "\n" (map show cities))
