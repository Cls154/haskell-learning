helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines


guessingGame :: Int -> IO ()
guessingGame secret =  do
    putStrLn "Enter a number: "
    guess <- getLine
    let guessInt = read guess :: Int
    if guessInt == secret
        then putStrLn "You guessed it!"
    else do
        if guessInt < secret
            then putStrLn "Too small"
            else putStrLn "Too large"
        guessingGame secret

testCities = ["London", "Paris", "New York", "Tokyo", "Sydney"]

addToTempsFile :: [String] -> IO ()
addToTempsFile [] = return ()
addToTempsFile (city : cities) = do
    putStr ("Enter the temperature in " ++ city ++ ": ")
    temp <- getLine
    let tempInt = read temp :: Int
    appendFile "temps.txt" city
    appendFile "temps.txt" (replicate (10 - length city) ' ')
    appendFile "temps.txt" (replicate tempInt '*' ++ "\n")
    addToTempsFile cities

createTempsFile :: IO ()
createTempsFile = do
    writeFile "temps.txt" ""
    addToTempsFile testCities


greeting :: IO ()
greeting = do
    putStr "What is your name? "
    name <- getLine
    putStrLn ("Hello " ++ name)

addTwoNumbers :: IO ()
addTwoNumbers = do
    putStr "Enter a number: "
    num1 <- getLine
    let numInt1 = read num1 :: Int
    putStr "Enter another number: "
    num2 <- getLine
    let numInt2 = read num2 :: Int
    putStrLn (show numInt1 ++ "+" ++ show numInt2 ++ "=" ++ show (numInt1 + numInt2))

copyFile :: IO ()
copyFile = do
    putStr "Enter the filename to copy including the extension: "
    filename <- getLine
    contents <- readFile filename
    writeFile ("copy_" ++ filename) contents
    putStrLn "file has been copied"

buildList :: [String] -> IO ()
buildList list = do
    putStr "Enter a line: "
    listData <- getLine
    let newList = list ++ [listData]
    putStrLn ("List is now " ++ (show newList))
    buildList newList

listBuilder :: IO ()
listBuilder = buildList []


readIntegers :: [Int] -> IO [Int]
readIntegers list = do
    putStr "Enter a number or 'end': "
    str <- getLine
    if str == "end"
        then return list
    else do
        let nInt = read str :: Int
        let newList = list ++ [nInt]
        print newList
        readIntegers newList

sumIntegers :: IO ()
sumIntegers = do
    list <- readIntegers []
    print (sum list)


addWord :: String -> [String] -> [String]
addWord str list = list ++ [str]

wordsToString :: [String] -> String
wordsToString [x] = x
wordsToString (str : strings) = str ++ "\n" ++ wordsToString strings

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength _ [] = []
wordsOfLength x (str : strings)
    | length str == x = str : wordsOfLength x strings
    | otherwise = wordsOfLength x strings


-- main :: IO ()
-- main = do
--     putStr "Enter the filename to copy including the extension: "
--     filename <- getLine
--     contents <- readFile filename
--     let list = read contents :: [String]
--     putStr "Add a fruit: "
--     newFruit <- getLine
--     let newList = addWord newFruit list
--     putStrLn (wordsToString newList)
--     writeFile filename (show newList)

-- displayMenu :: [String] -> IO ()
-- displayMenu list = do
--         -- list <- listIO 
--         putStrLn "Menu:"
--         putStrLn "(a) Add a fruit to the list."
--         putStrLn "(b) Display all fruits in the list"
--         putStrLn "(c) Display all fruits in the list of a given length"
--         putStrLn "(d) Exit"
--         putStrLn "Enter option here: "
--         option <- getLine
--         case option of 
--             "a" -> do
--                 putStr "Enter a fruit: "
--                 newFruit <- getLine
--                 let newList = addWord newFruit list 
--                 writeFile "fruits.txt" (show newList)
--                 -- displayMenu listIO

-- main :: IO ()
-- main = do
--     putStr "Enter the filename to copy including the extension: "
--     filename <- getLine
--     contents <- readFile filename
--     let list = read contents :: [String]
--     displayMenu list

displayMenu :: IO [String] -> IO ()
displayMenu listIO = do
    list <- listIO
    putStrLn "Menu:"
    putStrLn "  (a) Add a word to the list"
    putStrLn "  (b) Display all words"
    putStrLn "  (c) Display all words of a given length"
    putStrLn "  (d) Exit"
    putStr "Enter an option: "
    option <- getLine
    case option of
        "a" -> do
            putStr "Enter a word to add: "
            newWord <- getLine
            let newList = addWord newWord list
            writeFile "words.txt" (wordsToString newList)
            displayMenu (return newList)
        "b" -> do
            putStrLn "All words:"
            putStrLn (wordsToString list)
            displayMenu listIO
        "c" -> do
            putStr "Enter a length: "
            lengthStr <- getLine
            let lengthInt = read lengthStr :: Int
            let words = wordsOfLength lengthInt list
            putStrLn ("Words of length " ++ lengthStr ++ ":")
            putStrLn (wordsToString words)
            displayMenu listIO
        "d" -> putStrLn "Exiting..."
        _ -> do
            putStrLn "Invalid option."
            displayMenu listIO

main :: IO ()
main = do
    contents <- readFile "words.txt"
    let list = lines contents
    displayMenu (return list)




