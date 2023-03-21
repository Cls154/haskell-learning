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

