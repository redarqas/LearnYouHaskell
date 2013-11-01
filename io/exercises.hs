import Data.Char
import Control.Applicative
import Data.List
import Control.Monad
import System.IO


hello = do 
    putStrLn "Hello World"

helloyou = do
    putStrLn "What's your first name ? " 
    fname <- map toUpper <$> getLine
    putStrLn "What's your last name ?"
    lname <- map toUpper <$> getLine
    putStrLn $ "Hello " ++ fname ++ " " ++ lname

reverseWords :: String -> String
reverseWords = unwords . map reverse . words 

progReverse = do
    line <- getLine
    if null line 
        then return ()
        else do
            putStrLn $ reverseWords line
            progReverse

progWhen = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        progWhen

progSequence = do
    l <- sequence [getLine, getLine, getLine]
    print l

progMapPrint = sequence $ map print [1..4]

progMapPrint2 = mapM print [1..4]

progMapPrint2_ = mapM_ print [1..4]

progForever = forever $ do
    putStrLn "give me input : "
    l <- map toUpper <$> getLine
    print l


progForM = do
    colors <- forM [1,2] (\a -> do 
       putStrLn "your first color : "
       c <- getLine
       return c)
    putStrLn "Your two colors are : " 
    mapM_ print colors

progContentsToUpper = do
    contents <- getContents 
    putStrLn $ contentsToUpper contents

contentsToUpper :: String -> String
contentsToUpper  = map toUpper


shortLinesOnly :: Int -> String -> String 
shortLinesOnly taille contents = unlines $ filter(\l -> length l < taille) $ lines contents

progFilterContents = do
    shorts <- shortLinesOnly 7 <$> getContents
    putStrLn shorts


progInteract = interact $ shortLinesOnly 7

ispalindrome :: String -> String
ispalindrome s = if s == reverse s then s ++ " is a palindrome" else s ++ " is not a palindrome" 


progPalindrome = interact $ unlines . (map ispalindrome) . lines 

progOpenFile = do 
    h <- openFile "test.txt" ReadMode
    contents <- hGetContents h
    putStrLn $  unlines . (map ispalindrome) . lines $ contents
    hClose h

progWithFile = do
    withFile "test.txt" ReadMode (\h -> do
        contents <- hGetContents h
        putStrLn $  unlines . (map ispalindrome) . lines $ contents
        )
progReadFile = do 
    contents <- readFile "test.txt"
    putStrLn contents

progWriteFile = writeFile "write.txt" "test write file"

progSpeCopy = do
    from <- readFile "test.txt"
    writeFile "testSpeCopy.txt" $ reverseEachWord from  

reverseEachWord :: String -> String 
reverseEachWord = unlines . map(unwords . map(map toUpper . reverse) . words ) . lines 

main = do
    withFile "test.txt" ReadMode (\h -> do
        hSetBuffering h  $ BlockBuffering (Just 2048)
        contents <- hGetContents h 
        putStrLn contents)























