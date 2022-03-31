
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad
{-# HLINT ignore "Eta reduce" #-}

type Dict = [String]

data Position = L1 | L2 | L3 | L4 | L5 deriving(Eq, Show, Enum, Bounded, Ord)

data Color = Black | Yellow | Green deriving (Eq, Show)

data Result = BL Char Position | YE Char Position | GR Char Position deriving (Show, Eq)

parseColors :: String -> Maybe [Color]
parseColors res = traverse (\c -> case c of 'B' -> Just Black
                                            'Y' -> Just Yellow
                                            'G' -> Just Green
                                            _ -> Nothing ) res
formatGuess :: String -> String
formatGuess str = toLower <$> take 5 str

parseGuessToResult :: String -> Maybe [Color] -> [Result]
parseGuessToResult word col = pom L1 word (fromJust col)
    where pom pst (w:ws) (c:cs) | c == Black = BL w pst:pom (succ pst) ws cs
                                | c == Yellow = YE w pst:pom (succ pst) ws cs
                                | c == Green = GR w pst:pom (succ pst) ws cs
          pom _ [] [] = []
          pom _ _ _ = error "<<< parseGuessToResult error >>>"

positionToInt :: Position -> Int
positionToInt pos = case pos of L1 -> 0
                                L2 -> 1
                                L3 -> 2
                                L4 -> 3
                                L5 -> 4

filterStringByResult :: String -> [Result] -> Bool
filterStringByResult str rslt = pom str rslt
    where   pom str ((BL letter postion):slt) = (str !! positionToInt postion) /= letter && pom str slt
            pom str ((YE letter postion):slt) = (str !! positionToInt postion) /= letter && elem letter str && pom str slt
            pom str ((GR letter postion):slt) = (str !! positionToInt postion) == letter && pom str slt
            pom str [] = True

filterDict :: [Result] -> Dict -> Dict
filterDict rslt dct = filter (\x -> filterStringByResult x rslt) dct






searchLoop dct = pom dct
    where   pom [oneWord] = oneWord
            pom dct = filterDict



main :: IO()
main = do
    predct <- lines <$> readFile "words.txt"

    let getResult = do
        putStrLn "What was the result?"
        result <- getLine
        if length result == 5 && all (`elem` ['B','Y','G']) result then return result else do 
            putStrLn "The result is formatted wrong. It has to be 5 letters long. \nB - black \nY - Yellow \nG - Green \n. Try to enter again. "
            getResult

    let getGuess = do
        putStrLn "What was your guess?"
        guess <- getLine
        if length guess == 5 && all (`elem` ['a'..'z']) guess then return guess else do 
            putStrLn "The result is formatted wrong. It has to be 5 letters long, all lowercase. Try to enter again. "
            getGuess

    let loop dictionary (guess,result)
                | null dictionary = putStrLn "The correct solution is not in my database"
                | length dictionary == 1 = putStrLn ("The only possibily is: " ++ head dictionary)
                | otherwise = do
                    newGuess <- getGuess
                    newResult <- getResult
                    let newdct = filterDict (parseGuessToResult newGuess (parseColors newResult)) dictionary
                    putStrLn "All possible words are: \n"
                    print newdct
                    loop newdct (newGuess, newResult)

    let updateDictionary (guess,result) dict = do 
        let newDict = filterDict (parseGuessToResult guess (parseColors result)) dict
        putStrLn "All possible words are: \n"
        print newDict
        return newDict


    firstGuess <- getGuess
    firstResult <- getResult
    dict <- updateDictionary (firstGuess, firstResult) predct

    loop dict (firstGuess,firstResult)

    let theEnd = do
        putStrLn "Do you want to play again (press a) or quit (press q) ?"
        getLine  >>= \letter ->
                    case letter of  "a" -> main
                                    "q" -> putStrLn "OK!"
                                    _ -> theEnd
    theEnd
