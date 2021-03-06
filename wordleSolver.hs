
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad
import Data.List (sort)
{-# HLINT ignore "Eta reduce" #-}

type Dict = [String]

data Position = L1 | L2 | L3 | L4 | L5 deriving(Eq, Show, Enum, Bounded, Ord)

data Color = Black | Yellow | Green deriving (Eq, Show)

data Result = BL Char Position | YE Char Position | GR Char Position deriving (Show, Eq)

test a = a+2

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
    where   pom str ((BL letter postion):slt) = notElem letter str && pom str slt
            pom str ((YE letter postion):slt) = (str !! positionToInt postion) /= letter && elem letter str && pom str slt
            pom str ((GR letter postion):slt) = (str !! positionToInt postion) == letter && pom str slt
            pom str [] = True

filterDict :: [Result] -> Dict -> Dict
filterDict rslt dct = sort $ filter (\x -> filterStringByResult x rslt) dct

getResult :: IO String
getResult = do
        putStrLn "What was the result?"
        result <- getLine
        if length result == 5 && all (`elem` ['B','Y','G']) result then return result else do
            putStrLn "The result is formatted wrong. It has to be 5 letters long. \nB - black \nY - Yellow \nG - Green \n. Try to enter again. "
            getResult

getGuess :: IO String
getGuess = do
        putStrLn "What was your guess?"
        guess <- getLine
        if length guess == 5 && all (`elem` ['a'..'z'] ++ ['A'..'Z']) guess then return (formatGuess guess) else do
            putStrLn "The result is formatted wrong. It has to be 5 letters long. Try to enter again. "
            getGuess

updateDictionary :: Monad m => (String, String) -> [String] -> m Dict
updateDictionary (guess,result) dict = do
        let newDict = filterDict (parseGuessToResult guess (parseColors result)) dict
        return newDict


loop :: (Show t, Num t) => Dict -> (String, String) -> t -> IO ()
loop dictionary (guess,result) guessCount
                | null dictionary = putStrLn "The correct solution is not in my database"
                | length dictionary == 1 = putStrLn ("The only possibily is: " ++ head dictionary)
                | otherwise = do
                    putStrLn "All possible words are: \n"
                    print dictionary
                    putStrLn ("\n\nGuess no. " ++ show guessCount)
                    newGuess <- getGuess
                    newResult <- getResult
                    newDict <- updateDictionary(newGuess, newResult) dictionary
                    loop newDict (newGuess, newResult) (guessCount+1)

theEnd :: IO ()
theEnd = do
        putStrLn "Do you want to play again (press a) or quit (press q) ?"
        letter <- getLine
        case letter of  "a" -> main
                        "q" -> putStrLn "OK!"
                        _ -> theEnd

main :: IO()
main = do
    predct <- lines <$> readFile "words.txt"

    putStrLn "Guess no. 1"
    firstGuess <- getGuess
    firstResult <- getResult
    dict <- updateDictionary (firstGuess, firstResult) predct

    loop dict (firstGuess,firstResult) 2

    theEnd