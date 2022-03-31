
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
import Data.Char (toLower)
import Data.Maybe (fromJust)
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

main :: IO()
main = do
    dct <- lines <$> readFile "words.txt"
    putStrLn ("3rd word in the file is "  ++ dct !! 2)
     