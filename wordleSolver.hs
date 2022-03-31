
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Data.Char (toLower)
{-# HLINT ignore "Eta reduce" #-}

type Dict = [String]

data Position = L1 | L2 | L3 | L4 | L5 deriving(Eq, Show, Enum, Bounded, Ord)

data Color = Black | Yellow | Green deriving (Eq, Show)

data Result = BL Char Position | YE Char Position | GR Char Position deriving (Show, Eq)

parseColors :: String -> Maybe [Color]
parseColors res = Just (pom res 5)
    where   pom ('B':es) n = Black:pom es (n-1)
            pom ('Y':es) n = Yellow:pom es (n-1)
            pom ('G':es) n = Green:pom es (n-1)
            pom _ 0 = []
            pom _ _ = error "zły format"
            
formatGuess :: String -> String
formatGuess str = toLower <$> take 5 str

parseGuessToResult :: String -> [Color] -> [Result]
parseGuessToResult word col = pom L1 word col
    where pom pst (w:ws) (c:cs) | c == Black = BL w pst:pom (succ pst) ws cs
                                | c == Yellow = YE w pst:pom (succ pst) ws cs
                                | c == Green = GR w pst:pom (succ pst) ws cs
          pom _ [] [] = []
          pom _ _ _ = error "<<< parseGuessToResult error >>>"




main :: IO()
main = do
    dct <- lines <$> readFile "words.txt"
    putStrLn ("3rd word in the file is "  ++ dct !! 2)