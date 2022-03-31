
type Dict = [String]

data Letter = L1 | L2 | L3 | L4 | L5 deriving(Eq, Show)

data Color = Grey | Yellow | Green

main :: IO()
main = do
    dct <- lines <$> readFile "words.txt"
    putStrLn ("3rd word in the file is "  ++ dct !! 2)