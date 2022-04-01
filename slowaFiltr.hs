{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use print" #-}

import Lang
import Data.List (intercalate)
import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr, putStrLn)
import Data.Text as T 
import Data.Text.IO as TIO

{-# HLINT ignore "Eta reduce" #-}


filterDict list = Prelude.filter (\x -> T.length x == 5) list

main :: IO ()
main = do
    predct <- T.lines <$> TIO.readFile "slowa5.txt"
    TIO.putStrLn $ T.pack "read"
    print (predct !! 12)
    let fiveLetterWords = filterDict predct
    TIO.putStrLn (T.pack "KŁÓDKA")
    Prelude.putStrLn (show "łódka")
    TIO.writeFile "slowa52.txt" (T.intercalate (T.pack "\n") fiveLetterWords)