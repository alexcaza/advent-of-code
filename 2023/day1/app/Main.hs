module Main where

import System.IO
import Data.Char

getLines :: IO [String]
getLines = do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        return list

onlyInts :: [Char] -> [Char]
onlyInts str = filter (\c -> isDigit c) str

getFirstAndLastNum :: [Char] -> String
-- TODO: Figure out how to properly pattern match here and handle empty
-- list cases
getFirstAndLastNum c = head c : last c : []

main :: IO ()
main = do
    list <- getLines
    let res = map (read . getFirstAndLastNum . onlyInts) list 
        total = sum res
    print total

-- First part output: 55090
