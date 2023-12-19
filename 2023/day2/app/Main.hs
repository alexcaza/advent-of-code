module Main where

import System.IO

-- Need to get the game ids from the line
-- Need to get the turns and the number of cubes by color per turn
-- Need to check and see if the number of cubes break the max number of cubes
--      OR if a single color breaks the max for that color
-- Need to throw out impossible games and sum the ids of the possible games

maxRed :: Int
maxRed = 12

maxGreen :: Int
maxGreen = 13

maxBlue :: Int
maxBlue = 14

maxCubes = map

getLines :: IO [String]
getLines = do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        return list

getGameId :: String -> Int
getGameId s = case dropWhile (==':') s of
                    "" -> 0
                    s' -> w : getGameId s''
                          where (w, s'') = break (==':') s'

main :: IO ()
main = putStrLn "Hello, Haskell!"
