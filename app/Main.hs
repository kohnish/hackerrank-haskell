{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Data.Text
import System.Environment
import System.IO
import Debug.Trace

debug = flip trace
--
-- Complete the 'hourglassSum' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts 2D_INTEGER_ARRAY arr as parameter.
--

-- [[1,1,1,0,0,0],[0,1,0,0,0,0],[1,1,1,0,0,0],[0,0,2,4,4,0],[0,0,0,2,0,0],[0,0,1,2,4,0]]

--[1,1,1,0,0,0]
--[0,1,0,0,0,0]
--[1,1,1,0,0,0]
--[0,0,2,4,4,0]
--[0,0,0,2,0,0]
--[0,0,1,2,4,0]

hourglassSum :: [[Int]] -> Int
hourglassSum arr =

    -- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arrTemp <- readMultipleLinesAsStringArray 6
    let arr = map (\x -> map (read :: String -> Int) . words $ rstrip x) arrTemp

    let result = hourglassSum arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

