{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Bits
import Data.List
import Data.List.Split
import Data.Array
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'countingValleys' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER steps
--  2. STRING path
--

data Step = Up | Down deriving Show

readStep :: Char -> Step
readStep 'U' = Up
readStep 'D' = Down


takeStep :: (Int, Int) -> Step -> (Int, Int)
takeStep (alt, vs) Up = (alt + 1, vs)
takeStep (0, vs) Down = (-1, vs + 1)
takeStep (alt, vs) Down = (alt - 1, vs)


countingValleys :: Int -> String -> Int
countingValleys _ path = vs
    where
         (_, vs) = Data.List.foldl' takeStep (0, 0) (fmap readStep path)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    stepsTemp <- getLine
    let steps = read $ lstrip $ rstrip stepsTemp :: Int

    path <- getLine

    let result = countingValleys steps path

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

