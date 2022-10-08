{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.List
import Data.Text
import System.Environment
import System.IO

jumpingOnClouds :: [Int] -> Int
jumpingOnClouds [] = 0
jumpingOnClouds [x] = 0
jumpingOnClouds [a, b] = 1
jumpingOnClouds (x : a : b : xs)
  | a == 1 = jumpingOnClouds (b : xs) + 1
  | b == 1 = jumpingOnClouds (a : b : xs) + 1
  | otherwise = jumpingOnClouds (b : xs) + 1

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  nTemp <- getLine
  let n = read $ lstrip $ rstrip nTemp :: Int

  cTemp <- getLine

  let c = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip cTemp

  let result = jumpingOnClouds c

  hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr

