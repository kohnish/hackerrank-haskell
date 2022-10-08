{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Data.Text
import System.Environment
import System.IO

--
-- Complete the 'repeatedString' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts following parameters:
--  1. STRING s
--  2. LONG_INTEGER n
--
countA :: String -> Int
countA = foldr (\x y -> if x == 'a' then y + 1 else y) 0

rtrimStr :: String -> Int -> String
rtrimStr s n =
  let (first, _) = splitAt n s
   in first

repeatedString :: String -> Int -> Int
repeatedString s n =
  let a_num = countA s
      s_len = length s
      (d, m) = n `divMod` s_len
      min_match_num = a_num * d
      add_num = countA $ rtrimStr s m
   in min_match_num + add_num

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  s <- getLine

  nTemp <- getLine
  let n = read $ lstrip $ rstrip nTemp :: Int

  let result = repeatedString s n

  hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr


