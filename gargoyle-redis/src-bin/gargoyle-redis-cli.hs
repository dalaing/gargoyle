{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import System.Environment
import System.Exit

import Gargoyle.Redis

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbPath] -> redisLocal defaultRedis "redis-cli" dbPath Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ intercalate " "
        [ "USAGE:", pname, "<path>" ]
      putStrLn "\t<path>: path to local db"
      exitFailure
  return ()
