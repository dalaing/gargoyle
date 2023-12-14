{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List
import System.Environment
import System.Exit

import Gargoyle.Redis
import Gargoyle.Redis.Nix
import System.Which

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbPath] -> do
      g <- redisNix
      redisLocal g $(staticWhich "redis-cli") dbPath Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ intercalate " "
        [ "USAGE:", pname, "<path>" ]
      putStrLn "\t<path>: path to local db"
      exitFailure
  return ()
