{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitWith)

import Gargoyle.Redis (runRedisLocalWithSubstitution)
import Gargoyle.Redis.Nix (redisNix)

main :: IO ()
main = do
  args <- getArgs
  case args of
    dbPath:cmd:cmdArgs -> do
      redis <- redisNix
      exitWith =<< runRedisLocalWithSubstitution redis dbPath cmd (\conn -> if null cmdArgs then [conn] else substArgs conn cmdArgs) Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ unwords [ "USAGE:", pname, "<path>", "<command>", "[...<arguments>]" ]
      putStrLn "\t<path>: path to local db"
      putStrLn "\t<command>: command to run"
      putStrLn "\t<arguments>: list of arguments to <command> where the special argument '{}' is expanded into a connection string; if <arguments> is empty, '{}' will be supplied as the only argument by default"
      exitFailure
  where
    substArgs conn = map (\x -> if x == "{}" then conn else x)
