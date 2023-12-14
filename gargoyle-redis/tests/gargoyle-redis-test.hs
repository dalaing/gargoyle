module Main where

import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Temp

import Gargoyle
import Gargoyle.Redis

main :: IO ()
main = do
  let testRedis = defaultRedis
        { _gargoyle_exec = "dist/build" </> "gargoyle-redis-monitor/gargoyle-redis-monitor"
        }
  --TODO make this exception safe
  testPath <- mkdtemp "redis-test"
  redisLocal testRedis "redis-cli" (testPath </> "db") (Just "")
  removeDirectoryRecursive testPath
  exitSuccess
