module Main where

import Gargoyle
import Gargoyle.Redis

main :: IO ()
main = gargoyleMain defaultRedis
