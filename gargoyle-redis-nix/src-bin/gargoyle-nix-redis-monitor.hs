module Main where

import Gargoyle
import Gargoyle.Redis.Nix

main :: IO ()
main = redisNix >>= gargoyleMain
