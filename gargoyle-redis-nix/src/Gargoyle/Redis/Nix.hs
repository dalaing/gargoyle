{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.Redis.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.Redis

import Paths_gargoyle_redis_nix
import System.Which

redisNix :: IO (Gargoyle FilePath ByteString)
redisNix = do
  bindir <- getBinDir
  return $ (mkRedisGargoyle $(staticWhich "redis-server") $(staticWhich "redis-cli") shutdownRedisFast)
    { _gargoyle_exec = bindir <> "/gargoyle-nix-redis-monitor"
    }
