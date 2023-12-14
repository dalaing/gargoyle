module Gargoyle.Redis.Connect (withRedis, withRedis') where

import qualified Data.ByteString.Char8 as C8
import Gargoyle (withGargoyle)
import Gargoyle.Redis.Nix (redisNix)
import System.Directory (doesFileExist)
import Database.Redis

-- | Like 'withRedis'' but turns the connection string into a connection
-- 'Pool' for you and using 'error' on failure.
withRedis :: String -> (Connection -> IO a) -> IO a
withRedis dbPath f = either error pure =<< withRedis' dbPath f

-- | Connects to a redis instance using information at the given filepath.
-- The given filepath can be either a folder (for a local db)
-- or a file with a redis connection string.
--
-- 'withRedis'' takes a String, which represents the path to a redis instance, and a
-- function that returns redis connection information as arguments in
-- order to open and start the redis instance. Otherwise, it will create the
-- database for you if it doesn't exist.
withRedis' :: String -> (Connection -> IO a) -> IO (Either String a)
withRedis' dbPath f = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the URI for an existing server
    then C8.readFile dbPath >>= \b -> case C8.lines b of
      [] -> pure $ Left "Redis connection string configuration file is empty"
      -- TODO: Consider also blowing up if more than one line for next breaking release
      x:_ -> do
        case parseConnectInfo (C8.unpack x) of
          Left e -> pure $ Left e
          Right ci -> Right <$> withCheckedConnect ci f
    -- otherwise assume it's a folder for a local database
    else do
      g <- redisNix
      fmap Right . withGargoyle g dbPath $ \b ->
        withCheckedConnect (defaultConnectInfo {connectPort = UnixSocket (C8.unpack b)} ) f
