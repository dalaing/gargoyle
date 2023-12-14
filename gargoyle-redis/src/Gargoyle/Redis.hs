{-# LANGUAGE OverloadedStrings #-}
module Gargoyle.Redis where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Signals
import System.Process

import Gargoyle

-- | A 'Gargoyle' that assumes `redis-server` and `redis-cli` are in the path.
defaultRedis :: Gargoyle FilePath ByteString
defaultRedis = mkRedisGargoyle "redis-server" "redis-cli" shutdownRedisFast

-- | Create a gargoyle by telling it where the relevant Redis executables are.
mkRedisGargoyle :: FilePath -- ^ Path to `redis-server`
                -> FilePath -- ^ Path to `redis-cli`
                -> (FilePath -> FilePath -> IO ()) -- ^ Shutdown functino
                -> Gargoyle FilePath ByteString
                -- ^ The 'Gargoyle' returned provides to client code the connection
                -- string that can be used to connect to the Redis server
mkRedisGargoyle redisServerPath redisCliPath shutdownFun = Gargoyle
  { _gargoyle_exec = "gargoyle-redis-monitor"
  , _gargoyle_init = initLocalRedis
  , _gargoyle_start = startLocalRedis redisServerPath
  , _gargoyle_stop = shutdownFun redisCliPath
  , _gargoyle_getInfo = getLocalRedisConnectionString
  }

-- TODO something that allows us to add more config data would be handy
initLocalRedis :: FilePath -- ^ Path in which to initialize Redis Server
               -> IO ()
initLocalRedis dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  createDirectoryIfMissing True absoluteDbDir

getLocalRedisConnectionString :: FilePath -> IO ByteString
getLocalRedisConnectionString dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  pure . mconcat $
    [ T.encodeUtf8 . T.pack $ absoluteDbDir
    , "/redis.sock"
    ]

-- | Start a Redis server that is assumed to be in the given folder. This is a low level function
-- used to define the Redis 'Gargoyle'
startLocalRedis :: FilePath -- ^ Path to Redis `redis-server` executable
                -> FilePath -- ^ Path where the server to start is located
                -> IO FilePath -- ^ handle of the Redis server
startLocalRedis binPath dbDir = do
  -- redis-server [path to redis.conf created by init]
  devNull <- openFile "/dev/null" WriteMode
  absoluteDbDir <- makeAbsolute dbDir
  writeFile
    (absoluteDbDir </> "redis.conf") $
    unlines
      [ "port 0"
      , mconcat ["unixsocket ", absoluteDbDir, "/redis.sock"]
      , "unixsocketperm 770"
      , "daemonize yes"
      , mconcat ["pidfile ", absoluteDbDir, "/redis.pid"]
      , "dbfilename dump.rdb"
      , mconcat ["dir ", absoluteDbDir]
      , mconcat ["logfile ", absoluteDbDir, "/redis.log"]
      ]
  createDirectoryIfMissing True absoluteDbDir
  (_, _, _, redis) <- createProcess (proc binPath
    [ absoluteDbDir <> "/redis.conf"
    ]) { std_in = Inherit, std_out = UseHandle devNull, std_err = Inherit }
  r <- waitForProcess redis
  case r of
    ExitSuccess -> return absoluteDbDir
    _ -> do
      putStrLn $ "startLocalRedis failed: " <> show r
      exitWith r

-- | Perform a fast shudown of Redis;
-- see https://redis.io/commands/shutdown/
shutdownRedisFast :: FilePath -- ^ Path to Redis `redis-cli` executable
                  -> FilePath -- ^ Path where the server to start is located
                  -> IO ()
shutdownRedisFast = shutdownRedisWithOptions "NOW"

-- | Perform a regular shudown of Redis;
-- see https://redis.io/commands/shutdown/
shutdownRedisRegular :: FilePath -- ^ Path to Redis `redis-cli` executable
                  -> FilePath -- ^ Path where the server to start is located
                  -> IO ()
shutdownRedisRegular = shutdownRedisWithOptions ""

shutdownRedisWithOptions :: String -- ^ Argument to the `shutdown` function
                         -> FilePath -- ^ Path to Redis `redis-cli` executable
                         -> FilePath -- ^ Path where the server to start is located
                         -> IO ()
shutdownRedisWithOptions options binPath absoluteDbDir = do
  -- redis-cli -s [unix socket] shutdown [options]
  (_, _, _, redis) <- createProcess (proc binPath
    ([ "-s"
    , absoluteDbDir <> "/redis.sock"
    , "shutdown"
    ] ++ words options)) { std_in = NoStream, std_out = NoStream, std_err = Inherit }
  r <- waitForProcess redis
  case r of
    ExitSuccess -> return ()
    _ -> do
      putStrLn $ "shutdownLocalRedis failed: " <> show r
      exitWith r

-- | Run `redis-cli` against a Gargoyle managed db.
redisLocal :: Gargoyle pid ByteString -- ^ 'Gargoyle' against which to run
          -> FilePath -- ^ The path to `redis-cli`
          -> FilePath -- ^ The path where the managed daemon is expected
          -> Maybe String
          -- ^ Optionally provide stdin input instead of an inheriting current stdin.
          -- It will have a newline and quit command appended to it.
          -> IO ()
redisLocal g redisCliPath dbPath minput = withGargoyle g dbPath $ \dbUri -> do
  void $ installHandler keyboardSignal Ignore Nothing
  let redisProc = (proc redisCliPath [ "-s", T.unpack $ T.decodeUtf8 dbUri ])
        { std_in = case minput of
            Nothing -> Inherit
            Just _ -> CreatePipe
        , std_out = Inherit
        , std_err = Inherit
        }
  (mStdin, _, _, redis) <- createProcess redisProc
  case minput of
    Nothing -> return ()
    Just input -> hPutStrLn (fromJust mStdin) (input ++ "\nquit\n")
  ExitSuccess <- waitForProcess redis
  return ()

-- | Run an arbitrary process against a Gargoyle-managed DB, providing connection
--   information by substituting a given argument pattern with the connection string.
runRedisLocalWithSubstitution
  :: Gargoyle pid ByteString -- ^ 'Gargoyle' against which to run
  -> FilePath -- ^ The path where the managed daemon is expected
  -> FilePath -- ^ Path to process to run
  -> (String -> [String]) -- ^ Function producing arguments to the process given the connection string
  -> Maybe String -- ^ Optionally provide stdin input instead of an inheriting current stdin.
  -> IO ExitCode
runRedisLocalWithSubstitution g dbPath procPath mkProcArgs mInput = withGargoyle g dbPath $ \dbUri -> do
  void $ installHandler keyboardSignal Ignore Nothing
  let
    procSpec = (proc procPath $ mkProcArgs $ T.unpack $ T.decodeUtf8 dbUri)
      { std_in = case mInput of
          Nothing -> Inherit
          Just _ -> CreatePipe
      , std_out = Inherit
      , std_err = Inherit
      }
  withCreateProcess procSpec $ \mStdin _ _ procHandle -> do
    for_ mInput $
      hPutStrLn (fromMaybe (error "runRedisLocalWithSubstitution: input stream was expected") mStdin)
    waitForProcess procHandle
