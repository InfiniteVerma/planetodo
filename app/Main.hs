{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (liftM2, replicateM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Default (def)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Semigroup ((<>))
import Data.Word (Word16)
import Database.MySQL.Base
  ( ConnectInfo (..),
    Connection,
    Option (..),
    Result,
    affectedRows,
    close,
    connect,
    dataSeek,
    defaultConnectInfo,
    fetchFields,
    fetchRow,
    fieldCount,
    nextResult,
    query,
    rowSeek,
    storeResult,
    useResult,
  )
import System.Console.StructuredCLI
import System.Environment (getEnv, getEnvironment)
import Test.Hspec

-- This is how to connect to our test database
-- Options with bytestring values are given to partially test #17 and #23
testConn :: String -> String -> String -> String -> Word16 -> ConnectInfo
testConn host username password database port =
  defaultConnectInfo
    { connectHost = host,
      connectUser = username,
      connectPassword = password,
      connectDatabase = database,
      connectPort = port
    }

root :: Commands ()
root = do
  list
  bye
  command "exit" "return to previous level" exit

-- //TODO use this in root. Use monad transformer?
-- loadEnvs :: IO ConnectInfoEnv
-- loadEnvs = do
--   loadFile defaultConfig
--   database <- getEnv "database"
--   username <- getEnv "username"
--   host <- getEnv "host"
--   port <- getEnv "port"
--   password <- getEnv "password"
--   return ConnectInfoEnv {host = pack host, user = pack username, pwd = pack password, database = pack database, port = 3000}

list :: Commands ()
list = command "list" "lists todo items" $ do
  loadFile defaultConfig
  database <- getEnv "database"
  username <- getEnv "username"
  host <- getEnv "host"
  port <- getEnv "port"
  password <- getEnv "password"
  liftIO . bracket (connect $ testConn "127.0.0.1" username password database 3000) close $ \conn -> do
    printTodos conn
  return NoAction

bye :: Commands ()
bye = command "bye" "say goodbye" $ do
  liftIO . putStrLn $ "Sayonara!"
  return NoAction

main :: IO ()
main = void $ runCLI "Hello CLI" def root

-- entry point
--
-- gets number of todos (into count)
-- stores Todo[] of length count using buildList method
-- main :: IO ()
-- main = do
-- loadFile defaultConfig
-- database <- getEnv "database"
-- username <- getEnv "username"
-- host <- getEnv "host"
-- port <- getEnv "port"
-- password <- getEnv "password"
-- bracket (connect $ testConn "127.0.0.1" username password database 3000) close $ \conn -> do
--   -- insertTodo (Todo {idNum = "1", todo = "asdf", priority = "3"}) conn
--   printTodos conn

insertTodo :: Todo -> Connection -> IO ()
insertTodo todoItem conn = query conn $ pack ("insert into categories (name, priority) values (" ++ todoStr ++ " , " ++ priorityStr ++ ");")
  where
    todoStr = "\"" ++ unpack (todo todoItem) ++ "\""
    priorityStr = "\"" ++ unpack (priority todoItem) ++ "\""

printTodos :: Connection -> IO ()
printTodos conn = do
  query conn "select * from categories"
  result <- storeResult conn
  count <- affectedRows conn
  print count
  todos <- buildList (fromIntegral count) result
  mapM_ print todos

printMaybe :: Show a => Maybe a -> IO ()
printMaybe m =
  when (isJust m) $
    print (fromJust m)

buildList :: Int -> Result -> IO [Todo]
buildList 0 result = return []
buildList c result = do
  row <- fetchRow result
  let ans = map fromJust row
  fmap ((:) (makeItem ans)) (buildList (c -1) result)

makeItem :: [ByteString] -> Todo
makeItem row = Todo {idNum = row !! 0, todo = row !! 1, priority = row !! 2}

data Priority = Low | Mid | High deriving (Enum)

data Todo = Todo
  { idNum :: ByteString,
    todo :: ByteString,
    priority :: ByteString -- // TODO add this priority
  }

data ConnectInfoEnv = ConnectInfoEnv
  { host :: ByteString,
    user :: ByteString,
    pwd :: ByteString,
    database :: ByteString,
    port :: Word16
  }
  deriving (Show)

instance Show Todo where
  show (Todo id text priority) = unpack id ++ ": " ++ unpack text ++ " - " ++ unpack priority

-- TODO
-- x. New main wrapper without hspec
-- x. build item of Todo
-- x. build list of Todo with count: rowCount
-- x. pretty print it
-- x create new table with more properties (edit above things)
-- x. insert new todo
-- x insert with custom data
-- x  pull db creds from .env
-- x cli wrapper
-- 6. github repo + documentation
-- 7. tweet