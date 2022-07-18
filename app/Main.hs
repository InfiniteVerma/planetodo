{-# LANGUAGE OverloadedStrings #-}

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (liftM2, replicateM_, void, when)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, fromMaybe, isJust)
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
import System.Environment (getArgs, getEnv, getEnvironment)
import Test.Hspec

data Priority = Low | Mid | High deriving (Enum)

data Todo = Todo
  { idNum :: ByteString,
    todo :: ByteString,
    priority :: ByteString -- // TODO add this priority using enum
  }

instance Show Todo where
  show (Todo id text priority) = unpack id ++ ": " ++ unpack text ++ " - " ++ unpack priority

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

-- evaluate function
--
-- pattern match on the two command line options and do the corresponding
evaluate :: [String] -> String -> String -> String -> IO ()
evaluate ["--list"] user pwd db = bracket (connect $ testConn "127.0.0.1" user pwd db 3000) close $ \conn -> do printTodos conn
evaluate ("--insert" : xs) user pwd db = bracket (connect $ testConn "127.0.0.1" user pwd db 3000) close $ \conn -> do
  let todoitem = unwords xs
  insertTodo (Todo {idNum = "1", todo = pack todoitem, priority = "1"}) conn
evaluate _ user pwd db = printHelp

printHelp :: IO ()
printHelp = putStrLn "PlaneTodo v1.0.0\n\nCommands list: \n1. --list\n2. --insert {todoitem}"

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

buildList :: Int -> Result -> IO [Todo]
buildList 0 result = return []
buildList c result = do
  row <- fetchRow result
  let ans = map fromJust row
  fmap ((:) (makeItem ans)) (buildList (c -1) result)

makeItem :: [ByteString] -> Todo
makeItem row = Todo {idNum = row !! 0, todo = row !! 1, priority = row !! 2}

-- entry point
main :: IO ()
main = do
  loadFile defaultConfig
  db <- getEnv "database"
  user <- getEnv "username"
  host <- getEnv "host"
  port <- getEnv "port"
  pwd <- getEnv "password"
  args <- getArgs
  print args
  if null args
    then printHelp
    else evaluate args user pwd db