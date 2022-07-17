{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (liftM2, when, replicateM_)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, fromMaybe, isJust)
import Database.MySQL.Base
  ( ConnectInfo (..),
    Option (..),
    Result,
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
    affectedRows,
    Connection
  )
import System.Environment (getEnvironment)
import Test.Hspec

-- This is how to connect to our test database
-- Options with bytestring values are given to partially test #17 and #23
testConn :: ConnectInfo
testConn =
  defaultConnectInfo
    { connectHost = "",
      connectUser = "",
      connectPassword = "",
      connectDatabase = "",
      connectPort = 0
    }

-- entry point
--
-- gets number of todos (into count)
-- stores Todo[] of length count using buildList method
main :: IO ()
main = do
  bracket (connect testConn) close $ \conn -> do
    insertTodo (Todo {idNum="1", todo="asdf", priority="3"}) conn
    printTodos conn

insertTodo :: Todo -> Connection -> IO ()
insertTodo todoItem conn = query conn $ pack ("insert into categories (name, priority) values (" ++ todoStr ++ " , " ++ priorityStr ++ ");")
    where todoStr = "\"" ++ unpack (todo todoItem) ++ "\""
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
-- 5. cli wrapper
-- 6. github repo + documentation
-- 7. tweet