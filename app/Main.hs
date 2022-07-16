{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString)
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
-- main :: IO ()
-- main = do
--   bracket (connect $ testConn) close $ \conn -> hspec $ do
--     describe "Database" $ do
--       it "seems to be connected" $ do
--         query conn "select * from categories"
--         result <- storeResult conn
--         row <- fetchRow result
--         rowCount <- fieldCount (Right result)
--         print rowCount
-- mapM_ printMaybe row
--         row2 <- fetchRow result
--         mapM_ printMaybe row2
--         row3 <- fetchRow result
--         mapM_ printMaybe row3
--         row `shouldBe` [Just "1", Just "office supplies"]

main :: IO ()
main = do
  bracket (connect testConn) close $ \conn -> do
    query conn "select * from categories"
    result <- storeResult conn
    count <- fieldCount (Right result)
    todos <- buildList count result
    print todos
    print count

printMaybe :: Show a => Maybe a -> IO ()
printMaybe m =
  when (isJust m) $
    print (fromJust m)

buildList :: Int -> Result -> IO [Todo]
buildList _ result = do
  row <- fetchRow result
  let ans = map fromJust row
  return [(makeItem ans)]

makeItem :: [ByteString] -> Todo
makeItem row = Todo {idNum = row !! 0, text = row !! 1}

data Priority = Low | Mid | High deriving (Enum)

data Todo = Todo
  { idNum :: ByteString,
    text :: ByteString
    -- priority :: Priority // TODO add this priority
  }
  deriving (Show)

-- TODO
-- 0. New main wrapper without hspec
-- 1. build item of Todo
-- 2. build list of Todo with count: rowCount
-- 3. pretty print it
-- 4. insert new todo
-- 5. cli wrapper
-- 6. github repo + documentation
-- 7. tweet