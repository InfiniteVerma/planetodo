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
    { connectHost = "127.0.0.1",
      connectUser = "gh4j5k4hdpvr",
      connectPassword = "pscale_pw_Lru3XvcRzQsl0R9QzyVMGAJF2KcOWZeyFudmCv83DGc",
      connectDatabase = "firsttest",
      connectPort = 41291
    }

-- entry point
--
main :: IO ()
main = do
  bracket (connect $ testConn) close $ \conn -> hspec $ do
    describe "Database" $ do
      it "seems to be connected" $ do
        query conn "select * from categories"
        result <- storeResult conn
        row <- fetchRow result
        rowCount <- fieldCount (Right result)
        print rowCount
        row `shouldBe` [Just "1", Just "office supplies"]