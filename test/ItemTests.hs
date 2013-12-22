{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Aws.DynamoDB.Json.Test
    ( 
    ) where

import           Aws
import           Aws.DynamoDB
import           Aws.DynamoDB.Json.Types
import           Control.Monad.IO.Class
import           Data.Maybe (fromJust)
import           Text.Printf
import qualified Data.Text                      as T
import           Data.Aeson                     hiding(Value)
import qualified Data.Map                       as Map
import           Control.Applicative
import           Control.Monad
import qualified Test.QuickCheck                as QC
import           Aws.DynamoDB.Commands

import qualified Aws.DynamoDB as D
import Network.HTTP.Conduit
import           Data.Aeson
import           Data.Aeson.Types

import Test.QuickCheck (arbitrary, Property, quickCheck,verboseCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)



my_ddb_cfg :: DdbConfiguration NormalQuery
my_ddb_cfg  = D.ddbConfiguration HTTP D.ddbEndpointLocal

--main :: IO()
main = do
  deleteTables
  cfg <- Aws.baseConfiguration
  rsp <- createTableWith cfg my_ddb_cfg
         [AttributeDefinition "idx" AT_S]
         (KeySchema [KeySchemaElement "idx" HASH])
         (ProvisionedThroughput 1 1)
         (TableName "table")
         Nothing
  rsp <- dsc (TableName "table")

  rsp <- putItemWith cfg my_ddb_cfg  (Item (Map.fromList [("idx", ValueS "-1"), ("val", ValueS "S"), ("d", ValueN "2")]))
         (TableName "table") Nothing Nothing Nothing Nothing 
  rsp <- getItemWith cfg my_ddb_cfg 
         (Key . Map.fromList $ [("idx", ValueS "-1")]) (TableName "table") (Just ["d","val"]) Nothing Nothing 
  rsp <- updateItemWith cfg my_ddb_cfg 
         (Key . Map.fromList $ [("idx", ValueS "-1")])
         (TableName "table")
         (Just $ AttributeValueUpdate (Just DELETE) ( Just AV_S))
         Nothing Nothing Nothing Nothing

  return rsp


updateItemWith cfg ddbcfg a b c d e f g = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.updateItem a b c d e f g
  return rsp
  

getItemWith cfg ddbcfg a b c d e  = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.getItem a b c d e
  return rsp


putItemWith cfg ddbcfg a b c d e f = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.putItem a b c d e f
  return rsp

createTableWith  cfg ddbcfg a b c d e = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.createTable a b c d e
  return rsp


deleteTableWith  cfg ddbcfg a = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.deleteTable a
  return rsp
deleteTables = do
  ltrsp <- lt
  let tbls = tableNames ltrsp
  case tbls of
    Just tbs -> mapM del tbs

del::TableName  -> IO DeleteTableResponse
del tab = do
  cfg <- Aws.baseConfiguration  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.deleteTable tab
  return rsp



lt = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables Nothing Nothing
  return rsp

dsc tblName = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable tblName
  return rsp
