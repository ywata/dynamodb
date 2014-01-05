{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Aws.DynamoDB.Json.Test
    ( 
    ) where

import           Aws
import           Aws.DynamoDB
import           Aws.DynamoDB.Json.Types
import           Aws.DynamoDB.Json.TypeHelper
import           Control.Exception
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

--main = tests `catch` \_ -> return ()

tests = do
  deleteTables
  cfg <- Aws.baseConfiguration
  rsp <- createTableWith cfg my_ddb_cfg
         [AttributeDefinition "idx" AT_S, AttributeDefinition "a" AT_N]
         (KeySchema [KeySchemaElement "idx" HASH, KeySchemaElement "a" RANGE])
         (ProvisionedThroughput 1 1)
         (TableName "table")
         Nothing
  rsp <- dsc (TableName "table")

  rsp <- putItemWith cfg my_ddb_cfg  (Item (Map.fromList [("idx", AV_S "idx1"), ("a", AV_N "101"), ("b", AV_S "b101"), ("c",AV_S "c101")]))
         (TableName "table") Nothing (Just TOTAL) (Just SIZE) (Just RV_NONE)
  rsp <- putItemWith cfg my_ddb_cfg  (Item (Map.fromList [("idx", AV_S "idx2"), ("a", AV_N "102"), ("b", AV_S "b102"), ("c",AV_S "c102")]))
         (TableName "table") Nothing  (Just TOTAL) (Just SIZE) (Just ALL_OLD)
  rsp <- putItemWith cfg my_ddb_cfg  (Item (Map.fromList [("idx", AV_S "idx3"), ("a", AV_N "103"), ("b", AV_S "b103"), ("c",AV_S "c103")]))
         (TableName "table") Nothing (Just TOTAL) (Just SIZE) Nothing
  rsp <- updateItemWith cfg my_ddb_cfg 
         (Key . Map.fromList $ [("idx", AV_S "idx1"),("a", AV_N "101")])
         (TableName "table")
         (Just ( AttributeValueUpdate  "b" (Just PUT) (Just $  AV_S "b101x")))
         (Just (Expected $ Map.fromList [("b", ExpectedAttributeValue (Just True) (Just $ (AV_S "b101")))]))
         (Just TOTAL) (Just SIZE) (Just ALL_NEW)

  rsp <- updateItemWith cfg my_ddb_cfg 
         (Key . Map.fromList $ [("idx", AV_S "idx1"),("a", AV_N "103")])
         (TableName "table")
         (Just ( AttributeValueUpdate  "b" (Just DELETE) Nothing))
         Nothing (Just TOTAL) (Just SIZE) (Just ALL_OLD)
  rsp <- updateItemWith cfg my_ddb_cfg 
         (Key . Map.fromList $ [("idx", AV_S "idx2"),("a", AV_N "102")])
         (TableName "table")
         (Just ( AttributeValueUpdate  "b" (Just PUT) (Just $  AV_S "b102x")))
         Nothing (Just TOTAL) (Just SIZE) (Just ALL_NEW)

  rsp <- deleteItemWith cfg my_ddb_cfg
         (Key . Map.fromList $ [("idx", AV_S "idx2"),("a", AV_N "102")])
         (TableName "table")
         (Just (Expected $ Map.fromList [("b", ExpectedAttributeValue (Just True) (Just $ (AV_S "b102x")))]))
         Nothing Nothing Nothing 

--  rsp <- deleteItemWith cfg my_ddb_cfg
{-
  rsp <- updateItemWith cfg my_ddb_cfg 
         (Key . Map.fromList $ [("idx", AV_S "i"),("a", AV_N "100")])
         (TableName "table")
         (Just ( AttributeValueUpdate  "b" (Just DELETE) Nothing))
         Nothing Nothing Nothing Nothing
-}

  rsp <- scanWith cfg my_ddb_cfg
         (TableName "table")
         (Just $ AttributesToGet ["a"])
         Nothing
         (Just $ Limit 5)           
         (Just TOTAL)
         (Just . ScanFilter $ Map.fromList [("a", Condition NOT_NULL_ Nothing)]) -- ScanFilter
         (Just 10) -- Int
         (Just SPECIFIC_ATTRIBUTES) -- Select
         (Just 20) -- Int

  rsp <- (queryWith cfg my_ddb_cfg
         (TableName "table")
--         (Just $ AttributesToGet ["a"])
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing) 
         
--         (Just $ Limit 5)           
--         (Just TOTAL)
--         (Just . ScanFilter $ Map.fromList [("a", Condition NOT_NULL_ Nothing)]) -- ScanFilter
--         (Just 10) -- Int
--         (Just SPECIFIC_ATTRIBUTES) -- Select
--         (Just 20) -- Int
         
  

  return ()


queryWith cfg ddbcfg a b c d e f g h i j = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.query a b c d e f g h i j
  return rsp


scanWith cfg ddbcfg a b c d e f g h i = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.scan a b c d e f g h i
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

deleteItemWith cfg ddbcfg a b c d e f = do
  rsp <- withManager $ \mgr -> Aws.pureAws cfg ddbcfg mgr $
                               D.deleteItem a b c d e f
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
