{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Aws
import Aws.DynamoDB
import qualified Data.ByteString.Lazy as LBS

import           Data.String
import qualified Data.Map                       as Map
--import           Data.Aeson
--import           Data.Aeson.Types

import qualified Aws.DynamoDB as D
import Network.HTTP.Conduit
import           Data.Aeson
import           Data.Aeson.Types

--myCreateJob :: IO ()
cre tbn = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.createTable
                               [AttributeDefinition "ForumName" AT_S, AttributeDefinition "Index" AT_N ]
                               [KeySchemaElement "ForumName" HASH, KeySchemaElement "Index" RANGE]
                               []
                               (ProvisionedThroughput 1 1 Nothing Nothing Nothing) tbn
  return rsp

lt = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables "Txx" 5
  return rsp


dsc = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable "Txx"

  return rsp

get = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.getItem (["abcdefg"]) (Just True) (Map.fromList
                                         [("ForumName", ValueS "abcdef"), ("Index", ValueN 2)])  (Just True) "Txx"
  return rsp
 
put = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.putItem
                               (Expected . Map.fromList $ [])
                               (Item . Map.fromList $ [("ForumName", ValueS "abcdefg"), ("Index", ValueN 1)])
--                               (Item Map.empty)
                               TOTAL
                               NONE_
                               ALL_OLD
                               "Txx"
  return rsp
{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables -}
{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable ("NewTable")
-}

  return rsp

main = cre "a123"

del = do
  cfg <- Aws.baseConfiguration  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.deleteTable ("Txx")
  return rsp


--my_ddb_cfg :: DdbConfiguration NormalQuery
--my_ddb_cfg = ddbLocalConfiguration HTTPS ddbEndpointEu

my_ddb_cfg :: DdbConfiguration NormalQuery
my_ddb_cfg  = D.ddbConfiguration HTTP D.ddbEndpointLocal
              


a = "{\"TableDescription\":{\"AttributeDefinitions\":[{\"AttributeName\":\"Index\",\"AttributeType\":\"N\"},{\"AttributeName\":\"ForumName\",\"AttributeType\":\"S\"}],\"TableName\":\"Txx\",\"KeySchema\":[{\"AttributeName\":\"ForumName\",\"KeyType\":\"HASH\"},{\"AttributeName\":\"Index\",\"KeyType\":\"RANGE\"}],\"TableStatus\":\"ACTIVE\",\"CreationDateTime\":1383482892.691,\"ProvisionedThroughput\":{\"NumberOfDecreasesToday\":0,\"ReadCapacityUnits\":1,\"WriteCapacityUnits\":1},\"TableSizeBytes\":0,\"ItemCount\":0}}"
