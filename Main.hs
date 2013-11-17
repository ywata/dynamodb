{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Aws
import Aws.DynamoDB
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text                      as T
import           Data.String
import qualified Data.Map                       as Map
--import           Data.Aeson
--import           Data.Aeson.Types

import qualified Aws.DynamoDB as D
import Network.HTTP.Conduit
import           Data.Aeson
import           Data.Aeson.Types
{-
--myCreateJob :: IO ()
cre :: String -> IO CreateTableResponse
cre tbn = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.createTable
                               [AttributeDefinition "ForumName" AT_S, AttributeDefinition "Index" AT_N ]
                               [KeySchemaElement "ForumName" HASH, KeySchemaElement "Index" RANGE]
                               (Just (ProvisionedThroughput 1 1)) (T.pack tbn)
  return rsp

lt = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables (Just "Txx")  (Just 5)
  return rsp


dsc = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable "Txx"

  return rsp

get = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.getItem (Just ["abcdefg"]) (Just True)
                                        ( Keys $ Map.fromList
                                         [("ForumName"::T.Text, ValueS "abcdef"), ("Index", ValueN 2)])  (Just True) "Txx"
  return rsp
 
put = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.putItem
                               (Just . Expected . Map.fromList $ [])
                               (Item . Map.fromList $ [("ForumName", ValueS "abcdefg"), ("Index", ValueN 1)])
--                               (Item Map.empty)
                               (Just TOTAL)
                               (Just NONE_)
                               (Just ALL_OLD)
                               "Txx"
  return rsp
{-scan = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.scan
                               ""
  return rsp
-}

{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables -}
{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable ("NewTable")
-}

  return rsp

main = cre "a123"

del::String -> IO DeleteTableResponse
del tab = do
  cfg <- Aws.baseConfiguration  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.deleteTable (T.pack tab)
  return rsp


--my_ddb_cfg :: DdbConfiguration NormalQuery
--my_ddb_cfg = ddbLocalConfiguration HTTPS ddbEndpointEu

my_ddb_cfg :: DdbConfiguration NormalQuery
my_ddb_cfg  = D.ddbConfiguration HTTP D.ddbEndpointLocal
              


a = "{\"TableDescription\":{\"AttributeDefinitions\":[{\"AttributeName\":\"Index\",\"AttributeType\":\"N\"},{\"AttributeName\":\"ForumName\",\"AttributeType\":\"S\"}],\"TableName\":\"Txx\",\"KeySchema\":[{\"AttributeName\":\"ForumName\",\"KeyType\":\"HASH\"},{\"AttributeName\":\"Index\",\"KeyType\":\"RANGE\"}],\"TableStatus\":\"ACTIVE\",\"CreationDateTime\":1383482892.691,\"ProvisionedThroughput\":{\"NumberOfDecreasesToday\":0,\"ReadCapacityUnits\":1,\"WriteCapacityUnits\":1},\"TableSizeBytes\":0,\"ItemCount\":0}}"
-}
