{-# LANGUAGE OverloadedStrings #-}
module Aws.DynamoDB.Json.Test
    ( 
    ) where

import           Aws
import           Aws.DynamoDB
import           Aws.DynamoDB.Json.Types
import           Text.Printf
import qualified Data.Text                      as T
import           Data.Aeson                     hiding(Value)
import qualified Data.Map                       as Map
import           Control.Applicative
import           Control.Monad
import qualified Test.QuickCheck                as QC
import qualified Distribution.TestSuite         as TS
import           Aws.DynamoDB.Commands

import qualified Aws.DynamoDB as D
import Network.HTTP.Conduit
import           Data.Aeson
import           Data.Aeson.Types


my_ddb_cfg :: DdbConfiguration NormalQuery
my_ddb_cfg  = D.ddbConfiguration HTTP D.ddbEndpointLocal

cre :: String -> IO CreateTableResponse
cre tbn = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.createTable
                               [AttributeDefinition "ForumName" AT_S, AttributeDefinition "Index" AT_N ]
                               [KeySchemaElement "ForumName" HASH, KeySchemaElement "Index" RANGE]
                               (ProvisionedThroughput 1 1) (TableName  . T.pack $ tbn) Nothing
  return rsp
del::String -> IO DeleteTableResponse
del tab = do
  cfg <- Aws.baseConfiguration  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.deleteTable (TableName . T.pack $ tab)
  return rsp

lt = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables Nothing  (Just 5)
  return rsp




dsc = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable (TableName "Txx")

  return rsp
{-
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


-}