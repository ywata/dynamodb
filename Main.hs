{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Aws
import Aws.DynamoDB
import qualified Data.ByteString.Lazy as LBS

import           Data.String
import qualified Data.Map                       as Map
import           Data.Aeson
import           Data.Aeson.Types

import qualified Aws.DynamoDB as D
import Network.HTTP.Conduit
import           Data.Aeson
import           Data.Aeson.Types

--myCreateJob :: IO ()
myCreateTable = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.createTable
                               [AttributeDefinition "ForumName" S]
                               [KeySchemaElement "ForumName" "HASH"]
                               []
                               (ProvisionedThroughput 1 1 Nothing Nothing Nothing) "NewTable"

{-
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.putItem
                               (Expected Map.empty)
                               (Item Map.empty)
                               TOTAL
                               NONE_
                               "NONE"
                               "NewTable"
-}
{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.listTables -}
{-  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.describeTable ("NewTable")
-}
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.deleteTable ("NewTable")
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.deleteTable ("NewTable")

  return rsp

main = myCreateTable

--my_ddb_cfg :: DdbConfiguration NormalQuery
--my_ddb_cfg = ddbLocalConfiguration HTTPS ddbEndpointEu

my_ddb_cfg :: DdbConfiguration NormalQuery
my_ddb_cfg  = D.ddbConfiguration HTTP D.ddbEndpointLocal
              



