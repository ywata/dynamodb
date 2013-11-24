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
  verboseCheck prop_deleteAllTables
  verboseCheck prop_createTable
  verboseCheck prop_deleteAllTables
  verboseCheck prop_describeTable

prop_deleteAllTables = monadicIO $ do
  ltrsp <- run $ lt
  let tbls = tableNames ltrsp
  case tbls of
    Just tbls -> mapM (\x -> run $ del (x)) tbls
  ltrsp <- run $ lt
  assert (numTables ltrsp == Just 0)

  
--prop_createTable :: Property
prop_createTable = monadicIO $ do
  ltrsp <- run $ lt
  let preTbls = numTables ltrsp
  (tblName :: TableName) <- pick arbitrary
  pre $ 3 <= T.length (text tblName)  && T.length (text tblName) <= 255
  rsp <- run $ cre tblName
  ltrsp <- run $ lt
  let postTbls = numTables ltrsp

  case (preTbls, postTbls) of
    (Just pr, Just po) -> assert (pr + 1 == po)
    otherwise          -> assert False

  return ()

cre :: TableName -> IO CreateTableResponse
cre tbn = do
  cfg <- Aws.baseConfiguration
  
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.createTable
                               [AttributeDefinition "ForumName" AT_S, AttributeDefinition "Index" AT_N ]
                               [KeySchemaElement "ForumName" HASH, KeySchemaElement "Index" RANGE]
                               (ProvisionedThroughput 1 1) tbn  Nothing
  return rsp



numTables :: ListTablesResponse -> Maybe Int
numTables rsp = tableNames rsp >>= return . length

          

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
