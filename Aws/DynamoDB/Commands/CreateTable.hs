{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.DynamoDB.Commands.CreateTable
    ( CreateTable(..)
    , CreateTableResponse(..)
    , createTable
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Text as T


-- | A brief example createJob program
--
-- @
--      {-# LANGUAGE OverloadedStrings  #-}
--      
--      import           Aws
--      import           Aws.DynamoDB
--      import           Network.HTTP.Conduit
--      
--      myCreateJob :: IO ()
--      myCreateJob = 
--       do cfg <- Aws.baseConfiguration
--          rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
--                      createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
--          print rsp
--        
--      my_ets_cfg :: EtsConfiguration NormalQuery
--      my_ets_cfg = etsConfiguration HTTPS etsEndpointEu
--              
--      my_preset :: PresetId
--      my_preset = "1351620000001-000001"             -- System preset: Generic 720p
--              
--      my_pipeline :: PipelineId
--      my_pipeline = "<one-of-ypour-pipeline-ids>"
-- @


data CreateTable
    = CreateTable
        { ctAttributeDefinitions    :: [AttributeDefinition]
          , ctKeySchema             :: KeySchema
          , ctLocalSecondaryIndexes :: [LocalSecondaryIndex]
          , ctProvisionedThroughput :: ProvisionedThroughput
          , ctTableName             :: T.Text
        }
    deriving (Show,Eq)

instance ToJSON CreateTable where
  toJSON (CreateTable a b c d e) =
    object[
      "AttributeDefinitions"    .= a
      , "KeySchema"             .= toJSON b
--      , "LocalSecondaryIndexes" .= toJSON c
      , "ProvisionedThroughput" .= toJSON d
      , "TableName"             .= e
    ]

data CreateTableResult = CreateTableResult{
  tableDescription::TableDescription
  }deriving(Show, Eq)
instance FromJSON CreateTableResult where
  parseJSON (Object v) =
    CreateTableResult <$> v .: "TableDescription" 
  parseJSON _ = mzero

data CreateTableResponse
    = CreateTableResponse
        { ctrTableDescription :: TableDescription}
    deriving (Show,Eq)

createTable :: [AttributeDefinition] -> KeySchema ->  [LocalSecondaryIndex] -> ProvisionedThroughput -> T.Text -> CreateTable
createTable  ad ks lsi pt tn = CreateTable ad ks lsi pt tn


data Tx = Tx T.Text
instance ToJSON Tx where
  toJSON (Tx s) = String $ s

instance SignQuery CreateTable where

    type ServiceConfiguration CreateTable = DdbConfiguration

    signQuery ct@CreateTable {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.CreateTable"
        , ddbqBody    = Just $ toJSON $ ct
        }

instance ResponseConsumer CreateTable CreateTableResponse where

    type ResponseMetadata CreateTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (CreateTableResult tb@(TableDescription _ _ _  _ _ _ _ _ _)) = CreateTableResponse tb


instance Transaction CreateTable CreateTableResponse

instance AsMemoryResponse CreateTableResponse where

    type MemoryResponse CreateTableResponse = CreateTableResponse

    loadToMemory = return
