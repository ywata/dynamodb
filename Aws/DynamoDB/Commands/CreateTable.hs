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
import qualified Test.QuickCheck   as QC
import           Data.Aeson
import qualified Data.Text as T


data CreateTable
    = CreateTable
        { ctAttributeDefinitions    :: [AttributeDefinition]       -- Yes
          , ctKeySchema             :: KeySchema                   -- Yes
          , ctProvisionedThroughput :: ProvisionedThroughput       -- Yes
          , ctTableName             :: TableName                   -- Yes
          , ctLocalSecondaryIndexes :: Maybe [LocalSecondaryIndex] -- No
        }
    deriving (Show,Eq)
instance ToJSON CreateTable where
  toJSON (CreateTable a b c d e) =
    object[
      "AttributeDefinitions"    .= a
      , "KeySchema"             .= b
      , "ProvisionedThroughput" .= c
      , "TableName"             .= d
      , "LocalSecondaryIndexes" .= e
    ]
instance FromJSON CreateTable where
  parseJSON (Object v) = CreateTable <$>
                         v .: "AttributeDefinitions"  <*>
                         v .: "KeySchema"             <*>
                         v .: "ProvisionedThroughput" <*>
                         v .: "TableName"             <*>
                         v .:? "LocalSecondaryIndexes" 
instance QC.Arbitrary CreateTable where
  arbitrary = CreateTable <$>
              QC.arbitrary  <*>
              QC.arbitrary  <*>
              QC.arbitrary  <*>
              QC.arbitrary  <*>
              QC.arbitrary



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
instance ToJSON   CreateTableResponse where
  toJSON(CreateTableResponse a) = object["TableDescription" .= a]
instance FromJSON CreateTableResponse where
  parseJSON (Object v) =
    CreateTableResponse <$> v .: "TableDescription" 
  parseJSON _ = mzero
instance QC.Arbitrary CreateTableResponse where
  arbitrary = CreateTableResponse <$> QC.arbitrary

-- Checked
createTable :: [AttributeDefinition]
               -> KeySchema
               -> ProvisionedThroughput
               -> TableName
               ->  Maybe [LocalSecondaryIndex]
               -> CreateTable
createTable  a b c d e = CreateTable a b c d e


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
