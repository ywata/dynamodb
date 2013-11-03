{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.BatchWriteItem
    ( BatchWriteItem(..)
    , BatchWriteItemResponse(..)
    , batchWriteItem
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T


data BatchWriteItem
    = BatchWriteItem
        {
        }
    deriving (Show, Eq)

instance ToJSON BatchWriteItem where
  toJSON (BatchWriteItem) =
    object[
      ]


data BatchWriteItemResponse
    = BatchWriteItemResponse {}
    deriving (Show,Eq)


batchWriteItem :: BatchWriteItem
batchWriteItem= BatchWriteItem



instance SignQuery BatchWriteItem where

    type ServiceConfiguration BatchWriteItem  = DdbConfiguration

    signQuery a@BatchWriteItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.BatchWriteItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data BatchWriteItemResult = BatchWriteItemResult{}
instance FromJSON BatchWriteItemResult where
 parseJSON _ = return BatchWriteItemResult

instance ResponseConsumer BatchWriteItem BatchWriteItemResponse where

    type ResponseMetadata BatchWriteItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (BatchWriteItemResult {}) = BatchWriteItemResponse{}


instance Transaction BatchWriteItem BatchWriteItemResponse

instance AsMemoryResponse BatchWriteItemResponse where

    type MemoryResponse BatchWriteItemResponse = BatchWriteItemResponse

    loadToMemory = return

