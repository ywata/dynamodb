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
import           Data.Aeson         hiding(Value)
import qualified Data.Aeson         as A (Value)
import qualified Data.Text as T

data NotYet = NotYet
  deriving (Show, Eq)
data BatchWriteItem
    = BatchWriteItem
        {
          bwiRequestItems                  :: NotYet
          
          , bwiReturnConsumedCapacity      :: Maybe ReturnConsumedCapacity
          , bwiReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
        }
    deriving (Show, Eq)

instance ToJSON BatchWriteItem where
  toJSON (BatchWriteItem a b c) =
    object[
      ]


data BatchWriteItemResponse
    = BatchWriteItemResponse {}
    deriving (Show,Eq)


batchWriteItem :: NotYet -> Maybe ReturnConsumedCapacity -> Maybe ReturnItemCollectionMetrics -> BatchWriteItem
batchWriteItem a b c = BatchWriteItem a b c



instance SignQuery BatchWriteItem where

    type ServiceConfiguration BatchWriteItem  = DdbConfiguration

    signQuery a@BatchWriteItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.BatchWriteItem"
        , ddbqBody    = Just $ toJSON $ a
        }


data BatchWriteItemResult = BatchWriteItemResult{
  rwrConsumedCapacity :: Maybe ConsumedCapacity
  , rwrItemCollectionMetrics :: Maybe ItemCollectionMetrics
  , rwrUnprocessedItems      :: NotYet
                                                }
instance FromJSON BatchWriteItemResult where
 parseJSON _ = return $ BatchWriteItemResult Nothing Nothing  NotYet

instance ResponseConsumer BatchWriteItem BatchWriteItemResponse where

    type ResponseMetadata BatchWriteItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (BatchWriteItemResult {}) = BatchWriteItemResponse{}


instance Transaction BatchWriteItem BatchWriteItemResponse

instance AsMemoryResponse BatchWriteItemResponse where

    type MemoryResponse BatchWriteItemResponse = BatchWriteItemResponse

    loadToMemory = return

