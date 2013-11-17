{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.BatchGetItem
    ( BatchGetItem(..)
    , BatchGetItemResponse(..)
    , batchGetItem
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T


data BatchGetItem
    = BatchGetItem
        {
        }
    deriving (Show, Eq)

instance ToJSON BatchGetItem where
  toJSON (BatchGetItem) =
    object[
      ]


data BatchGetItemResponse
    = BatchGetItemResponse {}
    deriving (Show,Eq)


batchGetItem :: BatchGetItem
batchGetItem= BatchGetItem



instance SignQuery BatchGetItem where

    type ServiceConfiguration BatchGetItem  = DdbConfiguration

    signQuery a@BatchGetItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.BatchGetItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data NotYet = NotYet
data BatchGetItemResult = BatchGetItemResult{
  rgrConsumedCapacity :: Maybe ConsumedCapacity
  , rgrResponses      :: Maybe NotYet
  , rgrUnprocessedKeys:: Maybe NotYet
  }
instance FromJSON BatchGetItemResult where
 parseJSON _ = return $ BatchGetItemResult Nothing Nothing Nothing -- NotYet

instance ResponseConsumer BatchGetItem BatchGetItemResponse where

    type ResponseMetadata BatchGetItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (BatchGetItemResult {}) = BatchGetItemResponse{}


instance Transaction BatchGetItem BatchGetItemResponse

instance AsMemoryResponse BatchGetItemResponse where

    type MemoryResponse BatchGetItemResponse = BatchGetItemResponse

    loadToMemory = return

