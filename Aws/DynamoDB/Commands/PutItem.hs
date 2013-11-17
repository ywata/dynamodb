{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.DynamoDB.Commands.PutItem
    ( PutItem(..)
    , PutItemResponse(..)
    , putItem
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Map as Map

import qualified Data.Text as T
import           Aws.DynamoDB.Json.Types

data PutItem
    = PutItem
        {
          piItem                         :: Item                              -- Yes
          , piTableName                  :: TableName                         -- Yes
          , piExpected                   :: Maybe Expected                    -- No

          , piReturnConsumedCapacity     :: Maybe ReturnConsumedCapacity      -- No
          , piReturnItemCollectionMetrics:: Maybe ReturnItemCollectionMetrics -- No
          , piReutrnValues               :: Maybe ReturnValues                -- No

        }
    deriving (Show, Eq)

instance ToJSON PutItem where
  toJSON (PutItem a b c d e f) =
    object[
      "Expected" .= a
      , "Item"   .= b
      , "ReturnConsumedCapacity" .= c
      , "ReturnItemCollectionMetrics" .= d
      , "ReturnValues" .= e
      , "TableName" .= f
      ]

data PutItemResponse
    = PutItemResponse {}
    deriving (Show,Eq)

putItem:: Item
          -> TableName
          -> Maybe Expected          
          -> Maybe ReturnConsumedCapacity
          -> Maybe ReturnItemCollectionMetrics
          -> Maybe ReturnValues
          -> PutItem
putItem a b c d e f= PutItem a b c d e f


data PutItemResult = PutItemResult{
  pirAttributes::Maybe Key
  , pirConsumedCapacity :: Maybe ConsumedCapacity
  , pirItemCollectionMetrics :: Maybe ItemCollectionMetrics
  }deriving(Show, Eq)
instance FromJSON PutItemResult where
  parseJSON (Object v) =
    PutItemResult <$>
    v .:? "Attributes"             <*>
    v .:? "ConsumedCapacity"       <*>
    v .:? "ItemCollectionMetrics"    

              

instance SignQuery PutItem where

    type ServiceConfiguration PutItem  = DdbConfiguration

    signQuery pi@PutItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.PutItem"
        , ddbqBody    = Just $ toJSON $ pi
        }

instance ResponseConsumer PutItem PutItemResponse where

    type ResponseMetadata PutItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (PutItemResult {}) = PutItemResponse{}


instance Transaction PutItem PutItemResponse

instance AsMemoryResponse PutItemResponse where

    type MemoryResponse PutItemResponse = PutItemResponse

    loadToMemory = return
