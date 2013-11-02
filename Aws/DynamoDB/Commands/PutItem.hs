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
import           Data.Aeson
import qualified Data.Text as T
--import           Aws.DynamoDB.Json.Types

data PutItem
    = PutItem
        {
          piExpected::Expected
          , piItem ::Item
          , piReturnConsumedCapacity::ReturnConsumedCapacity
          , piReturnItemCollectionMetrics:: ReturnItemCollectionMetrics
          , piReutrnValues :: T.Text
          , piTableName :: TableName
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

putItem:: Expected -> Item -> ReturnConsumedCapacity -> ReturnItemCollectionMetrics -> T.Text -> TableName -> PutItem
putItem a b c d e f= PutItem a b c d e f



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
