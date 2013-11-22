{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.DeleteItem
    ( DeleteItem(..)
    , DeleteItemResponse(..)
    , deleteItem
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

data DeleteItem
    = DeleteItem{
      dikey                           :: Key                             -- Yes
      , diTableName                   :: TableName                         -- Yes        
      , diExpected                    :: Maybe Expected                    -- No
      , diReturnConsumedCapacity      :: Maybe ReturnConsumedCapacity      -- No
      , diReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics -- No
      , diReturnValues                :: Maybe ReturnValues                -- No
      }
    deriving (Show, Eq)

instance ToJSON DeleteItem where
  toJSON (DeleteItem a b c d e f) =
    object[
      "Key"                           .= a
      , "TableName"                   .= b
      , "Expected"                    .= c
      , "ReturnConsumedCapacity"      .= d
      , "ReturnItemCollectionMetrics" .= e
      , "ReturnValues"                .= f
      ]

instance FromJSON DeleteItem where
  parseJSON (Object v) = DeleteItem <$>
                         v .: "Key" <*>
                         v .: "TableName" <*>
                         v .:? "Expected" <*>
                         v .:? "ReturnConsumedCapacity" <*>
                         v .:? "ReturnItemCollectionMetrics" <*>
                         v .:? "ReturnValues"
                         
instance QC.Arbitrary DeleteItem where
  arbitrary = DeleteItem <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary
  
data DeleteItemResponse
    = DeleteItemResponse {
      dirArttributes             :: Maybe Attributes
      , dirConsumedCapacity      :: Maybe ConsumedCapacity
      , dirItemCollectionMetrics :: Maybe ItemCollectionMetrics
      }deriving (Show,Eq)
instance ToJSON DeleteItemResponse where
  toJSON (DeleteItemResponse a b c) = object[
    "Attributes"              .= a
    , "ConsumedCapacity"      .= b
    , "ItemCollectionMetrics" .= c
    ]
instance FromJSON DeleteItemResponse where
  parseJSON (Object v) = DeleteItemResponse <$>
                         v .:? "Attributes" <*>
                         v .:? "ConsumedCapacity" <*>
                         v .:? "ItemCollectionMetrics"
    
instance QC.Arbitrary DeleteItemResponse where  
  arbitrary = DeleteItemResponse <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary 
  


deleteItem :: Key
              -> TableName
              -> Maybe Expected
              -> Maybe ReturnConsumedCapacity
              -> Maybe ReturnItemCollectionMetrics
              -> Maybe ReturnValues
              -> DeleteItem
deleteItem a b c d e f = DeleteItem a b c d e f


instance SignQuery DeleteItem where

    type ServiceConfiguration DeleteItem  = DdbConfiguration

    signQuery a@DeleteItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.DeleteItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data DeleteItemResult = DeleteItemResult{
  diAttributes            :: Maybe Attributes
  , consumedCapacity      :: Maybe ConsumedCapacity
  , itemCollectionMetrics :: Maybe ItemCollectionMetrics
  }deriving(Show, Eq)
instance FromJSON DeleteItemResult where
 parseJSON (Object v) = DeleteItemResult <$>
                        v .: "Attributes" <*>
                        v .: "ConsumedCapacity" <*>
                        v .: "ItemCollectionMetrics"
                        

instance ResponseConsumer DeleteItem DeleteItemResponse where

    type ResponseMetadata DeleteItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (DeleteItemResult a b c) = DeleteItemResponse a b c


instance Transaction DeleteItem DeleteItemResponse

instance AsMemoryResponse DeleteItemResponse where

    type MemoryResponse DeleteItemResponse = DeleteItemResponse

    loadToMemory = return

