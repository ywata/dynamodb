{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.UpdateItem
    ( UpdateItem(..)
    , UpdateItemResponse(..)
    , updateItem
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import qualified Test.QuickCheck as QC


data UpdateItem
    = UpdateItem
        {
          uiKey                       :: Key
          , uiTableName               :: TableName
          , uiAttributeUpdates        :: Maybe AttributeValueUpdate
          , uiExpected                :: Maybe Expected
          , uiReturnConsumedCapacity  :: Maybe ReturnConsumedCapacity
          , uiReturnCollectionMetrics :: Maybe ReturnItemCollectionMetrics
          , uiReturnValue             :: Maybe ReturnValues
        }
    deriving (Show, Eq)

instance ToJSON UpdateItem where
  toJSON (UpdateItem a b c d e f g) =
    object[
      "Key"                .= a
      , "TableName"        .= b
      , "AttributeUpdates" .= c
      , "Expected"         .= d
      , "ReturnConsumedCapacity" .= e
      , "ReturnItemCollectionMetrics" .= f
      , "ReturnValues"                .= g
      ]
instance FromJSON UpdateItem where
    parseJSON (Object v) = UpdateItem <$>
                           v .: "Key"               <*>
                           v .: "TableName"         <*>
                           v .:? "AttributeUpdates" <*>
                           v .:? "Expected"         <*>
                           v .:? "ReturnConsumedCapacity" <*>
                           v .:? "ReturnItemCollectionMetrics" <*>
                           v .:? "ReturnValues"
instance QC.Arbitrary UpdateItem where
    arbitrary = UpdateItem <$>
                QC.arbitrary <*>
                QC.arbitrary <*>
                QC.arbitrary <*>
                QC.arbitrary <*>
                QC.arbitrary <*>
                QC.arbitrary <*>
                QC.arbitrary 

data UpdateItemResponse
    = UpdateItemResponse {
      uiAtributes               :: Maybe AttributeValueUpdate
      , uiConsumedCapacity      :: Maybe ConsumedCapacity
      , uiItemCollectionMetrics :: Maybe ItemCollectionMetrics
      } deriving (Show,Eq)
instance ToJSON UpdateItemResponse where
  toJSON (UpdateItemResponse a b c) =
    object[
      "Attributes"              .= a
      , "ConsumedCapacity"      .= b
      , "ItemCollectionMetrics" .= c
      ]

instance FromJSON UpdateItemResponse where
    parseJSON (Object v) = UpdateItemResponse <$>
                           v .: "Attributes" <*>
                           v .: "ConsumedCapacity" <*>
                           v .: "ItemCollectionMetrics"
instance QC.Arbitrary UpdateItemResponse where
    arbitrary = UpdateItemResponse <$>
                QC.arbitrary <*>
                QC.arbitrary <*>
                QC.arbitrary 


--updateItem :: UpdateItem
updateItem a b c d e f g = UpdateItem a b c d e f g

instance SignQuery UpdateItem where
    type ServiceConfiguration UpdateItem = DdbConfiguration

    signQuery a@UpdateItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.UpdateItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data UpdateItemResult = UpdateItemResult{
  attributes              :: Maybe AttributeValueUpdate
  , consumedCapacity      :: Maybe ConsumedCapacity
  , itemCollectionMetrics :: Maybe ItemCollectionMetrics
  } deriving(Show, Eq)

instance FromJSON UpdateItemResult where
 parseJSON (Object v) =  UpdateItemResult <$>
                         v .: "Attributes" <*>
                         v .: "ConsumedCapacity" <*>
                         v .: "ItemCollectionMetrics"
                         

instance ResponseConsumer UpdateItem UpdateItemResponse where

    type ResponseMetadata UpdateItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (UpdateItemResult a b c) = UpdateItemResponse a b c


instance Transaction UpdateItem UpdateItemResponse

instance AsMemoryResponse UpdateItemResponse where

    type MemoryResponse UpdateItemResponse = UpdateItemResponse

    loadToMemory = return

