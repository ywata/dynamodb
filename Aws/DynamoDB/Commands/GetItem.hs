{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.GetItem
    ( GetItem(..)
    , GetItemResponse(..)
    , getItem
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

data GetItem
    = GetItem
        {
          giKey                :: Key              -- Yes
          , giTableName        :: TableName        -- Yes
          , giAttributesToGet  :: Maybe [T.Text]   -- No
          , giConsistentRead   :: Maybe Bool       -- No
          , giReturnConsumedCapacity :: Maybe Bool -- No
        }
    deriving (Show, Eq)

instance ToJSON GetItem where
  toJSON (GetItem a b c d e) =
    object[
      "Key"                      .= a
      , "TableName"              .= b        
      , "AttributesToGet"        .= c
      , "ConsistentRead"         .= d
      , "ReturnConsumedCapacity" .= e

      ]
instance FromJSON GetItem where
  parseJSON (Object v) = GetItem <$>
                         v .: "Key" <*>
                         v .: "TableName" <*>
                         v .:? "AttributesToGet" <*>
                         v .:? "ConsistentRead"  <*>
                         v .:? "ReturnConsumedCapacity"
                         
instance QC.Arbitrary GetItem where
  arbitrary = GetItem <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary

data GetItemResult = GetItemResult{
  consumedCapacity :: Maybe ConsumedCapacity,
  item             :: Maybe Item
  } deriving(Show, Eq)

instance FromJSON GetItemResult where
 parseJSON (Object v) = GetItemResult <$>
                        v .:? "ConsumedCapacity" <*>
                        v .:? "Item"


data GetItemResponse
    = GetItemResponse {
      girConsumedCapacity :: Maybe ConsumedCapacity,
      girItem             :: Maybe Item
      }deriving (Show,Eq)

instance ToJSON GetItemResponse where
  toJSON (GetItemResponse a b) = object[
    "ConsumedCapacity" .= a
    , "Item"           .= b
    ]
instance FromJSON GetItemResponse where
 parseJSON (Object v) = GetItemResponse <$>
                        v .:? "ConsumedCapacity" <*>
                        v .:? "Item"
instance QC.Arbitrary GetItemResponse where
  arbitrary = GetItemResponse <$>
              QC.arbitrary <*>
              QC.arbitrary
              

--getItem :: GetItem
getItem a b c d e = GetItem a b c d e 



instance SignQuery GetItem where

    type ServiceConfiguration GetItem  = DdbConfiguration

    signQuery a@GetItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.GetItem"
        , ddbqBody    = Just $ toJSON $ a
        }


instance ResponseConsumer GetItem GetItemResponse where

    type ResponseMetadata GetItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (GetItemResult a b) = GetItemResponse a b

instance Transaction GetItem GetItemResponse

instance AsMemoryResponse GetItemResponse where

    type MemoryResponse GetItemResponse = GetItemResponse

    loadToMemory = return

