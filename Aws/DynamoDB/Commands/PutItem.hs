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
import qualified Test.QuickCheck as QC


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
      "Item"        .= a
      , "TableName" .= b
      , "Expected"  .= c
      , "ReturnConsumedCapacity" .= d
      , "ReturnItemCollectionMetrics" .= e
      , "ReturnValues" .= f

      ]
instance FromJSON PutItem where
  parseJSON (Object v) = PutItem <$>
                         v .: "Item"       <*>  
                         v .:  "TableName" <*>  
                         v .:? "Expected"  <*>
                         v .:? "ReturnConsumedCapacity" <*>
                         v .:? "ReturnItemCollectionMetrics" <*>
                         v .:? "ReturnValues"

  parseJSON _          = mzero

instance QC.Arbitrary PutItem where
  arbitrary = PutItem <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary

data PutItemResponse = PutItemResponse{
  pirAttributes              :: Maybe Key
  , pirConsumedCapacity      :: Maybe ConsumedCapacity
  , pirItemCollectionMetrics :: Maybe ItemCollectionMetrics
  }deriving(Show, Eq)
instance ToJSON PutItemResponse where
  toJSON(PutItemResponse a b c) = object[
    "Attributes"              .= a
    , "ConsumedCapacity"      .= b
    , "ItemCollectionMetrics" .= c
    ]
instance QC.Arbitrary PutItemResponse where
  arbitrary = PutItemResponse <$> 
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary
instance FromJSON PutItemResponse where
  parseJSON (Object v) =
    PutItemResponse <$>
    v .:? "Attributes"             <*>
    v .:? "ConsumedCapacity"       <*>
    v .:? "ItemCollectionMetrics"    

putItem:: Item
          -> TableName
          -> Maybe Expected          
          -> Maybe ReturnConsumedCapacity
          -> Maybe ReturnItemCollectionMetrics
          -> Maybe ReturnValues
          -> PutItem
putItem a b c d e f= PutItem a b c d e f


data PutItemResult = PutItemResult{
  attributes              :: Maybe Key
  , consumedCapacity      :: Maybe ConsumedCapacity
  , itemCollectionMetrics :: Maybe ItemCollectionMetrics
  }deriving(Show, Eq)
instance FromJSON PutItemResult where
  parseJSON (Object v) =
    PutItemResult <$>
    v .:? "Attributes"             <*>
    v .:? "ConsumedCapacity"       <*>
    v .:? "ItemCollectionMetrics"    
              

instance SignQuery PutItem where

    type ServiceConfiguration PutItem = DdbConfiguration

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
        cnv (PutItemResult a b c) = PutItemResponse a b c


instance Transaction PutItem PutItemResponse

instance AsMemoryResponse PutItemResponse where

    type MemoryResponse PutItemResponse = PutItemResponse

    loadToMemory = return
