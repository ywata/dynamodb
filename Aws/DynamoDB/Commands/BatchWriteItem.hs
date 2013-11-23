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
import qualified Test.QuickCheck as QC


data BatchWriteItem
    = BatchWriteItem
        {
        }
    deriving (Show, Eq)

instance ToJSON BatchWriteItem where
  toJSON (BatchWriteItem) =
    object[
      ]
--instance FromJSON BatchWriteItem where
--    parseJSON (Object v) = BatchWriteItem <$>
--instance QC.Arbitrary BatchWriteItem where
--    arbitrary = BatchWriteItem <$>



data BatchWriteItemResponse
    = BatchWriteItemResponse {}
    deriving (Show,Eq)
instance ToJSON BatchWriteItemResponse where
  toJSON (BatchWriteItemResponse) =
    object[
      ]
--instance FromJSON BatchWriteItemResponse where
--    parseJSON (Object v) = BatchWriteItemResponse <$>
--instance QC.Arbitrary BatchWriteItemResponse where
--    arbitrary = BatchWriteItemResponse <$>


batchWriteItem :: BatchWriteItem
batchWriteItem = BatchWriteItem



instance SignQuery BatchWriteItem where

    type ServiceConfiguration BatchWriteItem  = DdbConfiguration

    signQuery a@BatchWriteItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.BatchWriteItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data BatchWriteItemResult = BatchWriteItemResult{} deriving(Show, Eq)

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

