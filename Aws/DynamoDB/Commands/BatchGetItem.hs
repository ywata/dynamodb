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
import qualified Test.QuickCheck as QC


data BatchGetItem
    = BatchGetItem{
      
      } deriving (Show, Eq)
    

instance ToJSON BatchGetItem where
  toJSON (BatchGetItem) =
    object[
      ]
--instance FromJSON BatchGetItem where
--    parseJSON (Object v) = BatchGetItem <$>
--instance QC.Arbitrary BatchGetItem where
--    arbitrary = BatchGetItem <$>



data BatchGetItemResponse
    = BatchGetItemResponse {}
    deriving (Show,Eq)
instance ToJSON BatchGetItemResponse where
  toJSON (BatchGetItemResponse ) =
    object[
      ]
--instance FromJSON BatchGetItemResponse where
--    parseJSON (Object v) = BatchGetItemResponse <$>
--instance QC.Arbitrary BatchGetItemResponse where
--    arbitrary = BatchGetItemResponse <$>


batchGetItem :: BatchGetItem
batchGetItem = BatchGetItem



instance SignQuery BatchGetItem where

    type ServiceConfiguration BatchGetItem  = DdbConfiguration

    signQuery a@BatchGetItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.BatchGetItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data BatchGetItemResult = BatchGetItemResult{} deriving(Show, Eq)

instance FromJSON BatchGetItemResult where
 parseJSON _ = return BatchGetItemResult

instance ResponseConsumer BatchGetItem BatchGetItemResponse where

    type ResponseMetadata BatchGetItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (BatchGetItemResult {}) = BatchGetItemResponse{}


instance Transaction BatchGetItem BatchGetItemResponse

instance AsMemoryResponse BatchGetItemResponse where

    type MemoryResponse BatchGetItemResponse = BatchGetItemResponse

    loadToMemory = return

