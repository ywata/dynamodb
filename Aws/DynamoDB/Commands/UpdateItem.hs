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


data UpdateItem
    = UpdateItem
        {
        }
    deriving (Show, Eq)

instance ToJSON UpdateItem where
  toJSON (UpdateItem) =
    object[
      ]


data UpdateItemResponse
    = UpdateItemResponse {}
    deriving (Show,Eq)


updateItem :: UpdateItem
updateItem= UpdateItem



instance SignQuery UpdateItem where

    type ServiceConfiguration UpdateItem  = DdbConfiguration

    signQuery a@UpdateItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.UpdateItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data UpdateItemResult = UpdateItemResult{}
instance FromJSON UpdateItemResult where
 parseJSON _ = return UpdateItemResult

instance ResponseConsumer UpdateItem UpdateItemResponse where

    type ResponseMetadata UpdateItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (UpdateItemResult {}) = UpdateItemResponse{}


instance Transaction UpdateItem UpdateItemResponse

instance AsMemoryResponse UpdateItemResponse where

    type MemoryResponse UpdateItemResponse = UpdateItemResponse

    loadToMemory = return

