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
import qualified Data.Text as T


data DeleteItem
    = DeleteItem
        {
        }
    deriving (Show, Eq)

instance ToJSON DeleteItem where
  toJSON (DeleteItem) =
    object[
      ]


data DeleteItemResponse
    = DeleteItemResponse {}
    deriving (Show,Eq)


deleteItem :: DeleteItem
deleteItem= DeleteItem



instance SignQuery DeleteItem where

    type ServiceConfiguration DeleteItem  = DdbConfiguration

    signQuery a@DeleteItem {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.DeleteItem"
        , ddbqBody    = Just $ toJSON $ a
        }

data DeleteItemResult = DeleteItemResult{}
instance FromJSON DeleteItemResult where
 parseJSON _ = return DeleteItemResult

instance ResponseConsumer DeleteItem DeleteItemResponse where

    type ResponseMetadata DeleteItemResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (DeleteItemResult {}) = DeleteItemResponse{}


instance Transaction DeleteItem DeleteItemResponse

instance AsMemoryResponse DeleteItemResponse where

    type MemoryResponse DeleteItemResponse = DeleteItemResponse

    loadToMemory = return

