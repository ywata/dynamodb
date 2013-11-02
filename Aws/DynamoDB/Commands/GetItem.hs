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
import qualified Data.Text as T


data GetItem
    = GetItem
        {
        }
    deriving (Show, Eq)

instance ToJSON GetItem where
  toJSON (GetItem) =
    object[
      ]


data GetItemResponse
    = GetItemResponse {}
    deriving (Show,Eq)


getItem :: GetItem
getItem= GetItem



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
        cnv (GetItemResult {}) = GetItemResponse{}

instance Transaction GetItem GetItemResponse

instance AsMemoryResponse GetItemResponse where

    type MemoryResponse GetItemResponse = GetItemResponse

    loadToMemory = return

