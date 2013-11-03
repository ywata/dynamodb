{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.UpdateTable
    ( UpdateTable(..)
    , UpdateTableResponse(..)
    , updateTable
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T


data UpdateTable
    = UpdateTable
        {
        }
    deriving (Show, Eq)

instance ToJSON UpdateTable where
  toJSON (UpdateTable) =
    object[
      ]


data UpdateTableResponse
    = UpdateTableResponse {}
    deriving (Show,Eq)


updateTable :: UpdateTable
updateTable= UpdateTable



instance SignQuery UpdateTable where

    type ServiceConfiguration UpdateTable  = DdbConfiguration

    signQuery a@UpdateTable {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.UpdateTable"
        , ddbqBody    = Just $ toJSON $ a
        }

data UpdateTableResult = UpdateTableResult{}
instance FromJSON UpdateTableResult where
 parseJSON _ = return UpdateTableResult

instance ResponseConsumer UpdateTable UpdateTableResponse where

    type ResponseMetadata UpdateTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (UpdateTableResult {}) = UpdateTableResponse{}


instance Transaction UpdateTable UpdateTableResponse

instance AsMemoryResponse UpdateTableResponse where

    type MemoryResponse UpdateTableResponse = UpdateTableResponse

    loadToMemory = return

