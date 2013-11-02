{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.DeleteTable
    ( DeleteTable(..)
    , DeleteTableResponse(..)
    , deleteTable
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T


data DeleteTable
    = DeleteTable
        {
          delTableName :: TableName
        }
    deriving (Show, Eq)

instance ToJSON DeleteTable where
  toJSON (DeleteTable a) =
    object[
      "TableName" .= a
      ]


data DeleteTableResponse
    = DeleteTableResponse {}
    deriving (Show,Eq)


deleteTable :: TableName -> DeleteTable
deleteTable = DeleteTable



instance SignQuery DeleteTable where

    type ServiceConfiguration DeleteTable  = DdbConfiguration

    signQuery a@DeleteTable {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.DeleteTable"
        , ddbqBody    = Just $ toJSON $ a
        }

data DeleteTableResult = DeleteTableResult{}
instance FromJSON DeleteTableResult where
 parseJSON _ = return DeleteTableResult

instance ResponseConsumer DeleteTable DeleteTableResponse where

    type ResponseMetadata DeleteTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (DeleteTableResult {}) = DeleteTableResponse{}


instance Transaction DeleteTable DeleteTableResponse

instance AsMemoryResponse DeleteTableResponse where

    type MemoryResponse DeleteTableResponse = DeleteTableResponse

    loadToMemory = return

