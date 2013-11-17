{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.DescribeTable
    ( DescribeTable(..)
    , DescribeTableResponse(..)
    , describeTable
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T


data DescribeTable
    = DescribeTable
        {
          dtTableName:: TableName -- Yes
        }
    deriving (Show, Eq)

instance ToJSON DescribeTable where
  toJSON (DescribeTable a) =
    object[
      "TableName" .= a
      ]


data DescribeTableResponse
    = DescribeTableResponse {table::TableDescription}
    deriving (Show,Eq)


describeTable :: TableName -> DescribeTable
describeTable = DescribeTable 

instance SignQuery DescribeTable where

    type ServiceConfiguration DescribeTable  = DdbConfiguration

    signQuery a@DescribeTable {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.DescribeTable"
        , ddbqBody    = Just $ toJSON $ a
        }

data DescribeTableResult =
  DescribeTableResult{
    dtrTable::TableDescription
    }deriving(Show, Eq)
instance FromJSON DescribeTableResult where
 parseJSON (Object v) =
   DescribeTableResult <$>
   v .: "Table"


instance ResponseConsumer DescribeTable DescribeTableResponse where

    type ResponseMetadata DescribeTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (DescribeTableResult tbl) = DescribeTableResponse tbl


instance Transaction DescribeTable DescribeTableResponse

instance AsMemoryResponse DescribeTableResponse where

    type MemoryResponse DescribeTableResponse = DescribeTableResponse

    loadToMemory = return

