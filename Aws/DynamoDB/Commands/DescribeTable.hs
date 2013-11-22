{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.DescribeTable
    ( DescribeTable(..)
    , DescribeTableResponse(..)
    , DescribeTableResult(..)      
    , describeTable
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import qualified Test.QuickCheck as QC


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

instance FromJSON DescribeTable where
  parseJSON (Object v) = DescribeTable <$> v .: "TableName"
  
instance QC.Arbitrary DescribeTable where
  arbitrary = DescribeTable <$> QC.arbitrary 

data DescribeTableResponse
    = DescribeTableResponse {dtrTableDescription::TableDescription}
    deriving (Show,Eq)
instance ToJSON DescribeTableResponse where
  toJSON (DescribeTableResponse a) = object[
    "Table" .= a]
instance FromJSON DescribeTableResponse where
  parseJSON (Object v) = DescribeTableResponse <$>
                         v .: "Table"
instance QC.Arbitrary DescribeTableResponse where  
  arbitrary = DescribeTableResponse <$> QC.arbitrary


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
    tableDescription::TableDescription
    }deriving(Show, Eq)
instance ToJSON DescribeTableResult where
  toJSON (DescribeTableResult a) = object[ "Table" .= a]
instance FromJSON DescribeTableResult where
 parseJSON (Object v) =
   DescribeTableResult <$>
   v .: "Table"
instance QC.Arbitrary DescribeTableResult where
  arbitrary = DescribeTableResult <$> QC.arbitrary
  

instance ResponseConsumer DescribeTable DescribeTableResponse where

    type ResponseMetadata DescribeTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (DescribeTableResult tbl) = DescribeTableResponse tbl


instance Transaction DescribeTable DescribeTableResponse

instance AsMemoryResponse DescribeTableResponse where

    type MemoryResponse DescribeTableResponse = DescribeTableResponse

    loadToMemory = return

