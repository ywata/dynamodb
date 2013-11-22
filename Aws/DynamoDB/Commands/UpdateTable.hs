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
import qualified Test.QuickCheck as QC


data UpdateTable
    = UpdateTable
        {
          provisionedThroughPut :: ProvisionedThroughput  {- Yes -}
          , tableName           :: TableName              {- Yes -}
        }
    deriving (Show, Eq)

instance ToJSON UpdateTable where
  toJSON (UpdateTable a b) =
    object[
      "ProjectionedThroughPut" .= a
      , "TableName"            .= b
      ]
instance FromJSON UpdateTable where
    parseJSON (Object v) = UpdateTable <$>
                           v .: "ProjectionedThroughPut" <*>
                           v .: "TableName"
    
instance QC.Arbitrary UpdateTable where
    arbitrary = UpdateTable <$>
                QC.arbitrary <*>
                QC.arbitrary

data UpdateTableResponse
    = UpdateTableResponse {
      utrTableDescription :: Maybe TableDescription
      }deriving (Show,Eq)
    
instance ToJSON UpdateTableResponse where
  toJSON (UpdateTableResponse a) =
    object[
      "TableDescription" .= a
      ]
instance FromJSON UpdateTableResponse where
    parseJSON (Object v) = UpdateTableResponse <$>
                           v .:? "TableDescription"
instance QC.Arbitrary UpdateTableResponse where
    arbitrary = UpdateTableResponse <$> QC.arbitrary


updateTable :: ProvisionedThroughput -> TableName -> UpdateTable
updateTable a b = UpdateTable a b 



instance SignQuery UpdateTable where
    type ServiceConfiguration UpdateTable  = DdbConfiguration

    signQuery a@UpdateTable {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.UpdateTable"
        , ddbqBody    = Just $ toJSON $ a
        }

data UpdateTableResult = UpdateTableResult{
  tableDescription :: Maybe TableDescription  
  }deriving(Show, Eq)
instance ToJSON UpdateTableResult where
  toJSON (UpdateTableResult a) = object[
    "TableDescription" .= a
    ]
instance FromJSON UpdateTableResult where
 parseJSON (Object v) = UpdateTableResult <$>
                        v .:? "UpdateTableResult"

instance ResponseConsumer UpdateTable UpdateTableResponse where

    type ResponseMetadata UpdateTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (UpdateTableResult a) = UpdateTableResponse a


instance Transaction UpdateTable UpdateTableResponse

instance AsMemoryResponse UpdateTableResponse where

    type MemoryResponse UpdateTableResponse = UpdateTableResponse

    loadToMemory = return

