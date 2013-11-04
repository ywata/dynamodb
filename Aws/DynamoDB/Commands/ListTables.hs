{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.ListTables
    ( ListTables(..)
    , ListTablesResponse(..)
    , listTables
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T


data ListTables
    = ListTables
        {
          ltExclusivesStartTableName :: TableName
          , ltLimit :: Int
        }
    deriving (Show, Eq)

instance ToJSON ListTables where
  toJSON (ListTables a b) =
    object[
      "ExclusiveStartTableName" .= a
      , "Limit"                 .= b
      ]


data ListTablesResponse
    = ListTablesResponse {
      lastEvaluatedTableName::Maybe TableName
      , tableNames::[TableName]
                         }
    deriving (Show,Eq)


listTables :: TableName -> Int -> ListTables
listTables a b= ListTables a b

instance SignQuery ListTables where

    type ServiceConfiguration ListTables  = DdbConfiguration

    signQuery a@ListTables {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.ListTables"
        , ddbqBody    = Just $ toJSON $ a
        }

data ListTablesResult =
  ListTablesResult{
    ltrlastEvaluatedTableName :: Maybe TableName
    , ltrTableNames :: [TableName]
                  }deriving(Show, Eq)
instance FromJSON ListTablesResult where
 parseJSON (Object v) =
   ListTablesResult <$>
   v .:? "LastEvaluatedTableName" <*>
   v .: "TableNames"

instance ResponseConsumer ListTables ListTablesResponse where

    type ResponseMetadata ListTablesResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (ListTablesResult a b) = ListTablesResponse a b



instance Transaction ListTables ListTablesResponse

instance AsMemoryResponse ListTablesResponse where

    type MemoryResponse ListTablesResponse = ListTablesResponse

    loadToMemory = return

