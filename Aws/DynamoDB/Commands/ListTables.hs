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
import qualified Test.QuickCheck as QC

data ListTables
    = ListTables{
          ltExclusivesStartTableName :: Maybe TableName -- No
          , ltLimit                  :: Maybe Int       -- No
        }
    deriving (Show, Eq)

instance ToJSON ListTables where
  toJSON (ListTables a b) =
    object[
      "ExclusiveStartTableName" .= a
      , "Limit"                 .= b
      ]
instance FromJSON ListTables where
  parseJSON (Object v) = ListTables <$>
                         v .:? "ExclusiveStartTableName" <*>
                         v .:? "Limit"
instance QC.Arbitrary ListTables where
  arbitrary = ListTables <$> QC.arbitrary <*>  QC.arbitrary
data ListTablesResponse
    = ListTablesResponse {
      lastEvaluatedTableName :: Maybe TableName
      , tableNames           :: Maybe [TableName]
      }deriving (Show,Eq)
instance ToJSON ListTablesResponse where
  toJSON (ListTablesResponse a b) = object[
    "LastEvaluatedTableName" .= a
    ,  "TableNames"          .= b]
instance FromJSON ListTablesResponse where
  parseJSON (Object v) = ListTablesResponse <$>
                         v .:? "LastEvaluatedTableName" <*> {--}
                         v .:? "TableNames"
instance QC.Arbitrary ListTablesResponse where
  arbitrary = ListTablesResponse <$> QC.arbitrary  <*> QC.arbitrary

listTables :: Maybe TableName -> Maybe Int -> ListTables
listTables a b = ListTables a b

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
    , ltrTableNames           :: Maybe [TableName]
    }deriving(Show, Eq)
instance ToJSON ListTablesResult where
  toJSON (ListTablesResult a b) = object[
    "LastEvaluatedTableName" .= a
    , "TableNames"           .= b]
instance FromJSON ListTablesResult where
 parseJSON (Object v) =
   ListTablesResult <$>
   v .:? "LastEvaluatedTableName" <*>
   v .: "TableNames"
instance QC.Arbitrary ListTablesResult where
  arbitrary = ListTablesResult <$>
              QC.arbitrary <*>
              QC.arbitrary 


instance ResponseConsumer ListTables ListTablesResponse where

    type ResponseMetadata ListTablesResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (ListTablesResult a b) = ListTablesResponse a b



instance Transaction ListTables ListTablesResponse

instance AsMemoryResponse ListTablesResponse where

    type MemoryResponse ListTablesResponse = ListTablesResponse

    loadToMemory = return

