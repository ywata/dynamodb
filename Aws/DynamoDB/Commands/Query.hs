{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.Query
    ( Query(..)
    , QueryResponse(..)
    , query
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

data Query
    = Query
        {
          qTableName                :: TableName
          , qAttributeToGet         :: Maybe AttributesToGet
          , qConsistentRead         :: Maybe ConsistentRead
          , qExclusiveStartKey      :: Maybe ExclusiveStartKey
          , qIndexName              :: Maybe IndexName
          , qKeyConditions          :: Maybe KeyConditions
          , qLimit                  :: Maybe Limit
          , qReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
          , qScanIndexForward       :: Maybe ScanIndexForward
          , qSelect                 :: Maybe Select
        }
    deriving (Show, Eq)

instance ToJSON Query where
  toJSON (Query a b c d e f g h i j) =
    object[
      "TableName"                .= a
      , "AttributesToGet"        .= b
      , "ConsistentRead"         .= c
      , "ExclusiveStartKey"      .= d
      , "IndexName"              .= e
      , "KeyConditions"          .= f
      , "Limit"                  .= g
      , "ReturnConsumedCapacity" .= h
      , "ScanIndexForward"       .= i
      , "Select"                 .= j
      ]
instance FromJSON Query where
  parseJSON (Object v) = Query <$>
                         v .: "TableName" <*>
                         v .:? "AttributesToGet" <*>
                         v .:? "ConsistentRead" <*>
                         v .:? "ExclusiveStartKey" <*>
                         v .:? "IndexName"<*>
                         v .:? "KeyConditions"<*>
                         v .:? "Limit"<*>
                         v .:? "ReturnConsumedCapacity"<*>
                         v .:? "ScanIndexForward"<*>
                         v .:? "Select"
instance QC.Arbitrary Query where
  arbitrary = Query <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary
                                                                      

data QueryResponse
    = QueryResponse {
      qrConsumedCapacity :: ConsumedCapacity
      , qrCount          :: Int
      , qrItems          :: [Item]
      , qrLastEvaluatedKey :: LastEvaluatedKey
      }deriving (Show,Eq)

instance ToJSON QueryResponse where
  toJSON (QueryResponse a b c d) =
    object[ "ConsumedCapacity"   .= a
            , "Count"            .= b
            , "Items"            .= c
            , "LastEvaluatedKey" .= d]
instance FromJSON QueryResponse where
 parseJSON (Object v) = QueryResponse <$>
                        v .: "ConsumedCapacity" <*>
                        v .: "Count" <*>
                        v .: "Items" <*>
                        v .: "LastEvaluatedKey"
instance QC.Arbitrary QueryResponse where
  arbitrary = QueryResponse <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary
              
  

--query :: Query
query :: TableName
      -> Maybe AttributesToGet
      -> Maybe ConsistentRead
      -> Maybe ExclusiveStartKey
      -> Maybe IndexName
      -> Maybe KeyConditions
      -> Maybe Limit
      -> Maybe ReturnConsumedCapacity
      -> Maybe ScanIndexForward
      -> Maybe Select
      -> Query
query a b c d e f g h i j = Query a b c d e f g h i j



instance SignQuery Query where

    type ServiceConfiguration Query  = DdbConfiguration

    signQuery a@Query {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.Query"
        , ddbqBody    = Just $ toJSON $ a
        }

data QueryResult = QueryResult{
  consumedCapacity :: ConsumedCapacity
  , count          :: Int
  , items          :: [Item]
  , lastEvaluatedKey :: LastEvaluatedKey
  } deriving(Show, Eq)
instance FromJSON QueryResult where
 parseJSON (Object v) = QueryResult <$>
                        v .: "ConsumedCapacity" <*>
                        v .: "Count" <*>
                        v .: "Items" <*>
                        v .: "LastEvaluatedKey"

instance ResponseConsumer Query QueryResponse where

    type ResponseMetadata QueryResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (QueryResult a b c d) = QueryResponse a b c d


instance Transaction Query QueryResponse

instance AsMemoryResponse QueryResponse where

    type MemoryResponse QueryResponse = QueryResponse

    loadToMemory = return

