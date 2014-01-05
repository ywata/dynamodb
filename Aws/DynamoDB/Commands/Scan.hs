{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.Scan
    ( Scan(..)
    , ScanResponse(..)
    , scan
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

data Scan
    = Scan{
      sTableName                :: TableName                     -- Yes
      , sAttributesToGet        :: Maybe AttributesToGet         -- No
      , sExclusiveStartKey      :: Maybe ExclusiveStartKey
      , sLimit                  :: Maybe Limit
      , sReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      , sScanFilter             :: Maybe ScanFilter
      , sSegment                :: Maybe Int
      , sSelect                 :: Maybe Select
      , sTotalSegments          :: Maybe Int
      } deriving (Show, Eq)
    

instance ToJSON Scan where
  toJSON (Scan a b c d e f g h i) =
    object[
      "TableName"                .= a
      , "AttributesToGet"        .= b
      , "ExclusiveStartKey"      .= c
      , "Limit"                  .= d
      , "ReturnConsumedCapacity" .= e
      , "ScanFilter"             .= f
      , "Segment"                .= g
      , "Select"                 .= h
      , "TotalSegments"          .= i
      ]
instance FromJSON Scan where
  parseJSON (Object v) = Scan <$>
                         v .: "TableName" <*>
                         v .:? "AttributesToGet" <*>
                         v .:? "ExclusiveStartKey" <*>
                         v .:? "Limit" <*>
                         v .:? "ReturnConsumedCapacity" <*>
                         v .:? "ScanFilter" <*>
                         v .:? "Segment" <*>
                         v .:? "Select" <*>
                         v .:? "TotalSegments"
                         
instance QC.Arbitrary Scan where
  arbitrary = Scan <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary


data ScanResponse
    = ScanResponse {
      srConsumedCapacity   :: Maybe ConsumedCapacity
      , srCount            :: Maybe Count
      , srItems            :: Maybe Items
      , srLastEvaluatedKey :: Maybe LastEvaluatedKey
      , srScannedCount     :: Maybe ScannedCount
      } deriving (Show,Eq)
instance ToJSON ScanResponse where
  toJSON (ScanResponse a b c d e) = object[
    "ConsumedCapacity"   .= a
    , "Count"            .= b
    , "Items"            .= c
    , "LastEvaluatedKey" .= d
    , "ScanResponse"     .= e
    ]

instance FromJSON ScanResponse where
  parseJSON (Object v) = ScanResponse <$>
                         v .:? "ConsumedCapacity" <*>
                         v .:? "Count" <*>                         
                         v .:? "Items" <*>       
                         v .:? "LastEvaluatedKey" <*>
                         v .:? "ScanResponse"      

instance QC.Arbitrary ScanResponse where  
  arbitrary = ScanResponse <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary

scan :: TableName
        -> Maybe AttributesToGet
        -> Maybe ExclusiveStartKey
        -> Maybe Limit
        -> Maybe ReturnConsumedCapacity
        -> Maybe ScanFilter
        -> Maybe Int        
        -> Maybe Select
        -> Maybe Int
        -> Scan
scan a b c d e f g h  = Scan a b c d e f g h 


instance SignQuery Scan where

    type ServiceConfiguration Scan  = DdbConfiguration

    signQuery a@Scan {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.Scan"
        , ddbqBody    = Just $ toJSON $ a
        }

data ScanResult = ScanResult{
  consumedCapacity   :: Maybe ConsumedCapacity
  , count            :: Maybe Count
  , items            :: Maybe Items
  , lastEvaluatedKey :: Maybe LastEvaluatedKey
  , scannedCount     :: Maybe ScannedCount
  } deriving(Show, Eq)
instance FromJSON ScanResult where
 parseJSON (Object v) = ScanResult <$>
                        v .:? "ConsumedCapacity" <*>
                        v .:? "Coount" <*>
                        v .:? "Items" <*>
                        v .:? "LastEvaluatedKey" <*>
                        v .:? "ScannedCount"
-- parseJSON a         = error $ show a


instance ResponseConsumer Scan ScanResponse where

    type ResponseMetadata ScanResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (ScanResult a b c d e) = ScanResponse a b c d e


instance Transaction Scan ScanResponse

instance AsMemoryResponse ScanResponse where

    type MemoryResponse ScanResponse = ScanResponse

    loadToMemory = return

