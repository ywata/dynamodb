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

data NotYet = NotYet deriving(Show, Eq)
data Scan
    = Scan
      {
        sTableName                  :: TableName                     -- Yes
          , sAttributeToGet         :: Maybe AttributeToGet
          , sExclusiveStartKey      :: Maybe ExclusiveStartKey
          , sLimit                  :: Maybe Limit
          , sReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
          , sScanFilter             :: Maybe NotYet -- ScanFilter
          , sSegment                :: Maybe Int
          , sSelect                 :: Maybe Select
          , sTotalSegments          :: Maybe Int
        }
    deriving (Show, Eq)

instance ToJSON Scan where
  toJSON (Scan a b c d e f g h i) =
    object[
      ]


data ScanResponse
    = ScanResponse {}
    deriving (Show,Eq)


scan :: TableName
        -> Maybe AttributeToGet
        -> Maybe ExclusiveStartKey
        -> Maybe Limit
        -> Maybe ReturnConsumedCapacity
        -> Maybe NotYet
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

data ScanResult = ScanResult{}
instance FromJSON ScanResult where
 parseJSON _ = return ScanResult

instance ResponseConsumer Scan ScanResponse where

    type ResponseMetadata ScanResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (ScanResult {}) = ScanResponse{}


instance Transaction Scan ScanResponse

instance AsMemoryResponse ScanResponse where

    type MemoryResponse ScanResponse = ScanResponse

    loadToMemory = return

