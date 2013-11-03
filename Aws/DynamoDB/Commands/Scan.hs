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


data Scan
    = Scan
        {
        }
    deriving (Show, Eq)

instance ToJSON Scan where
  toJSON (Scan) =
    object[
      ]


data ScanResponse
    = ScanResponse {}
    deriving (Show,Eq)


scan :: Scan
scan= Scan



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

