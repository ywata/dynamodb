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
        }
    deriving (Show, Eq)

instance ToJSON ListTables where
  toJSON (ListTables) =
    object[
      ]


data ListTablesResponse
    = ListTablesResponse {}
    deriving (Show,Eq)


listTables :: ListTables
listTables= ListTables



instance SignQuery ListTables where

    type ServiceConfiguration ListTables  = DdbConfiguration

    signQuery a@ListTables {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.ListTables"
        , ddbqBody    = Just $ toJSON $ a
        }

data ListTablesResult = ListTablesResult{}
instance FromJSON ListTablesResult where
 parseJSON _ = return ListTablesResult

instance ResponseConsumer ListTables ListTablesResponse where

    type ResponseMetadata ListTablesResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (ListTablesResult {}) = ListTablesResponse{}


instance Transaction ListTables ListTablesResponse

instance AsMemoryResponse ListTablesResponse where

    type MemoryResponse ListTablesResponse = ListTablesResponse

    loadToMemory = return

