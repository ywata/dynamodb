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


data Query
    = Query
        {
        }
    deriving (Show, Eq)

instance ToJSON Query where
  toJSON (Query) =
    object[
      ]


data QueryResponse
    = QueryResponse {}
    deriving (Show,Eq)


query :: Query
query= Query



instance SignQuery Query where

    type ServiceConfiguration Query  = DdbConfiguration

    signQuery a@Query {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.Query"
        , ddbqBody    = Just $ toJSON $ a
        }

data QueryResult = QueryResult{}
instance FromJSON QueryResult where
 parseJSON _ = return QueryResult

instance ResponseConsumer Query QueryResponse where

    type ResponseMetadata QueryResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp -> cnv <$> jsonConsumer rsp
      where
        cnv (QueryResult {}) = QueryResponse{}


instance Transaction Query QueryResponse

instance AsMemoryResponse QueryResponse where

    type MemoryResponse QueryResponse = QueryResponse

    loadToMemory = return

