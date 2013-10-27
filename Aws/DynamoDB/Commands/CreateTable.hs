{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.DynamoDB.Commands.CreateTable
    ( CreateTable(..)
    , CreateTableResponse(..)
    , createTable
    , defaultJSInput
    , defaultJSOutput
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson


-- | A brief example createJob program
--
-- @
--      {-# LANGUAGE OverloadedStrings  #-}
--      
--      import           Aws
--      import           Aws.DynamoDB
--      import           Network.HTTP.Conduit
--      
--      myCreateJob :: IO ()
--      myCreateJob = 
--       do cfg <- Aws.baseConfiguration
--          rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
--                      createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
--          print rsp
--        
--      my_ets_cfg :: EtsConfiguration NormalQuery
--      my_ets_cfg = etsConfiguration HTTPS etsEndpointEu
--              
--      my_preset :: PresetId
--      my_preset = "1351620000001-000001"             -- System preset: Generic 720p
--              
--      my_pipeline :: PipelineId
--      my_pipeline = "<one-of-ypour-pipeline-ids>"
-- @


data CreateTable
    = CreateTable
        { cjInput      :: JSInput 
        , cjOutput     :: JSOutput
        , cjPipelineId :: PipelineId
        }
    deriving (Show,Eq)

data CreateTableResponse
    = CreateTableResponse
        { cjrId         :: JobId
        , cjrInput      :: JSInput 
        , cjrOutput     :: JSOutputStatus
        , cjrPipelineId :: PipelineId
        }
    deriving (Show,Eq)


createTable :: S3Object -> S3Object -> PresetId -> PipelineId -> CreateTable
createTable inb oub pri pli = CreateTable cji cjo pli
  where
    cji = defaultJSInput  inb
    cjo = defaultJSOutput oub pri


defaultJSInput :: S3Object -> JSInput
defaultJSInput inb = JSInput inb FRauto Rauto ARauto ABauto Cauto

defaultJSOutput :: S3Object -> PresetId -> JSOutput
defaultJSOutput oub pri = JSOutput oub "" ROTauto pri


instance SignQuery CreateTable where

    type ServiceConfiguration CreateTable = DdbConfiguration

    signQuery CreateTable {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = "jobs"
        , ddbqQuery   = []
        , ddbqBody    = Just $ toJSON $ JobSpec cjInput cjOutput cjPipelineId
        }

instance ResponseConsumer CreateTable CreateTableResponse where

    type ResponseMetadata CreateTableResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (JobSingle(JobSpecId a b c d)) = CreateTableResponse a b c d

instance Transaction CreateTable CreateTableResponse

instance AsMemoryResponse CreateTableResponse where

    type MemoryResponse CreateTableResponse = CreateTableResponse

    loadToMemory = return
