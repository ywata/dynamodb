module Aws.DynamoDB
    ( module Aws.DynamoDB.Commands
    , module Aws.DynamoDB.Core
    ) where

import Aws.DynamoDB.Commands
import Aws.DynamoDB.Core
--import Aws.DynamoDB.Json.Test
-- $use
--
-- A brief example createJob program
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


