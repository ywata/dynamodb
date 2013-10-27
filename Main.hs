{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Aws
import Aws.DynamoDB
import qualified Aws.DynamoDB as D
import Network.HTTP.Conduit

--myCreateJob :: IO ()
myCreateJob = do
  cfg <- Aws.baseConfiguration
  rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ddb_cfg mgr $
                               D.createTable "abc" "def" my_preset my_pipeline
  return rsp

--my_ddb_cfg :: DdbConfiguration NormalQuery
--my_ddb_cfg = ddbLocalConfiguration HTTPS ddbEndpointEu

my_ddb_cfg :: DdbConfiguration NormalQuery
my_ddb_cfg  = D.ddbConfiguration HTTP D.ddbEndpointLocal

my_preset :: D.PresetId
my_preset = "1351620000001-000001"             -- System preset: Generic 720p
              
my_pipeline :: D.PipelineId
my_pipeline = "<one-of-ypour-pipeline-ids>"


main = myCreateJob



