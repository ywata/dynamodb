{-# LANGUAGE OverloadedStrings #-}
module Aws.DynamoDB.Json.Test
    ( 
      testAll
    , testAllVerbose
    ) where

import           Aws.DynamoDB.Json.Types
import           Text.Printf
import           Data.Aeson                     hiding(Value)
import qualified Data.Map                       as Map
import           Control.Applicative
import           Control.Monad
import qualified Test.QuickCheck                as QC
import qualified Distribution.TestSuite         as TS
import           Aws.DynamoDB.Commands

-- | Detailed-0.9/Cabal-1.14.0 test suite:
{-
tests :: [TS.Test] 
tests = map TS.impure simple_tests
-}

-- | Something to run in ghci:
test :: IO ()
test = testBase False
testAll = test
testAllVerbose :: IO ()
testAllVerbose = testBase True
testBase:: Bool -> IO ()
testBase b = mapM_ part simple_tests
      where
        part st = 
             do putStr $ printf "%-35s: " $ nameST st
                testST st b


simple_tests :: [SimpleTest]
simple_tests =
    [

      ae (mk_aet "AttributeValue"                       :: AETest AttributeValue             )
      , ae (mk_aet "CreateTable"                          :: AETest CreateTable                )
      
--      , ae (mk_aet "Item"     :: AETest Item        )
      , ae (mk_aet "AttributeValueUpdate"               :: AETest AttributeValueUpdate       )
      , ae (mk_aet "ActionType"                         :: AETest ActionType                 )
      , ae (mk_aet "AttributeDefinition"                :: AETest AttributeDefinition        )
      , ae (mk_aet "AttributeName"                      :: AETest AttributeName              )
      , ae (mk_aet "Condition"                          :: AETest Condition                  )        
      , ae (mk_aet "ConsistentRead"                     :: AETest ConsistentRead             )
      , ae (mk_aet "ConsumedCapacity"                   :: AETest ConsumedCapacity           )
      , ae (mk_aet "DateTime"                           :: AETest DateTime                   )
      , ae (mk_aet "DDouble"                            :: AETest DDouble                    )
      , ae (mk_aet "ExpectedAttributeValue"             :: AETest ExpectedAttributeValue     )
      , ae (mk_aet "IndexName"                          :: AETest IndexName                  )
      , ae (mk_aet "ItemCollectionMetrics"              :: AETest ItemCollectionMetrics      )
      , ae (mk_aet "KeyConditions"                      :: AETest KeyConditions              )
      , ae (mk_aet "KeyType"                            :: AETest KeyType                    )
      , ae (mk_aet "Keys"                               :: AETest Keys                       )
      , ae (mk_aet "KeySchemeElement"                   :: AETest KeySchemaElement           )
      , ae (mk_aet "Limit"                              :: AETest Limit                      )
      , ae (mk_aet "LocalSecondaryIndex"                :: AETest LocalSecondaryIndex        )
      , ae (mk_aet "LocalSecondaryIndexDescription"     :: AETest LocalSecondaryIndexDescription)      
      , ae (mk_aet "NonKeyAttribute"                    :: AETest NonKeyAttribute            )
      , ae (mk_aet "Operator"                           :: AETest ComparisonOperator         )
      , ae (mk_aet "Projection"                         :: AETest Projection                 )
      , ae (mk_aet "ProjectionType"                     :: AETest ProjectionType             )
      , ae (mk_aet "ProvisionedThroughput"              :: AETest ProvisionedThroughput      )
      , ae (mk_aet "ProvisionedThroughputDescription"   :: AETest ProvisionedThroughputDescription)
      , ae (mk_aet "ReturnConsumedCapacity"             :: AETest ReturnConsumedCapacity     )
      , ae (mk_aet "ReturnItemCollectionMetrics"        :: AETest ReturnItemCollectionMetrics)
      , ae (mk_aet "ReturnValues"                       :: AETest ReturnValues               )
      , ae (mk_aet "ScanFIlter"                         :: AETest ScanFilter                 )
      , ae (mk_aet "TableStatus"                        :: AETest TableStatus                )                        
      , ae (mk_aet "TableDescription"                   :: AETest TableDescription           ) 
      , ae (mk_aet "Value"                              :: AETest Value                      )

    ]
  where
    ae (AET st) = st


-- Our simple QC tests and instances to insert into the Cabal framework.

data SimpleTest = ST
    { nameST :: String
    , testST :: Bool -> IO QC.Result
    }
{-
instance TS.TestOptions SimpleTest where
    name           = nameST
    options        = const []
    defaultOptions = const $ return $ TS.Options []
    check          = const $ const $ return []

instance TS.ImpureTestable SimpleTest where
    runM st _ = cnv_result `fmap` testST st False
-}
cnv_result :: QC.Result -> TS.Result
cnv_result qcr =
        case qcr of
          QC.Success _ _ _ -> TS.Pass
          _                -> TS.Fail "Failed"

-- AETest adds a phantom index to SimpleTest as a convenience
-- to simplify usage of mk_aet, the generator for the
-- aeson-round-trip tests.

newtype AETest a = AET SimpleTest

-- Check aeson decode is an inverse of encode, namely that:
--
--       decode (encode [x]) == [x]
--
-- N.B. x rwapped in list as JSON not good for encoding simple
--      data types like strings and numbers and aeson strictly
--      adheres to the standard.

mk_aet :: (QC.Arbitrary a,Show a,FromJSON a,ToJSON a,Eq a) => String -> AETest a
mk_aet nm0 = tst $ \x -> maybe False (==[x]) $ decode $ encode [x]
      where
        nm     = "aeson." ++ nm0

        tst    :: (QC.Arbitrary a,Show a,FromJSON a,ToJSON a,Eq a) => 
                                                        (a->Bool) -> AETest a
        tst p  = AET $ ST nm $ qc p

        qc p v = if v then QC.verboseCheckResult p else QC.quickCheckResult p