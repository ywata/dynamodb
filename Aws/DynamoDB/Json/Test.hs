module Aws.ElasticTranscoder.Json.Test
    ( 
      testAll
    , testAllVerbose
    ) where

import           Aws.DynamoDB.Json.Types
import           Text.Printf
import           Data.Aeson
import qualified Test.QuickCheck                as QC
import qualified Distribution.TestSuite         as TS


-- | Detailed-0.9/Cabal-1.14.0 test suite:
{-
tests :: [TS.Test] 
tests = map TS.impure simple_tests
-}

-- | Something to run in ghci:

testAll :: IO ()
testAll = mapM_ part simple_tests
      where
        part st = 
             do putStr $ printf "%-35s: " $ nameST st
                testST st False


-- | Verbose tests from ghci:

testAllVerbose :: IO ()
testAllVerbose = mapM_ part simple_tests
      where
        part st = 
             do putStr $ printf "\n%s:\n" $ nameST st
                testST st True


-- Our list of tests:

simple_tests :: [SimpleTest]
simple_tests =
    [ ae (mk_aet "IndexName"                          :: AETest IndexName          ),
      ae (mk_aet "KeyType"                            :: AETest KeyType            ),
      ae (mk_aet "Limit"                              :: AETest Limit              ),
      ae (mk_aet "AttributeValue"                     :: AETest AttributeValue     ),
      ae (mk_aet "ActionType"                         :: AETest ActionType         ),
      ae (mk_aet "AttributeDefinition"                :: AETest AttributeDefinition),
      ae (mk_aet "AttributeName"                      :: AETest AttributeName),
      ae (mk_aet "KeySchema"                          :: AETest KeySchemaElement)
      
--      ae (mk_aet "LocalSecondaryIndex"                          :: AETest LocalSecondaryIndex),      
--      ae (mk_aet "CreationDateTime"                   :: AETest CreationDateTime),
--      ae (mk_aet "TableDescription"                   :: AETest TableDescription)      
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
