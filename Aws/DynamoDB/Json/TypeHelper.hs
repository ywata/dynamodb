{-# LANGUAGE OverloadedStrings          #-}
module Aws.DynamoDB.Json.TypeHelper where

import Debug.Trace
import          Prelude hiding (lookup, keys)

import           Data.Maybe
import           Control.Monad
import           Control.Applicative

import           Data.String

--import           Test.QuickCheck.Arbitrary.ToolShed.Test.QuickCheck.Arbitrary.Map
--import           ToolShed.Test.QuickCheck.Arbitrary.Map

import qualified Data.Map.Lazy                  as Map
import qualified Data.HashMap.Lazy              as H

import           Data.Attoparsec.Number         (Number(..))
import           Data.Aeson                     hiding (Value)
--import qualified Data.Aeson.Functions           as A (mapHashKey)
import qualified Data.Aeson.Types               as A 
import qualified Data.Text                      as T
import qualified Test.QuickCheck                as QC
import           Safe

import qualified Data.Vector                    as V
import           Aws.DynamoDB.Json.BasicTypes

--------------
decodeValue :: FromJSON a => A.Value ->  Map.Map T.Text a
decodeValue (Object o) = Map.fromList . H.toList
                       $ H.map unRight
                       $ H.filter isRight 
                       $ H.map (eitherDecode . encode) o
  where
    isRight(Right _) = True
    isRight _        = False
    unRight (Right x) = x
    
decodeValue _ = error "decodeValue mismatch."
    
parseObjectByName :: (FromJSON a) => A.Value -> T.Text -> A.Parser (Maybe a)
parseObjectByName o t = case o of
  (Object v) -> v .:? t
  _          -> mzero


lookup :: T.Text -> A.Value -> Maybe A.Value
lookup k (Object v) = H.lookup k v
lookup k x          = error $ show x ++ "\n" ++ show k

keys :: A.Value -> Maybe [T.Text]
keys (Object v) = Just . H.keys $ v
keys _          = Nothing

take1 :: [a] -> Maybe a
take1 [] = Nothing
take1 (x:xs) = Just x

deepValue :: A.Value -> [T.Text] -> Maybe A.Value
deepValue a ts = deepObj a ts Nothing
  where
    deepObj _ [] r   = r
    deepObj v (x:xs) r = do
      o' <- lookup x v
      deepObj o' xs (Just o')


--------------
--------- Code below are derived from aws-elastictranscoding
-------------
data DdbServiceError = DDB { message :: T.Text, type_::T.Text }
    deriving (Show,Eq)

instance FromJSON DdbServiceError where
    parseJSON (Object v) = DDB <$> v .: "Message" <*> v .: "__type"
    parseJSON _          = mzero

instance ToJSON DdbServiceError where
    toJSON (DDB msg_ type_) =
        object
            [ "Message"   .= msg_
              , "__type"  .= type_
            ]
 
instance QC.Arbitrary DdbServiceError where
    arbitrary = DDB . T.pack <$> QC.arbitrary <*> QC.arbitrary

--
-- | 'success'
--

newtype SUCCESS = SUCCESS { _SUCCESS :: Bool }
    deriving (Show,Eq)

instance FromJSON SUCCESS where
    parseJSON (Object v) = SUCCESS <$> v .: "success"
    parseJSON _          = mzero

instance ToJSON SUCCESS where
    toJSON = Bool . _SUCCESS

instance QC.Arbitrary SUCCESS where
    arbitrary = SUCCESS <$> QC.arbitrary



  
------------------------------------------------------------------------------
--
-- Parser Toolkit
--
------------------------------------------------------------------------------


json_str_map_p :: Ord a => Map.Map T.Text a -> A.Value -> A.Parser a
json_str_map_p mp = json_string_p $ flip Map.lookup mp 

json_string_p :: Ord a => (T.Text->Maybe a) -> A.Value -> A.Parser a
json_string_p p (String t) | Just val <- p t = return val
                           | otherwise       = mzero
json_string_p _  _                           = mzero

text_map :: (Ord a,Bounded a,Enum a) => (a->T.Text) -> Map.Map T.Text a
text_map f = Map.fromList [ (f x,x) | x<-[minBound..maxBound] ]

read_p :: Read a => T.Text -> A.Parser a
read_p txt = maybe mzero return $ readMay $ T.unpack txt


------------------------------------------------------------------------------
--
-- QC Toolkit
--
------------------------------------------------------------------------------


poss :: QC.Gen a -> QC.Gen (Maybe a)
poss gen = QC.frequency 
    [ (,) 1  $ QC.elements [Nothing]
    , (,) 20 $ Just <$> gen
    ]

nat_pair :: QC.Gen (Int,Int)
nat_pair = two $ QC.sized $ \n -> QC.choose (0, n)

two :: QC.Gen a -> QC.Gen (a,a)
two gen = (,) <$> gen <*> gen


