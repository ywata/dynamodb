{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE EmptyDataDecls             #-} 
module Aws.DynamoDB.Json.Types
    (
      AttributeDefinition(..)
      , Attributes(..)
      , AttributeValue(..)
--      , ConsumedCapacity(..)
      , CreateTableResult(..)
      , ExclusiveTableName(..)
      , Expected(..)
      , DdbServiceError(..)
      , GetItemResult(..)        
--      , ItemCollectionMetrics(..)
      , Item(..)
      , KeySchema(..)
      , Limit(..)
      , KeySchemaElement(..)
      , LocalSecondaryIndex(..)
      , ProvisionedThroughput(..)

      , PutItemResult(..)
      , ReturnConsumedCapacity(..)
      , ReturnItemCollectionMetrics(..)
      , TableDescription(..)
      , TableName(..)
--       
    ) where

import           Control.Monad
import           Control.Applicative
import           Text.Printf
import           Text.Regex
import           Data.String

import qualified Data.Map                       as Map
import           Data.Aeson
import qualified Data.Aeson.Types               as A
import qualified Data.Text                      as T
import qualified Test.QuickCheck                as QC
import           Safe


type TableName = T.Text

--
-- | AttributeDefinition
--
data AttributeDefinition = AttributeDefinition{
  attributeName :: T.Text, 
  attributeType :: AttributeValue
  } deriving(Show, Eq)

instance ToJSON AttributeDefinition where
  toJSON ad@(AttributeDefinition _ _) =
    object [
      "AttributeName" .= attributeName ad,
      "AttributeType" .= attributeType ad
      ]

instance FromJSON AttributeDefinition where
  parseJSON (Object v) =
    AttributeDefinition <$>
      v .: "AttributeName" <*>
      v .: "AttributeType"
  parseJSON _ = mzero

--
-- | AttributeName  
--
newtype AttributeName = AttributeName{_AttributeName :: T.Text}
                        deriving(Show, IsString, Eq)
instance ToJSON AttributeName where
  toJSON (AttributeName a) = String a



data AttributeValue = B | BS | N | NS | S | SS
                    deriving (Show, Eq, Ord, Bounded,Enum)

attributeValue_t ::AttributeValue -> T.Text
attributeValue_t av =
  case av of
    B  -> "B"
    BS -> "BS"
    N  -> "N"
    NS -> "NS"
    S  -> "S"
    SS -> "SS"
attributeValue_m :: Map.Map T.Text AttributeValue
attributeValue_m = text_map attributeValue_t
instance ToJSON AttributeValue where
  toJSON = String . attributeValue_t
instance FromJSON AttributeValue where
  parseJSON = json_str_map_p attributeValue_m
instance QC.Arbitrary AttributeValue where
  arbitrary = QC.elements [minBound..maxBound]

data ActionType = ADD | PUT | DELETE
                              deriving(Show, Eq, Ord, Bounded, Enum)
actionType_t :: ActionType -> T.Text
actionType_t a =
  case a of
    ADD -> "ADD"
    PUT -> "PUT"
    DELETE -> "DELETE"
actionType_m :: Map.Map T.Text ActionType
actionType_m = text_map actionType_t

instance ToJSON ActionType where
  toJSON = String . actionType_t
instance FromJSON ActionType where
  parseJSON = json_str_map_p actionType_m

instance QC.Arbitrary ActionType  where
  arbitrary = QC.elements [minBound, maxBound]

data Attributes = Attributes{
  }

data AttributeValueUpdate = Action{actionType:: ActionType}|
                            Value AttributeValue
instance ToJSON AttributeValueUpdate where
  toJSON Action{actionType =a}   = object ["actionType" .= a]
  toJSON v@(Value av) = toJSON av

data ConsumedCapacity = ConsumedCapacity{
  ccCacacityUnits :: Int,
  ccTableName     :: TableName
  }
newtype ConsistentRead = ConsistentRead Bool
  deriving(Show, Eq)
instance ToJSON ConsistentRead where
  toJSON (ConsistentRead a) = object["ConsistentRead" .= a]

data Unknown = Unknown{}

data Condition = Condition{}
                          
data Operator
  = EQ_ |NE_| LE_ | LT_ | GE_ | GT_ | BEGIN_WITH_ | BETWEEN_ | NOT_NULL_ | NULL_ |CONTAINS_| NOT_CONTAINS_| IN_
  deriving(Show, Eq, Ord, Bounded, Enum)
operator_t :: Operator -> T.Text
operator_t op =
  case op of
    EQ_ -> "EQ"
    NE_ -> "NE"
    LE_ -> "LE"
    GE_ -> "GE"
    GT_ -> "GT"
    BEGIN_WITH_ -> "BEGIN_WITH"
    BETWEEN_    -> "BETWEEN"
    NOT_NULL_   -> "NOT_NULL"
    NULL_       -> "NULL"
    CONTAINS_   -> "CONTAINS"
    NOT_CONTAINS_ -> "NOT_CONTAINS"
    IN_           -> "IN"

data CreateTableResult = CreateTableResult{
  tableDescription::TableDescription
  }deriving(Show, Eq)
instance FromJSON CreateTableResult where
  parseJSON (Object v) =
    CreateTableResult <$> v .: "TableDescription" 
    
  parseJSON _ = mzero

data PutItemResult = PutItemResult
                     deriving(Show, Eq)
instance FromJSON PutItemResult where
  parseJSON a = return PutItemResult

data GetItemResult = GetItemResult{}
instance FromJSON GetItemResult where
 parseJSON _ = return GetItemResult


type DateTime = Int
--newtype DateTime = DateTime{_DateTime :: Int}
--                 deriving(Show, Eq)
--instance FromJSON DateTime where
--  parseJSON = withText "DateTime" $ return . DateTime 

type KeySchema = [KeySchemaElement]

data DescribeTableResult
data ExpectedAttributeValue


newtype IndexName = IndexName{_IndexName :: T.Text}
                  deriving(Show, Eq)
instance FromJSON IndexName where
  parseJSON = withText "IndexName" $ return . IndexName
instance ToJSON IndexName where
  toJSON = String . _IndexName

instance QC.Arbitrary IndexName where
  arbitrary = IndexName . T.pack <$> QC.arbitrary
  
  
data KeysAndAttributes  
data KeySchemaElement = KeySchemaElement{
  kseAttributeName :: AttributeName
  , kseKeyType     :: KeyType
  }deriving(Show, Eq)
instance ToJSON KeySchemaElement where
  toJSON (KeySchemaElement a b) = object[
    "AttributeName" .= toJSON a
    , "KeyType"     .= toJSON b]

newtype KeyType = KeyType{_KeyType :: T.Text}
                  deriving(Show, IsString, Eq)
instance FromJSON KeyType where
  parseJSON = withText "KeyType" $ return . KeyType
instance ToJSON KeyType where
  toJSON = String . _KeyType
instance QC.Arbitrary KeyType where
  arbitrary = KeyType . T.pack <$> QC.arbitrary

newtype Limit = Limit{_Limit::Int}
             deriving(Show, Eq)
instance ToJSON Limit where
  toJSON a = object["Limit" .= _Limit a]

data ListTablesResult
data LocalSecondaryIndex = LocalSecondaryIndex{
  lsiIndexName:: IndexName
  , lsiKeySchema :: KeySchema
  , lsiProjection:: Projection
  }deriving(Show, Eq)
  
data LocalSecondaryIndexDescription

data NonKeyAttribute = NonKeyAttribute{type_::[T.Text]}
                       deriving(Show, Eq)
  
  
data Projection = Projection{
  pNonKeyAttribute::[NonKeyAttribute]
  , pProjectionType::[ProjectionType]
  }deriving(Show, Eq)
data ProjectionType = ALL_ | KEYS_ONLY_ | INCLUDE_
                                          deriving(Show, Eq, Ord, Bounded, Enum)
projectionType_t:: ProjectionType -> T.Text
projectionType_t pt =
  case pt of
    ALL_ -> "ALL"
    KEYS_ONLY_ -> "KEYS_ONLY"
    INCLUDE_ -> "INCLUDE"
projectionType_m :: Map.Map T.Text ProjectionType
projectionType_m = text_map projectionType_t


data ProvisionedThroughput = ProvisionedThroughput{
  ptReadCapacityUnits::Int
  , ptWriteCapacityUnits::Int
  , ptLastDecreasedTime :: Maybe DateTime
  , ptLastIncreasedTime :: Maybe DateTime
  , ptNumberOfDecreasedToday :: Maybe DateTime
  }deriving(Show, Eq)
instance ToJSON ProvisionedThroughput where
  toJSON (ProvisionedThroughput a b c d e) =
    object[
      "ReadCapacityUnits"    .= a
      , "WriteCapacityUnits" .= b
      ]


data ProvisionedThroughputDescription

data PutRequest
data QueryResult
data ReturnConsumedCapacity = TOTAL | NONE
                            deriving(Show, Eq, Ord, Bounded, Enum)
returnConsumedCapacity_t:: ReturnConsumedCapacity -> T.Text
returnConsumedCapacity_t a =
  case a of
    TOTAL -> "TOTAL"
    NONE  -> "NONE"
    
returnConsumedCapacity_m :: Map.Map T.Text ReturnConsumedCapacity
returnConsumedCapacity_m = text_map returnConsumedCapacity_t
instance ToJSON ReturnConsumedCapacity where
  toJSON = String . returnConsumedCapacity_t
instance FromJSON ReturnConsumedCapacity where
  parseJSON = json_str_map_p returnConsumedCapacity_m
instance QC.Arbitrary ReturnConsumedCapacity where
  arbitrary = QC.elements [minBound..maxBound]

data ReturnItemCollectionMetrics = SIZE | NONE_
                            deriving(Show, Eq, Ord, Bounded, Enum)
returnItemCollectionMetrics_t:: ReturnItemCollectionMetrics -> T.Text
returnItemCollectionMetrics_t a =
  case a of
    SIZE -> "SIZE"
    NONE_  -> "NONE"
    
returnItemCollectionMetrics_m :: Map.Map T.Text ReturnItemCollectionMetrics
returnItemCollectionMetrics_m = text_map returnItemCollectionMetrics_t

instance ToJSON ReturnItemCollectionMetrics  where
  toJSON = String . returnItemCollectionMetrics_t
instance FromJSON ReturnItemCollectionMetrics where
  parseJSON = json_str_map_p returnItemCollectionMetrics_m
instance QC.Arbitrary ReturnItemCollectionMetrics where
  arbitrary = QC.elements [minBound..maxBound]

data ReturnValues = RV_NONE | ALL_OLD | UPDATED_OLD | ALL_NEW | UPDATED_NEW
                  deriving(Show, Eq, Ord, Bounded, Enum)

returnValues_t::ReturnValues -> T.Text
returnValues_t a =
  case a of
    RV_NONE -> "NONE"
    ALL_OLD -> "ALL_OLD"
    UPDATED_OLD -> "UPDATED_OLD"
    ALL_NEW -> "ALL_NEW"
    UPDATED_NEW -> "UPDATED_NEW" 
returnValues_m :: Map.Map T.Text ReturnValues
returnValues_m = text_map returnValues_t


data ScanResult
data TableDescription = TableDescription{
  tdAttributeDescriptions:: [AttributeDefinition]
  , tdCreationDateTime   :: DateTime
  , tdItemCount          :: Int
  , tdKeySchema          :: KeySchema
  , tdLocalSeconadaryIndexes :: [LocalSecondaryIndex]
  , tdProvisionedThroughput :: ProvisionedThroughput
  , tdTableName :: TableName
  , tdTableSizeBytes :: Int
  , tdTableStatus :: T.Text  
  }deriving(Show, Eq)
instance FromJSON TableDescription where
  parseJSON _ = mzero


data UpdatedItemResult
data UpdateTableResult

data Item = Item{
  iItem :: Map.Map T.Text Value_
  }deriving(Show, Eq)
instance ToJSON Item where
  toJSON a = object [ "ForumName" .= object [ "S" .= String "123456"]]

newtype ExclusiveTableName = ExclusiveTableName{_ExclusiveTableName::TableName}
                           deriving(Show, Eq)
instance ToJSON ExclusiveTableName where
  toJSON ex = object[ "ExclusiveTableName" .= _ExclusiveTableName ex]

data Expected = Expected{
  eExpected:: Map.Map T.Text (Maybe Exists, Maybe Value_)
  }deriving(Show, Eq)

instance ToJSON Expected where
  toJSON (Expected mp) = object[] -- TODO


data Exists = Exists Bool
              deriving(Show, Eq)
instance ToJSON Exists where
  toJSON (Exists True)  = object["Exists" .= True]
  toJSON (Exists False) = object["Exists" .= False]


data Value_ =
  ValueB T.Text
  | ValueBS [T.Text]
  | ValueN Int
  | ValueNS [T.Text]
  | ValueS T.Text
  | ValueSS [T.Text]
    deriving(Show, Eq)

instance ToJSON Value_ where
  toJSON (ValueB  a) = object[ "B"  .= a]
  toJSON (ValueBS a) = object[ "BS" .= a]
  toJSON (ValueN  a) = object[ "N"  .= a]
  toJSON (ValueNS a) = object[ "NS" .= a]
  toJSON (ValueS  a) = object[ "S"  .= a]
  toJSON (ValueSS a) = object[ "SS" .= a]

data WriteRequest


newtype DdbServiceError = ESE { _ESE :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON DdbServiceError where
    parseJSON (Object v) = ESE <$> v .: "message"
    parseJSON _          = mzero

instance ToJSON DdbServiceError where
    toJSON (ESE msg) =
        object
            [ "message" .= msg
            ]
 
instance QC.Arbitrary DdbServiceError where
    arbitrary = ESE . T.pack <$> QC.arbitrary



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
