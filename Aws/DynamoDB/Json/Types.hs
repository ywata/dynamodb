{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Aws.DynamoDB.Json.Types
    (
      ActionType(..)
      , AttributeDefinition(..)
      , AttributeName(..)
      , Attributes(..)
      , AttributesToGet(..)
      , DDouble(..)
      , AttributeType(..)
      , AttributeValue(..)
      , AttributeValueUpdate(..)
      , Condition(..)
      , ConsumedCapacity(..)
      , ConsistentRead(..)
      , Count(..)
      , DateTime(..)
      , ExclusiveTableName(..)
      , ExclusiveStartKey(..)
      , Expected(..)
      , ExpectedAttributeValue(..)
      , DdbServiceError(..)
      , ItemCollectionMetrics(..)
      , IndexName(..)
      , Item(..)
      , Items(..)
      , Key(..)
      , KeyConditions(..)
--      , KeyValue(..)
      , KeySchema(..)        
--      , KeySchema_(..)
      , KeySchemaElement(..)
      , KeyType(..)
      , LastEvaluatedKey(..)
      , Limit(..)
        
      , LocalSecondaryIndex(..)
      , LocalSecondaryIndexDescription(..)
      , NonKeyAttribute(..)
      , ComparisonOperator(..)
      , Projection(..)
      , ProjectionType(..)
      , ProvisionedThroughput(..)
      , ProvisionedThroughputDescription(..)
--      , RequestItems(..)
      , ReturnConsumedCapacity(..)
      , ReturnItemCollectionMetrics(..)
      , ReturnValues(..)
      , ScanFilter(..)
      , ScannedCount(..)
      , Select(..)
      , ScanIndexForward(..)
      , TableDescription(..)
      , TableName(..)
      , TableStatus(..)
      , Value(..)
--      , arbitraryKeySchema
      , arbitraryAttributeDefinitions
--       
    ) where

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

--type Key      = T.Text
--type KeyValue  = (Key, Value_)
type NonKeyAttribute = T.Text
type IndexName = T.Text
--type KeySchema = [KeySchemaElement]
data Assoc b    = Assoc T.Text b
type ItemCollectionKey = Map.Map T.Text AttributeValue


type DateTime = DDouble

newtype Count = Count Int
                deriving(Show, Eq)
instance ToJSON Count where
  toJSON (Count c) = object["Count" .= c]
instance FromJSON Count where
  parseJSON (Object v) = Count <$> v .: "Count"
instance QC.Arbitrary Count where
  arbitrary = Count <$> QC.arbitrary

newtype ScannedCount = ScannedCount Int
                deriving(Show, Eq)
instance ToJSON ScannedCount where
  toJSON (ScannedCount c) = object["ScannedCount" .= c]
instance FromJSON ScannedCount where
  parseJSON (Object v) = ScannedCount <$> v .: "ScannedCount"
instance QC.Arbitrary ScannedCount where
  arbitrary = ScannedCount <$> QC.arbitrary





--
-- | AttributeDefinition
--
data AttributeDefinition = AttributeDefinition{
  attributeName :: T.Text, 
  attributeType :: AttributeType
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
instance QC.Arbitrary AttributeDefinition where
  arbitrary = AttributeDefinition <$> QC.arbitrary <*> QC.arbitrary

--
-- | AttributeName
--
newtype AttributeName = AttributeName T.Text
                    deriving(Show, Eq)
instance ToJSON AttributeName where
  toJSON (AttributeName a) = String a
instance FromJSON AttributeName where
  parseJSON (String a) = AttributeName <$> return a
  parseJSON _          = mzero
instance QC.Arbitrary AttributeName where
  arbitrary = AttributeName <$> QC.arbitrary



--
-- | Attributes
--
data Attributes = Attributes{
  attributes :: [T.Text]
  }deriving(Show, Eq)

instance ToJSON Attributes where
  toJSON (Attributes ts) = object[
    "Attributes" .= ts
    ]
instance FromJSON Attributes where
  parseJSON(Object v) = Attributes <$>
                        v .:  "Attributes"
instance QC.Arbitrary Attributes where  
  arbitrary = Attributes <$> QC.arbitrary


--
-- | AttributesToGet
--
data AttributesToGet = AttributesToGet{
  attributesToGet :: [T.Text]
  }deriving(Show, Eq)

instance ToJSON AttributesToGet where
  toJSON (AttributesToGet ts) = object[
    "AttributesToGet" .= ts
    ]
instance FromJSON AttributesToGet where
  parseJSON(Object v) = AttributesToGet <$>
                        v .:  "AttributesToGet"
instance QC.Arbitrary AttributesToGet where  
  arbitrary = AttributesToGet <$> QC.arbitrary

--
-- | AttributeValue
--
data AttributeValue = AV_B | AV_BS | AV_N | AV_NS | AV_S | AV_SS
                    deriving (Show, Eq, Ord, Bounded,Enum)

attributeValue_t ::AttributeValue -> T.Text
attributeValue_t av =
  case av of
    AV_B  -> "B"
    AV_BS -> "BS"
    AV_N  -> "N"
    AV_NS -> "NS"
    AV_S  -> "S"
    AV_SS -> "SS"
attributeValue_m :: Map.Map T.Text AttributeValue
attributeValue_m = text_map attributeValue_t
instance ToJSON AttributeValue where
  toJSON = String . attributeValue_t
instance FromJSON AttributeValue where
  parseJSON = json_str_map_p attributeValue_m
instance QC.Arbitrary AttributeValue where
  arbitrary = QC.elements [minBound..maxBound]


--
-- | AttributeType
--
data AttributeType = AT_B | AT_N | AT_S 
                    deriving (Show, Eq, Ord, Bounded,Enum)

attributeType_t ::AttributeType -> T.Text
attributeType_t av =
  case av of
    AT_B  -> "B"
    AT_N  -> "N"
    AT_S  -> "S"

attributeType_m :: Map.Map T.Text AttributeType
attributeType_m = text_map attributeType_t
instance ToJSON AttributeType where
  toJSON = String . attributeType_t
instance FromJSON AttributeType where
  parseJSON = json_str_map_p attributeType_m
instance QC.Arbitrary AttributeType where
  arbitrary = QC.elements [minBound..maxBound]


--
-- | ActionType
--
data ActionType = ADD | PUT | DELETE
                              deriving(Show, Eq, Ord, Bounded, Enum)
actionType_t :: ActionType -> T.Text
actionType_t a =
  case a of
    ADD    -> "ADD"
    PUT    -> "PUT"
    DELETE -> "DELETE"
actionType_m :: Map.Map T.Text ActionType
actionType_m = text_map actionType_t

instance ToJSON ActionType where
  toJSON = String . actionType_t
instance FromJSON ActionType where
  parseJSON = json_str_map_p actionType_m
instance QC.Arbitrary ActionType  where
  arbitrary = QC.elements [minBound.. maxBound]

--
-- | AttributeValueList = [AttributeValue]
--
  
--
-- | AttributeValueUpdate -- tested
--
data AttributeValueUpdate = AttributeValueUpdate{
  action :: Maybe ActionType
  , value:: Maybe AttributeValue
  }deriving(Show, Eq)

instance ToJSON AttributeValueUpdate where
  toJSON (AttributeValueUpdate a v) =
    object [
      "Action" .= a, "Value" .= v
           ]
instance FromJSON AttributeValueUpdate where
  parseJSON (Object v) = AttributeValueUpdate <$>
                         v .:? "Action"       <*>
                         v .:? "Value"

instance QC.Arbitrary AttributeValueUpdate where
  arbitrary = AttributeValueUpdate <$> QC.arbitrary <*> QC.arbitrary

--
-- | ConsumedCapacity -- tested
--
data ConsumedCapacity = ConsumedCapacity{
  ccCacacityUnits :: Maybe Int,
  ccTableName     :: Maybe TableName
  }deriving(Show, Eq)
instance ToJSON ConsumedCapacity where
  toJSON ConsumedCapacity{ccCacacityUnits = u, ccTableName = t} =
    object[ "CapacityUnits" .= u, "TableName" .= t]
instance FromJSON ConsumedCapacity where
  parseJSON (Object v) = ConsumedCapacity <$>
              v .:? "CapacityUnits"       <*>
              v .:? "TableName"            
              
--  parseJSON _ = mzero
instance QC.Arbitrary ConsumedCapacity where
  arbitrary = ConsumedCapacity <$> QC.arbitrary <*> QC.arbitrary

--
-- | ConsistentRead -- tested
--
newtype ConsistentRead = ConsistentRead Bool
  deriving(Show, Eq)
instance ToJSON ConsistentRead where
  toJSON (ConsistentRead a) = object["ConsistentRead" .= a]
instance FromJSON ConsistentRead where
  parseJSON (Object v) = ConsistentRead <$>
                         v .: "ConsistentRead"
instance QC.Arbitrary ConsistentRead where
  arbitrary = ConsistentRead <$> QC.arbitrary

--
-- | Condition -- tested
--
data Condition = Condition{
  cComparisonOperator :: ComparisonOperator
  , cAttributeList    :: Maybe [AttributeValue]
  }deriving(Show, Eq)

instance ToJSON Condition where
  toJSON (Condition a b) = object[
    "ComparisonOperator" .= a
    , "AttributeList"    .= b
    ]
instance FromJSON Condition where
  parseJSON (Object v) = Condition <$>
                         v .: "ComparisonOperator" <*>
                         v .: "AttributeList"
  parseJSON _          = mzero                         

instance QC.Arbitrary Condition where
  arbitrary = Condition <$> QC.arbitrary <*> QC.arbitrary



--
-- | Operator
--
data ComparisonOperator
  = EQ_ |NE_| LE_ | LT_ | GE_ | GT_ | BEGIN_WITH_ | BETWEEN_ | NOT_NULL_ | NULL_ |CONTAINS_| NOT_CONTAINS_| IN_
  deriving(Show, Eq, Ord, Bounded, Enum)
comparisonOperator_t :: ComparisonOperator -> T.Text
comparisonOperator_t op =
  case op of
    EQ_ -> "EQ"
    NE_ -> "NE"
    LT_ -> "LT"    
    LE_ -> "LE"
    GE_ -> "GE"
    GT_ -> "GT"
    BEGIN_WITH_   -> "BEGIN_WITH"
    BETWEEN_      -> "BETWEEN"
    NOT_NULL_     -> "NOT_NULL"
    NULL_         -> "NULL"
    CONTAINS_     -> "CONTAINS"
    NOT_CONTAINS_ -> "NOT_CONTAINS"
    IN_           -> "IN"
comparisonOperator_m :: Map.Map T.Text ComparisonOperator
comparisonOperator_m = text_map comparisonOperator_t

instance ToJSON ComparisonOperator where
  toJSON = String . comparisonOperator_t
instance FromJSON ComparisonOperator where
  parseJSON = json_str_map_p comparisonOperator_m
instance QC.Arbitrary ComparisonOperator where
  arbitrary = QC.elements [minBound..maxBound]

data ExclusiveStartKey = ExclusiveStartKey{
  exclusiveStartKey :: Map.Map T.Text Value
  } deriving(Show, Eq)
instance ToJSON ExclusiveStartKey where
  toJSON (ExclusiveStartKey a) = object[
    "ExclusiveStartKey" .= a
    ]
instance FromJSON ExclusiveStartKey where
  parseJSON (Object v) = ExclusiveStartKey <$>
                         v .: "ExclusiveStartKey"
instance QC.Arbitrary ExclusiveStartKey where  
  arbitrary = ExclusiveStartKey <$> QC.arbitrary

--
-- | ExpectedAttributeValue -- tested
--
data ExpectedAttributeValue = ExpectedAttributeValue{
  eavExist :: Maybe Bool
  , eavValue :: Maybe AttributeValue
  }deriving(Show, Eq)
instance ToJSON ExpectedAttributeValue where
  toJSON (ExpectedAttributeValue a b) = object["Exist" .= a, "Value" .= b]
instance FromJSON ExpectedAttributeValue where
  parseJSON (Object v) = ExpectedAttributeValue <$>
                         v .:? "Exist" <*>
                         v .:? "Value"
instance QC.Arbitrary ExpectedAttributeValue where  
  arbitrary = ExpectedAttributeValue <$> QC.arbitrary <*> QC.arbitrary



--
-- | KeyConditions  -- tested
--
data KeyConditions = KeyConditions (Map.Map T.Text Condition)
                     deriving(Show, Eq)

instance ToJSON KeyConditions where
  toJSON(KeyConditions k) = object["KeyConditions" .= k]
instance FromJSON KeyConditions where
  parseJSON (Object v) = KeyConditions <$>
                         v .: "KeyConditions"
instance QC.Arbitrary KeyConditions where  
  arbitrary = KeyConditions <$> QC.arbitrary


--
-- | KeySchema_
--
newtype KeySchema = KeySchema [KeySchemaElement]
                    deriving(Show, Eq)
instance ToJSON KeySchema where
  toJSON (KeySchema ks) = toJSON ks

instance FromJSON KeySchema where
  parseJSON (Array v) = do
    d  <-  mapM parseJSON (V.toList v)
    return $ KeySchema d
        
  parseJSON _          = mzero

instance QC.Arbitrary KeySchema where
  arbitrary = KeySchema <$> arbitraryKeySchema

--
-- | KeySchemaElement -- tested
--
data KeySchemaElement = KeySchemaElement{
  kseAttributeName :: T.Text
  , kseKeyType     :: KeyType
  }deriving(Show, Eq)
instance ToJSON KeySchemaElement where
  toJSON (KeySchemaElement a b) = object[
    "AttributeName" .= toJSON a
    , "KeyType"     .= toJSON b]
instance FromJSON KeySchemaElement where
  parseJSON (Object v) =
    KeySchemaElement     <$>
    v .: "AttributeName" <*>
    v .: "KeyType"
instance QC.Arbitrary KeySchemaElement where
--  arbitrary = do
--    a <- QC.arbitrary
--    arbitraryKeySchemaElement a
  arbitrary = KeySchemaElement <$>
              QC.arbitrary <*>
              QC.arbitrary


--
-- | Keys -- tested
--
data Key  = Key (Map.Map T.Text Value)
           deriving (Show, Eq)
instance  ToJSON Key where
  toJSON (Key a)  = object["Keys" .= a]
instance FromJSON Key where
  parseJSON (Object v) = Key <$> v .: "Keys"
  parseJSON _          = mzero
instance  QC.Arbitrary Key where
  arbitrary = Key <$> QC.arbitrary


--  
-- | KeyType -- tested
--
data KeyType = HASH | RANGE
                      deriving(Show, Eq, Ord, Bounded, Enum)
keyType_t:: KeyType -> T.Text
keyType_t kt =
  case kt of
    HASH  -> "HASH"
    RANGE -> "RANGE"
keyType_m :: Map.Map T.Text KeyType
keyType_m = text_map keyType_t
instance ToJSON KeyType where
  toJSON = String . keyType_t
instance FromJSON KeyType where
  parseJSON = json_str_map_p keyType_m
instance QC.Arbitrary KeyType where
  arbitrary = QC.elements [minBound..maxBound]



--
-- | ItemCollectionMetrics --tested
--
data ItemCollectionMetrics = ItemCollectionMetrics{
  icmItemCollectionKey     :: Maybe ItemCollectionKey
  , icmSizeEstimateRangeGB :: Maybe [DDouble]
  }deriving(Show, Eq)
instance ToJSON  ItemCollectionMetrics where
  toJSON (ItemCollectionMetrics a b) = object ["ItemCollectionKey" .= a, "SizeEstimateRangeGB".=b]
instance FromJSON ItemCollectionMetrics where
  parseJSON (Object v) =
    ItemCollectionMetrics     <$>
    v .:? "ItemCollectionKey" <*> v .:? "SizeEstimateRangeGB"
instance QC.Arbitrary ItemCollectionMetrics where
  arbitrary = ItemCollectionMetrics <$> QC.arbitrary <*> QC.arbitrary


--
-- | LastEvaluatedKey
--
data LastEvaluatedKey = LastEvaluatedKey{
  lekLastEvaluatedKey :: Map.Map T.Text Value -- Key ?
  }deriving(Show, Eq)
instance ToJSON LastEvaluatedKey where
  toJSON (LastEvaluatedKey a) = object["LastEvaluatedKey" .= toJSON a]
instance FromJSON LastEvaluatedKey where
  parseJSON (Object v) = LastEvaluatedKey <$> v .: "LastEvaluatedKey"
  parseJSON _          = mzero
instance QC.Arbitrary LastEvaluatedKey where
  arbitrary = LastEvaluatedKey <$> QC.arbitrary

--
-- | Limit  -- tested
--
newtype Limit = Limit{_Limit::Int}
             deriving(Show, Eq)
instance ToJSON Limit where
  toJSON a = object["Limit" .= _Limit a]
instance FromJSON Limit where
  parseJSON (Object v) = Limit <$> v .: "Limit"
  parseJSON _ = mzero
instance QC.Arbitrary Limit where
  arbitrary = Limit <$> QC.arbitrarySizedIntegral

--
-- | LocalSecondaryIndex -- test failed
--
data LocalSecondaryIndex = LocalSecondaryIndex{
  lsiIndexName   :: IndexName
  , lsiKeySchema :: KeySchema
  , lsiProjection:: Projection
  }deriving(Show, Eq)
instance ToJSON LocalSecondaryIndex where
  toJSON a = object[
    "IndexName"      .= lsiIndexName a
    , "KeySchema"    .= lsiKeySchema a
    , "Projection" .= lsiProjection a]
instance FromJSON LocalSecondaryIndex where
  parseJSON (Object v) =
    LocalSecondaryIndex <$>
    v .: "IndexName"    <*>
    v .: "KeySchema"    <*>
    v .: "Projection"
  parseJSON _ = mzero
instance QC.Arbitrary LocalSecondaryIndex where
  arbitrary = LocalSecondaryIndex <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary              

--
-- | LocalSecondaryIndexDescription
--
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription{
  lsidIndexName        :: Maybe T.Text
  , lsidIndexSizeBytes :: Maybe Int
  , lsidItemCount      :: Maybe Int
  , lsidKeySchema      :: Maybe KeySchema
  , lsidProjection     :: Maybe Projection
  }deriving(Show, Eq)
instance ToJSON LocalSecondaryIndexDescription where
  toJSON (LocalSecondaryIndexDescription a b c d e) = object[
    "IndexName" .= a
    , "IndexSizeBytes" .= b
    , "ItemCount"      .= c
    , "KeySchema"      .= d
    , "Projection"     .= e
    ] 
instance FromJSON LocalSecondaryIndexDescription where
  parseJSON (Object v) = LocalSecondaryIndexDescription <$>
                         v .: "IndexName"     <*>
                         v .: "IndexSizeBytes"     <*>
                         v .: "ItemCount"     <*>
                         v .: "KeySchema"     <*>
                         v .: "Projection"
instance QC.Arbitrary LocalSecondaryIndexDescription where  
  arbitrary = LocalSecondaryIndexDescription <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary 


--
-- | Projection -- tested
--
data Projection = Projection{
  pNonKeyAttributes::[NonKeyAttribute]
  , pProjectionType::[ProjectionType]
  }deriving(Show, Eq)
instance ToJSON Projection where
  toJSON (Projection a b) = object[
    "NonKeyAttributes" .= a
    , "ProjectionType" .= b]

instance FromJSON Projection where
  parseJSON (Object v) =
    Projection             <$>
    v .: "NonKeyAttributes" <*>
    v .: "ProjectionType"
  parseJSON _ = mzero    
instance QC.Arbitrary Projection where
  arbitrary = Projection   <$>
              QC.arbitrary <*>
              QC.arbitrary
--
-- | ProjectionType -- tested
--
data ProjectionType = ALL_ | KEYS_ONLY_ | INCLUDE_
                    deriving(Show, Eq, Ord, Bounded, Enum)
projectionType_t:: ProjectionType -> T.Text
projectionType_t pt =
  case pt of
    ALL_       -> "ALL"
    KEYS_ONLY_ -> "KEYS_ONLY"
    INCLUDE_   -> "INCLUDE"
projectionType_m :: Map.Map T.Text ProjectionType
projectionType_m = text_map projectionType_t
instance ToJSON ProjectionType where
  toJSON = String . projectionType_t
instance FromJSON ProjectionType where
  parseJSON  = json_str_map_p projectionType_m
    
instance QC.Arbitrary ProjectionType where  
  arbitrary =  QC.elements [minBound..maxBound]

--
-- | ProvisionedThroughput -- failed when DateTime = Double
--                         -- passed if DateTime = Int
data ProvisionedThroughput = ProvisionedThroughput{
  ptReadCapacityUnits   ::Int
  , ptWriteCapacityUnits::Int
  }deriving(Show, Eq)
instance ToJSON ProvisionedThroughput where
  toJSON (ProvisionedThroughput a b) =
    object (["ReadCapacityUnits"        .= a
          , "WriteCapacityUnits"     .= b]) -- ++ obp)

instance FromJSON ProvisionedThroughput where
  parseJSON (Object v) =
    ProvisionedThroughput     <$>
    v .: "ReadCapacityUnits"  <*>
    v .: "WriteCapacityUnits" 
  parseJSON _ = mzero
instance QC.Arbitrary ProvisionedThroughput where
  arbitrary = do
    a <- positiveIntegralGen
    b <- positiveIntegralGen
    let QC.Positive a' = a
        QC.Positive b' = b
        (a'', b'') = (max 10 a', max 10 b')
    return $ ProvisionedThroughput a'' b''  -- reduce maximum size
--    return $ ProvisionedThroughput 40000 40000

--
-- | ProvisionedThroughputDescription -- failed when DateTIme=Double
--                                    -- passed if DateTime = Int
data ProvisionedThroughputDescription = ProvisionedThroughputDescription{
  ptdLastDecreaseDateTime     :: Maybe DateTime
  , ptdLastIncreaseDateTime   :: Maybe DateTime
  , ptdNumberOfDecreaseToday  :: Maybe Int
    , ptdReadCapacityUnites   :: Maybe Int
      , ptdWriteCapacityUnits :: Maybe Int
  }deriving(Show, Eq)
instance ToJSON ProvisionedThroughputDescription where
  toJSON(ProvisionedThroughputDescription a b c d e) =
    object ["LastDecreaseDateTime"    .= a
            , "LastIncreaseDateTime"  .= b
            , "NumberOfDecreaseToday" .= c
            , "ReadCapacityUnits"     .= d
              , "WriteCapacityUnits"  .= e]

instance FromJSON ProvisionedThroughputDescription where
  parseJSON (Object v) = ProvisionedThroughputDescription <$>
                         v .:? "LastDecreaseDateTime" <*>
                         v .:? "LastIncreaseDateTime" <*>
                         v .:? "NumberOfDecreaseToday" <*>
                         v .:? "ReadCapacityUnits"     <*>
                         v .:? "WriteCapacityUnits"
  parseJSON _ = mzero
instance QC.Arbitrary ProvisionedThroughputDescription where
  arbitrary = ProvisionedThroughputDescription <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary               
--
-- | ReturnConsumedCapacity -- tested
-- 
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


--
-- | ReturnItemCollectionMetrics  -- tested
--  
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

--
-- | ReturnValues  -- tested
--
data ReturnValues = RV_NONE | ALL_OLD | UPDATED_OLD | ALL_NEW | UPDATED_NEW
                  deriving(Show, Eq, Ord, Bounded, Enum)

returnValues_t::ReturnValues -> T.Text
returnValues_t a =
  case a of
    RV_NONE     -> "NONE"
    ALL_OLD     -> "ALL_OLD"
    UPDATED_OLD -> "UPDATED_OLD"
    ALL_NEW     -> "ALL_NEW"
    UPDATED_NEW -> "UPDATED_NEW" 
returnValues_m :: Map.Map T.Text ReturnValues
returnValues_m = text_map returnValues_t
instance ToJSON ReturnValues where
  toJSON = String . returnValues_t
instance FromJSON ReturnValues where
  parseJSON = json_str_map_p returnValues_m
instance QC.Arbitrary ReturnValues where
  arbitrary = QC.elements [minBound..maxBound]


--
-- | ScanFilter  -- tested
--
data ScanFilter = ScanFilter (Map.Map T.Text Condition)
                     deriving(Show, Eq)
  
instance ToJSON ScanFilter where
  toJSON(ScanFilter k) = object["ScanFilter" .= k]
instance FromJSON ScanFilter where
  parseJSON (Object v) = ScanFilter <$>
                         v .: "ScanFilter"
instance QC.Arbitrary ScanFilter where  
  arbitrary = ScanFilter <$> QC.arbitrary


data ScanIndexForward = ScanIndexForward Bool
                      deriving(Show, Eq)
instance ToJSON ScanIndexForward where
  toJSON (ScanIndexForward b) = object["ScanIndexForward" .= b]
instance FromJSON ScanIndexForward where
  parseJSON (Object v) = ScanIndexForward <$>
                         v .: "ScanIndexForward"
instance QC.Arbitrary ScanIndexForward where  
  arbitrary = ScanIndexForward <$> QC.arbitrary

data Select = ALL_ATTRIBUTES | ALL_PROJECTED_ATTRIBUTES | COUNT | SPECIFIC_ATTRIBUTES
              deriving(Show, Eq, Ord, Bounded, Enum)
select_t :: Select -> T.Text
select_t a =
  case a of
    ALL_ATTRIBUTES           -> "ALL_ATTRIBUTES"
    ALL_PROJECTED_ATTRIBUTES -> "ALL_PROJECTED_ATTRIBUTES"
    COUNT                    -> "COUNT"
    SPECIFIC_ATTRIBUTES      -> "SPECIFIC_ATTRIBUTES"
select_m :: Map.Map T.Text Select
select_m = text_map select_t
instance ToJSON Select where
  toJSON = String . select_t
instance FromJSON Select where
  parseJSON = json_str_map_p select_m
instance QC.Arbitrary Select where
  arbitrary = QC.elements [minBound..maxBound]


--
-- | TableDescription failed because of Double
--                    succeeded if DateTime = Int
data TableDescription = TableDescription{
  tdAttributeDefinitions     :: Maybe [AttributeDefinition] 
  , tdCreationDateTime       :: Maybe DateTime 
  , tdItemCount              :: Maybe Int 
  , tdKeySchema              :: Maybe KeySchema 
  , tdLocalSeconadaryIndexes :: Maybe [LocalSecondaryIndex]
  , tdProvisionedThroughput  :: Maybe ProvisionedThroughput 
  , tdTableName              :: Maybe TableName 
  , tdTableSizeBytes         :: Maybe Int
  , tdTableStatus            :: Maybe TableStatus 
  }deriving(Show, Eq)
instance ToJSON TableDescription where
  toJSON a = object[
    "AttributeDefinitions"  .= tdAttributeDefinitions a 
    , "CreationDateTime"      .= tdCreationDateTime a
    , "ItemCount"             .= tdItemCount a
    , "KeySchema"             .= tdKeySchema a 
    , "LocalSecondaryIndexes" .= tdLocalSeconadaryIndexes a
    , "ProvisionedThroughput" .= tdProvisionedThroughput a  
    , "TableName"             .= tdTableName a
    , "TableSizeBytes"        .= tdTableSizeBytes a
    , "TableStatus"           .= tdTableStatus a
    ]
instance FromJSON TableDescription where
  parseJSON (Object v) =
    TableDescription <$>
    v .:? "AttributeDefinitions"    <*>
    v .:? "CreationDateTime"        <*> 
    v .:? "ItemCount"               <*>
    v .:? "KeySchema"               <*> 
    v .:? "LocalSecondaryIndexes"   <*> 
    v .:? "ProvisionedThroughput"   <*>
    v .:? "TableName"               <*>
    v .:? "TableSizeBytes"          <*>
    v .:? "TableStatus" 
  parseJSON _ = mzero
instance QC.Arbitrary TableDescription where
  arbitrary = TableDescription <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*> 
              QC.arbitrary <*>
              QC.arbitrary <*> 
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary 

--
-- | TableName
--
newtype TableName = TableName {text::T.Text}
                    deriving(Show, Eq)
instance ToJSON TableName where
  toJSON (TableName a) = String a
instance FromJSON TableName where
  parseJSON (String a) = TableName <$> return a
  parseJSON _          = mzero
instance QC.Arbitrary TableName where
  arbitrary = TableName <$> QC.arbitrary


--
-- | TableStatus -- tested
--
data TableStatus = ACTIVE | CREATING
                            deriving(Show, Eq, Ord, Bounded, Enum)
tableStatus_t :: TableStatus -> T.Text 
tableStatus_t a = 
  case a of
    ACTIVE -> "ACTIVE"
    CREATING -> "CREATING"
tableStatus_m :: Map.Map T.Text TableStatus
tableStatus_m = text_map tableStatus_t
instance ToJSON TableStatus where
  toJSON = String . tableStatus_t
instance FromJSON TableStatus where
  parseJSON = json_str_map_p tableStatus_m
instance QC.Arbitrary TableStatus where
  arbitrary = QC.elements [minBound..maxBound]

--
-- | Item not tested
--
data Item = Item{
  iItem :: Map.Map T.Text Value -- Key ?
  }deriving(Show, Eq)
instance ToJSON Item where
  toJSON (Item a) = toJSON a
instance FromJSON Item where
  parseJSON (Object v) = Item <$> (return . objectToMap $ v)
{-instance ToJSON Item where
  toJSON (Item a) = object["Item" .= toJSON a]
instance FromJSON Item where
  parseJSON (Object v) = Item <$> v .: "Item"
  parseJSON _          = mzero
-}
instance QC.Arbitrary Item where
  arbitrary = Item <$> QC.arbitrary

data Items = Items{
  items ::[Map.Map T.Text Value]
  }deriving(Show, Eq)
instance ToJSON Items where
  toJSON (Items a) = object["Items" .= a]
instance FromJSON Items where
  parseJSON (Object v) = Items <$> v.:"Items"
instance QC.Arbitrary Items where
  arbitrary = Items <$> QC.arbitrary
  shrink    = QC.shrinkNothing


objectToMap::Object -> Map.Map T.Text Value 
objectToMap = Map.fromList . map (\(x, y) -> (x, fromJust y)) . filter sndNothing . map conv . H.toList
  where
    conv::(T.Text, A.Value) -> (T.Text, Maybe Value)
    conv (a, v) = (a, decode . encode . toJSON $ v)
    sndNothing (a, Nothing) = False
    sndNothing _            = True


--
-- | ExclusiveableName
--
newtype ExclusiveTableName = ExclusiveTableName{_ExclusiveTableName::TableName}
                           deriving(Show, Eq)
instance ToJSON ExclusiveTableName where
  toJSON ex = object[ "ExclusiveTableName" .= _ExclusiveTableName ex]

data Expected = Expected{
  eExpected :: ExpectedAttributeValue
  }deriving(Show, Eq)

instance ToJSON Expected where
  toJSON (Expected mp) = object[
    "Expected" .= mp
    ]
instance FromJSON Expected where
  parseJSON(Object v) = Expected <$>
                        v .: "Expected"
instance QC.Arbitrary Expected where
  arbitrary = Expected <$> QC.arbitrary

data Exists = Exists Bool
              deriving(Show, Eq)
instance ToJSON Exists where
  toJSON (Exists True)  = object["Exists" .= True]
  toJSON (Exists False) = object["Exists" .= False]

--
-- | Value -- tested
--
data Value =
  ValueB    T.Text    -- Should be ByteString
  | ValueBS [T.Text]  -- Should be ByteString
  | ValueN  Int
  | ValueNS [Int]
  | ValueS  T.Text
  | ValueSS [T.Text]
    deriving(Show, Eq)

instance ToJSON Value where
  toJSON (ValueB  a) = object[ "B"  .= a]
  toJSON (ValueBS a) = object[ "BS" .= a]
  toJSON (ValueN  a) = object[ "N"  .= a]
  toJSON (ValueNS a) = object[ "NS" .= a]
  toJSON (ValueS  a) = object[ "S"  .= a]
  toJSON (ValueSS a) = object[ "SS" .= a]
instance FromJSON Value where
  parseJSON (Object v) = ValueB  <$> v  .: "B"  <|>
                         ValueBS <$> v  .: "BS" <|>
                         ValueS  <$> v  .: "S"  <|>
                         ValueSS <$> v  .: "SS" <|>
                         ValueN  <$> v  .: "N"  <|>
                         ValueNS <$> v  .: "NS" 
  parseJSON _          = mzero

instance QC.Arbitrary Value where
  arbitrary = QC.oneof [liftM   ValueB  QC.arbitrary
                        , liftM ValueBS QC.arbitrary
                        , liftM ValueN  QC.arbitrary
                        , liftM ValueNS QC.arbitrary
                        , liftM ValueS  QC.arbitrary
                        , liftM ValueSS QC.arbitrary]
  shrink(ValueB  x) = [ValueB  x' | x' <- QC.shrink x]
  shrink(ValueBS x) = [ValueBS x' | x' <- QC.shrink x]
  shrink(ValueN  x) = [ValueN  x' | x' <- QC.shrink x]
  shrink(ValueNS x) = [ValueNS x' | x' <- QC.shrink x]
  shrink(ValueS  x) = [ValueS  x' | x' <- QC.shrink x]
  shrink(ValueSS x) = [ValueSS x' | x' <- QC.shrink x]  

------------
-- ARBITRARY IMPLEMENTATION
------------
--arbitraryKeySchema::QC.Gen KeySchema
arbitraryKeySchema = QC.sized $ \n ->
  do
    fst <- arbitraryKeySchemaElement HASH
    k <- QC.choose (0, n)
    sequence (return fst  : [arbitraryKeySchemaElement RANGE | _ <- [0.. max 1 k]])
     
arbitraryKeySchemaElement kType = do
  attribName <- QC.arbitrary
  return $ KeySchemaElement attribName kType

arbitraryAttributeDefinitions :: QC.Gen [AttributeDefinition]
arbitraryAttributeDefinitions = QC.sized $ \n ->
  do
    fst <- QC.arbitrary
    k <- QC.choose (-1, n)
    sequence (return fst : [QC.arbitrary | _ <- [0 .. (min (-1) (min 0 k)) ]])


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
