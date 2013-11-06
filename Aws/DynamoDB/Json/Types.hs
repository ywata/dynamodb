{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE EmptyDataDecls             #-} 
module Aws.DynamoDB.Json.Types
    (
      ActionType(..)
      , AttributeDefinition(..)
      , Attributes(..)
      , AttributeName(..)
      , AttributeType(..)
      , AttributeValue(..)
      , ConsumedCapacity(..)
      , ConsistentRead(..)
      , DateTime(..)
      , ExclusiveTableName(..)
      , Expected(..)
      , DdbServiceError(..)
      , ItemCollectionMetrics(..)
      , IndexName(..)
      , Item(..)
      , Key(..)
      , KeyValue(..)
      , KeySchema(..)
      , KeySchemaElement(..)
      , KeyType(..)
      , Limit(..)
        
      , LocalSecondaryIndex(..)
      , NonKeyAttribute(..)
      , Operator(..)
      , Projection(..)
      , ProjectionType(..)
      , ProvisionedThroughput(..)

      , ReturnConsumedCapacity(..)
      , ReturnItemCollectionMetrics(..)
      , ReturnValues(..)
      , TableDescription(..)
      , TableName(..)
      , TableStatus(..)
      , Value_(..)
--       
    ) where

import           Data.Maybe
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

type AttributeName = T.Text
type TableName = T.Text
type Key       = T.Text
type KeyValue  = (Key, Value_)
type DateTime = Double
type IndexName = T.Text
type KeySchema = [KeySchemaElement]
data Assoc b    = Assoc T.Text b
--type AssocList a b = [Assoc a b]

instance QC.Arbitrary T.Text where
  arbitrary = T.pack <$> QC.arbitrary

instance (ToJSON b) => ToJSON (Assoc b) where
  toJSON (Assoc a b) = object [ a .= toJSON b]
instance FromJSON (Assoc b) where
  parseJSON _ = mzero

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
-- | AttributeValue
--

-- AttributeValue contains the type of AttributeType
-- AttributeType deals B, N S only.
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
  arbitrary = QC.elements [minBound.. maxBound]

data Attributes = Attributes{
                            }

--
-- | AttributeValueUpdate -- not tested
--
data AttributeValueUpdate = Action{actionType:: ActionType}|
                            Value AttributeValue
instance ToJSON AttributeValueUpdate where
  toJSON Action{actionType =a}   = object ["actionType" .= a]
  toJSON v@(Value av) = toJSON av

--
-- | ConsumedCapacity
--
data ConsumedCapacity = ConsumedCapacity{
  ccCacacityUnits :: Int,
  ccTableName     :: TableName
  }deriving(Show, Eq)
instance ToJSON ConsumedCapacity where
  toJSON ConsumedCapacity{ccCacacityUnits = u, ccTableName =t} =
    object[ "CapacityUnits" .= u, "TableName" .= t]
instance FromJSON ConsumedCapacity where
  parseJSON (Object v) = ConsumedCapacity <$>
              v .: "CapacityUnits" <*>
              v .: "TableName"
  parseJSON _ = mzero
instance QC.Arbitrary ConsumedCapacity where
  arbitrary = ConsumedCapacity <$> QC.arbitrary <*> QC.arbitrary

--
-- | ConsumedCapacity
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


--data Condition = Condition{}


--
-- | Operator
--
data Operator
  = EQ_ |NE_| LE_ | LT_ | GE_ | GT_ | BEGIN_WITH_ | BETWEEN_ | NOT_NULL_ | NULL_ |CONTAINS_| NOT_CONTAINS_| IN_
  deriving(Show, Eq, Ord, Bounded, Enum)
operator_t :: Operator -> T.Text
operator_t op =
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
operator_m :: Map.Map T.Text Operator
operator_m = text_map operator_t

instance ToJSON Operator where
  toJSON = String . operator_t
instance FromJSON Operator where
  parseJSON = json_str_map_p operator_m
instance QC.Arbitrary Operator where
  arbitrary = QC.elements [minBound..maxBound]


data ExpectedAttributeValue

--
-- | ItemCollectionKey not tested
--
type ItemCollectionKey = Map.Map T.Text AttributeValue
data ItemCollectionMetrics = ItemCollectionMetrics{
  icmItemCollectionKey     :: Maybe ItemCollectionKey
  , icmSizeEstimateRangeGB :: Maybe [Double]
  }deriving(Show, Eq)
instance FromJSON ItemCollectionMetrics where
  parseJSON (Object v) =
    ItemCollectionMetrics <$>
    v .:? "ItemCollectionKey" <*>
    v .:? "SizeEstimateRangeGB"
--instance QC.Arbitrary ItemCollectionMetrics where
--  arbitrary = ItemCollectionMetrics <$> QC.arbitrary <*> QC.arbitrary

--
-- | KeysAndAttributes
--
data KeysAndAttributes
--
-- | KeySchemaElement
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
  arbitrary = KeySchemaElement <$>
              QC.arbitrary <*>
              QC.arbitrary

--  
-- | KeyType
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
-- | Limit  
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
  lsiIndexName:: IndexName
  , lsiKeySchema :: KeySchema
  , lsiProjection:: Projection
  }deriving(Show, Eq)
instance ToJSON LocalSecondaryIndex where
  toJSON a = object[
    "IndexName"      .= lsiIndexName a
    , "KeySchema"    .= lsiKeySchema a
    , "lsiProjection" .= lsiProjection a]
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


data LocalSecondaryIndexDescription
--
-- | NonKeyAttribute
--
data NonKeyAttribute = NonKeyAttribute{type_::[T.Text]}
                       deriving(Show, Eq)
instance ToJSON NonKeyAttribute where
  toJSON a = object[
    "NonKeyAttributes" .= (toJSON . type_ $ a)]
instance FromJSON NonKeyAttribute where
  parseJSON (Object v)= v .: "NonKeyAttributes"
  parseJSON _ = mzero
instance QC.Arbitrary NonKeyAttribute where
  arbitrary = NonKeyAttribute <$> QC.arbitrary

--
-- | Projection
--
data Projection = Projection{
  pNonKeyAttribute::[NonKeyAttribute]
  , pProjectionType::[ProjectionType]
  }deriving(Show, Eq)
instance ToJSON Projection where
  toJSON a = object[
    "NonKeyAttribute" .= pNonKeyAttribute a
    , "ProjectionType" .= pProjectionType a]

instance FromJSON Projection where
  parseJSON (Object v) =
    Projection             <$>
    v .: "NonKeyAttribute" <*>
    v .: "ProjectionType"
  parseJSON _ = mzero    
instance QC.Arbitrary Projection where
  arbitrary = Projection   <$>
              QC.arbitrary <*>
              QC.arbitrary
--
-- | ProjectionType failed
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

data ProvisionedThroughput = ProvisionedThroughput{
  ptReadCapacityUnits::Int
  , ptWriteCapacityUnits::Int
  , ptLastDecreasedTime :: Maybe DateTime
  , ptLastIncreasedTime :: Maybe DateTime
  , ptNumberOfDecreasedToday :: Maybe DateTime
  }deriving(Show, Eq)
instance ToJSON ProvisionedThroughput where
  toJSON (ProvisionedThroughput a b c d e) =
    object (["ReadCapacityUnits"        .= a
          , "WriteCapacityUnits"     .= b]) -- ++ obp)
    where
      obp = map (\(x, y) -> x .= y) $
        filter (\(x, y) -> isJust y) 
               [("LastDecreasedTime", c), ("LastIncreasedTime", d), ("NumberOfDecreasedToday", e)]

instance FromJSON ProvisionedThroughput where
  parseJSON (Object v) =
    ProvisionedThroughput <$>
    v .: "ReadCapacityUnits"     <*>
    v .: "WriteCapacityUnits"    <*>
    v .:? "LastDecreasedTime"    <*>
    v .:? "LastIncreasedTime"    <*>
    v .:? "NumberOfDecreasedToday"
  parseJSON _ = mzero
instance QC.Arbitrary ProvisionedThroughput where
  arbitrary = ProvisionedThroughput <$>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary <*>
              QC.arbitrary              


data ProvisionedThroughputDescription
--
-- | ReturnConsumedCapacity
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
-- | ReturnItemCollectionMetrics
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
-- | ReturnValues
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

data ScanResult
--
-- | TableDescription failed 'cause of Double
--  
data TableDescription = TableDescription{
  tdAttributeDefinitions:: [AttributeDefinition]
  , tdCreationDateTime   :: DateTime 
  , tdItemCount          :: Int 
  , tdKeySchema          :: KeySchema
  , tdLocalSeconadaryIndexes ::Maybe [LocalSecondaryIndex]  
  , tdProvisionedThroughput :: ProvisionedThroughput 
  , tdTableName :: TableName
  , tdTableSizeBytes :: Int
  , tdTableStatus :: TableStatus
  }deriving(Show, Eq)
instance ToJSON TableDescription where
  toJSON a = object[
    "AttributeDefinitions"  .= tdAttributeDefinitions a,
    "CreationDateTime"      .= tdCreationDateTime a,
    "ItemCount"             .= tdItemCount a,
    "KeySchema"             .= tdKeySchema a, 
    "LocalSecondaryIndexes" .= tdLocalSeconadaryIndexes a,
    "ProvisonedThroughput"  .= tdProvisionedThroughput a,
    "TableName"             .= tdTableName a,
    "TableSizeBytes"        .= tdTableSizeBytes a,
    "TableStatus"           .= tdTableStatus a
    ]
           
instance FromJSON TableDescription where
  parseJSON (Object v) =
    TableDescription <$>
    v .: "AttributeDefinitions"    <*>
    v .: "CreationDateTime"        <*> 
    v .: "ItemCount"               <*>
    v .: "KeySchema"               <*> 
    v .:? "LocalSecondaryIndexes"  <*>
    v .: "ProvisionedThroughput"   <*>
    v .: "TableName"               <*>
    v .: "TableSizeBytes"          <*>
    v .: "TableStatus" 
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
-- | TableStatus
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
  iItem :: Map.Map T.Text Value_
  }deriving(Show, Eq)
instance ToJSON Item where
  toJSON (Item a) = toJSON a
--instance ToJSON Item where
--  toJSON a = object [ "ForumName" .= object [ "S" .= String "123456"]]

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
instance FromJSON Value_ where
  parseJSON a = mzero <|> mzero
instance QC.Arbitrary Value_ where
  arbitrary = QC.oneof [liftM ValueB    QC.arbitrary
                        , liftM ValueBS QC.arbitrary
                        , liftM ValueN  QC.arbitrary
                        , liftM ValueNS QC.arbitrary
                        , liftM ValueS  QC.arbitrary
                        , liftM ValueSS QC.arbitrary]
  shrink(ValueB x)  = [ValueB  x' | x' <- QC.shrink x]
  shrink(ValueBS x) = [ValueBS x' | x' <- QC.shrink x]
  shrink(ValueN x)  = [ValueN  x' | x' <- QC.shrink x]
  shrink(ValueNS x) = [ValueNS x' | x' <- QC.shrink x]
  shrink(ValueS x)  = [ValueS  x' | x' <- QC.shrink x]
  shrink(ValueSS x) = [ValueSS x' | x' <- QC.shrink x]  

--------------
--------- Code below are derived from aws-elastictranscoding
-------------
newtype DdbServiceError = DDB { _DDB :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON DdbServiceError where
    parseJSON (Object v) = DDB <$> v .: "message"
    parseJSON _          = mzero

instance ToJSON DdbServiceError where
    toJSON (DDB msg) =
        object
            [ "message" .= msg
            ]
 
instance QC.Arbitrary DdbServiceError where
    arbitrary = DDB . T.pack <$> QC.arbitrary



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
