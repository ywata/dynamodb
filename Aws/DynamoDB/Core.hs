{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE FlexibleInstances          #-} 
{-# LANGUAGE DeriveDataTypeable         #-} 

module Aws.DynamoDB.Core
    ( DdbQuery(..)
    , DdbConfiguration(..)
    , ddbConfiguration
    , DDBEndpoint
    , ddbEndpointUsEast
    , ddbEndpointUsWest
    , ddbEndpointUsWest2
    , ddbEndpointEu
    , ddbEndpointApSouthEast
    , ddbEndpointApNorthEast
    , ddbEndpointLocal
    , endpoint 
    , region
    , DdbError(..)
    , DdbMetadata(..)
    , ddbSignQuery
    , ddbResponseConsumer
    , jsonConsumer
    , module Aws.Core
    , module Aws.DynamoDB.Json.Types
    ) where

import           Aws.Sign4
import           Aws.Core
import           Aws.DynamoDB.Json.Types
import qualified Control.Exception              as C
import           Control.Monad
import           Control.Applicative
import           Control.Monad.IO.Class
import           Text.Printf
import           Data.String
import           Data.Monoid
import           Data.Aeson                     hiding(Value)
import qualified Data.Aeson                     as A (Value) 
import           Data.Time
import           Data.IORef
import           Data.Maybe
import           Data.Typeable
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Conduit                   as C
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Lazy.Char8     as LC
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP

--import           Crypto.Hash.SHA256

data DdbQuery
    = DdbQuery
        { ddbqMethod  :: Method
        , ddbqRequest :: T.Text
        , ddbqQuery   :: HTTP.Query
        , ddbqCommand :: T.Text
        , ddbqBody    :: Maybe A.Value
        } deriving (Show)

data DdbConfiguration qt
    = DdbConfiguration
        { ddbProtocol      :: Protocol
        , ddbEndpoint      :: DDBEndpoint
        , ddbPort          :: Int
        , ddbDefaultExpiry :: NominalDiffTime
        }
    deriving (Show)

instance DefaultServiceConfiguration (DdbConfiguration NormalQuery) where
    defServiceConfig   = ddbConfiguration HTTPS ddbEndpointUsEast
--    defServiceConfig   = ddbConfiguration HTTP  ddbEndpointUsEast  
    debugServiceConfig = ddbConfiguration HTTP  ddbEndpointUsEast

newtype DDBEndpoint = DDBEndpoint { _DDBEndpoint :: B.ByteString }
    deriving (Show)

instance IsString DDBEndpoint where
    fromString = DDBEndpoint . BC.pack

ddbConfiguration :: Protocol -> DDBEndpoint -> DdbConfiguration qt
ddbConfiguration pro edp = 
    DdbConfiguration 
        { ddbProtocol      = pro
        , ddbEndpoint      = edp
--TODO        , ddbPort          = defaultPort pro
        , ddbPort          = port
        , ddbDefaultExpiry = 15*60
        }
  where
    port = case (region edp) of
      "localhost" -> 8000
      _           -> defaultPort pro
ddbEndpointUsEast, ddbEndpointUsWest, ddbEndpointUsWest2, ddbEndpointEu,
                ddbEndpointApSouthEast, ddbEndpointApNorthEast, ddbEndpointLocal :: DDBEndpoint

ddbEndpointUsEast      = "us-east-1"
ddbEndpointUsWest      = "us-west-1"
ddbEndpointUsWest2     = "us-west-2"
ddbEndpointEu          = "eu-west-1"
ddbEndpointApSouthEast = "ap-southeast-1"
ddbEndpointApNorthEast = "ap-northeast-1"
ddbEndpointLocal       = "localhost"

endpoint, region :: DDBEndpoint -> B.ByteString

--endpoint = \edp -> B.concat ["dynamodb.",region edp,".amazonaws.com"]
endpoint edp  = case (region edp) of
  "localhost" -> "localhost"
  _           -> B.concat ["dynamodb.",region edp,".amazonaws.com"]
region   = _DDBEndpoint


data DdbError
    = DdbError
        { ddbStatusCode   :: HTTP.Status
        , ddbErrorMessage :: T.Text
        }

     -- { ddbError        :: B.ByteString
     -- }
    
    deriving (Show, Typeable)

instance C.Exception DdbError


data DdbMetadata
    = DdbMetadata 
        {
--          ddbMAmzId2    :: Maybe T.Text
--        , ddbMRequestId :: Maybe T.Text
        }
    deriving (Show, Typeable)

instance Monoid DdbMetadata where
  mempty = DdbMetadata
  mappend m1 m2 = DdbMetadata
--    mempty        = DdbMetadata Nothing Nothing
--    mappend m1 m2 = DdbMetadata (a1 `mplus` a2) (r1 `mplus` r2)
--      where
--        DdbMetadata a1 r1 = m1
--        DdbMetadata a2 r2 = m2

instance Loggable DdbMetadata where
  toLogText d = "TestLog"
--    toLogText (DdbMetadata id2 rid) = 
--        "S3: request ID=" 
--                `mappend` fromMaybe "<none>" rid
--                `mappend` ", x-amz-id-2=" 
--                `mappend` fromMaybe "<none>" id2


ddbSignQuery :: DdbQuery -> DdbConfiguration qt -> SignatureData -> SignedQuery
ddbSignQuery dq@DdbQuery{..} DdbConfiguration{..} SignatureData{..} =
    SignedQuery
        { sqMethod        = ddbqMethod
        , sqProtocol      = ddbProtocol
        , sqHost          = endpoint ddbEndpoint
        , sqPort          = ddbPort
        , sqPath          = pth
        , sqQuery         = ddbqQuery
        , sqDate          = Just signatureTime
        , sqAuthorization = Just aut 
        , sqBody          = HTTP.RequestBodyLBS <$> lbd
        , sqStringToSign  = sts                 -- NB for debugging only
        , sqContentType   = ctp
        , sqContentMd5    = Nothing
        , sqAmzHeaders    = []
        , sqOtherHeaders  = hdd
        }


  where
    -- authorization (and string to sign) fields

    aut = s4Authz        sg4    
    sts = s4StringToSign sg4

    -- AWS Signature v4 parameters
     
    sg4 =
        Sign4
            { s4Credentials = signatureCredentials
            , s4Date        = signatureTime
            , s4Endpoint    = region ddbEndpoint
            , s4Service     = "dynamodb"
            , s4Method      = mth
            , s4Path        = pth
            , s4Headers     = hds
            , s4Query       = ddbqQuery
            , s4Body        = maybe B.empty id bdy
            , s4SgndHeaders = Nothing
            , s4CnclHeaders = Nothing
            }
    
    -- the headers (with and without 'host' header)
    
    hds =
        [ (,) "Host" $ endpoint ddbEndpoint
        ] ++ hdd

    hdd = 
        [ (,) "Date" $ fmtTime iso8601BasicUtcDate signatureTime
          , (,) "X-Amz-Target" $ BC.pack . T.unpack $  ddbqCommand
          , (,) "Connection" "Keep-Alive"

        ]
    
    -- URI path

--    pth = BC.pack $ printf "/2012-09-25/%s" $ T.unpack ddbqRequest
    pth = BC.pack $ printf "/"

    -- method, content type and body
    
    mth = 
        case ddbqMethod of
        --Head      -> "HEAD" 
          Get       -> "GET"
          PostQuery -> "POST"
          Post      -> "POST"
          Put       -> "PUT"
          Delete    -> "DELETE"

    ctp = case ddbqMethod of
            Post -> Just "application/json; charset=UTF-8"
            _    -> Nothing
    
    bdy = BC.pack . LC.unpack <$> lbd
    lbd = encode <$> ddbqBody
    
ddbResponseConsumer :: IORef DdbMetadata -> HTTPResponseConsumer a ->
                       HTTPResponseConsumer a
ddbResponseConsumer mrf inr rsp = 
 do liftIO $ tellMetadataRef mrf
                DdbMetadata 
                    {
--                      ddbMAmzId2    = ai2
--                    , ddbMRequestId = rqi
                    }
    if HTTP.responseStatus rsp >= HTTP.status400
      then ddb_error_rc rsp     -- handle error
      else inr          rsp     -- normal processing
  where
    ai2 = mhs "x-amz-id-2"
    rqi = mhs "x-amz-request-id"

    -- extract header string
    mhs = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders rsp)

ddb_error_rc :: HTTPResponseConsumer a
ddb_error_rc rsp0 =
 do rsp <- HTTP.lbsResponse rsp0
    C.monadThrow $ err rsp $ HTTP.responseBody rsp
  where
    err rsp msg = 
            case eitherDecode msg :: Either String DdbServiceError of
              Left per -> 
                DdbError
                    { ddbStatusCode   = HTTP.responseStatus rsp
                    , ddbErrorMessage = oops per msg `T.append` (T.pack . LC.unpack $  HTTP.responseBody rsp) `T.append` "***"
                    }
              Right ese -> 
                DdbError
                    { ddbStatusCode   = HTTP.responseStatus rsp
                    , ddbErrorMessage = (message ese) `T.append` " __type " `T.append` (type_ ese)
                    }
    oops per msg =
                T.pack $ printf "JSON parse error (%s): %s" per $ LC.unpack msg

jsonConsumer :: FromJSON a => HTTPResponseConsumer a
jsonConsumer rsp0 =
  do rsp <- HTTP.lbsResponse rsp0
     either (C.monadThrow . oops rsp) return $ eitherDecode $ HTTP.responseBody rsp 
  where
    oops rsp dgc = 
        DdbError
            { ddbStatusCode   = HTTP.responseStatus rsp
            , ddbErrorMessage = "Failed to parse JSON response: " `T.append` T.pack dgc 
                                `T.append` (T.pack . LC.unpack $ HTTP.responseBody rsp)
            }


