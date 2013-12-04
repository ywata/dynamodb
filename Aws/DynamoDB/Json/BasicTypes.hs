{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
module Aws.DynamoDB.Json.BasicTypes
    (
      DDouble(..),
      positiveIntegralGen,
      nonNegativeIntegralGen
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
import qualified Test.QuickCheck.Modifiers      as QC (NonNegative(..), Positive(..))
import           Safe

newtype  DDouble =  DDouble Double
                    deriving(Show)

instance ToJSON DDouble where
  toJSON (DDouble a) = Number (D a)
instance FromJSON DDouble where
  parseJSON n@(Number (D d)) = return $ DDouble d
  parseJSON n@(Number (I d)) = return $ DDouble (fromIntegral d)
  parseJSON _          = mzero
instance QC.Arbitrary DDouble where
  arbitrary = DDouble <$> QC.arbitrary
instance Eq DDouble where
  (DDouble a) == (DDouble b) = cmpDouble a b


cmpDouble :: RealFloat a => a -> a -> Bool
cmpDouble a b -- = relDif a b
  | relDif a b <= epsilon  = True -- This is not a good choice but better than the previous one.
  | otherwise                  = False
  where
    epsilon = (10.0)**(-15.0)

{- http://www.jpcert.or.jp/sc-rules/c-flp35-c.html -}
relDif :: RealFloat a => a -> a -> a
relDif a b
  | m == 0.0  = 0.0
  | otherwise = (abs (a - b)) / (max (abs a) (abs b))
  where m = max (abs a) (abs b)


instance QC.Arbitrary T.Text where
  arbitrary = arbitraryAsciiText

newtype AsciiChar   = AsciiChar {char::Char}
                      deriving(Show, Eq)
--newtype AsciiString = AsciiString {string::String}
--                      deriving(Show, Eq)

instance QC.Arbitrary AsciiChar where
  arbitrary = AsciiChar <$> QC.elements (['a'..'z']++[ 'A'..'Z'] ++['0'..'9'] ++ ['_', '-', '.'])

arbitraryAsciiCharList :: QC.Gen [AsciiChar]
arbitraryAsciiCharList = QC.sized $ \n ->
    do k <- QC.choose (1, max n maxChars)
       sequence [QC.arbitrary | _ <- [1..k]]
    where
      maxChars = 20 --255

asciiStringToText :: [AsciiChar] -> T.Text
asciiStringToText  = T.pack . map char

positiveIntegralGen :: (QC.Arbitrary a, Integral a) => QC.Gen (QC.Positive a)
arbitraryAsciiText = liftM asciiStringToText arbitraryAsciiCharList
positiveIntegralGen = QC.arbitrary

nonNegativeIntegralGen :: (QC.Arbitrary a, Integral a) => QC.Gen (QC.NonNegative a)
nonNegativeIntegralGen = QC.arbitrary

-----
instance (Ord k, QC.Arbitrary k, QC.Arbitrary v) => QC.Arbitrary (Map.Map k v)      where
  arbitrary = Map.fromList <$> QC.arbitrary

