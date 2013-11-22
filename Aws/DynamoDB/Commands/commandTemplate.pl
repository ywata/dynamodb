#!/usr/bin/perl

if( $#ARGV < 1){
     print STDERR "usage:$0 CommandName commandName\n";
    exit 1
}

$Command = $ARGV[0];
$CommandResponse = "$Command" . "Response";
$CommandResult = "$Command" . "Result";
$command = $Command;
$command = $ARGV[1];

open(TEMPLATE, ">$Command.hs") or die $!;


print TEMPLATE <<"END_OF_TEMPLATE";
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 
module Aws.DynamoDB.Commands.$Command
    ( $Command\(..\)
    , $CommandResponse\(..\)
    , $command
    ) where

import           Aws.Core
import           Aws.DynamoDB.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import qualified Test.QuickCheck as QC


data $Command
    = $Command
        \{
        \}
    deriving (Show, Eq)

instance ToJSON $Command where
  toJSON ($Command) =
    object[
      ]
instance FromJSON $Command where
    parseJSON (Object v) = $Command <\$>
instance QC.Arbitrary $Command where
    arbitrary = $Command <\$>



data $CommandResponse
    = $CommandResponse {}
    deriving (Show,Eq)
instance ToJSON $CommandResponse where
  toJSON ($Command) =
    object[
      ]
instance FromJSON $CommandResponse where
    parseJSON (Object v) = $CommandResponse <\$>
instance QC.Arbitrary $CommandResponse where
    arbitrary = $CommandResponse <\$>


$command \:\: $Command
$command = $Command



instance SignQuery $Command where

    type ServiceConfiguration $Command  = DdbConfiguration

    signQuery a\@$Command {..} = ddbSignQuery DdbQuery
        { ddbqMethod  = Post
        , ddbqRequest = ""
        , ddbqQuery   = []
        , ddbqCommand = "DynamoDB_20120810.$Command"
        , ddbqBody    = Just \$ toJSON \$ a
        }

data $CommandResult = $CommandResult\{\} deriving(Show, Eq)

instance FromJSON $CommandResult where
 parseJSON _ = return $CommandResult

instance ResponseConsumer $Command $CommandResponse where

    type ResponseMetadata $CommandResponse = DdbMetadata

    responseConsumer _ mref = ddbResponseConsumer mref \$ \\rsp -> cnv <\$> jsonConsumer rsp
      where
        cnv ($CommandResult \{\}) = $CommandResponse\{\}


instance Transaction $Command $Command\Response

instance AsMemoryResponse $CommandResponse where

    type MemoryResponse $CommandResponse = $CommandResponse

    loadToMemory = return

END_OF_TEMPLATE
__END__

