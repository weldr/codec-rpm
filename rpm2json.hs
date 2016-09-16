{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Conduit(($$), (=$), awaitForever, stdinC)
import           Control.Monad.IO.Class(liftIO)
import           Data.Aeson(Value(..), toJSON, ToJSON, object, (.=))
import           Data.Aeson.TH(deriveToJSON, defaultOptions)
import           Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit(Conduit, Consumer, yield)
import           RPM.Parse(parseRPMC)
import           RPM.Tags(Tag)
import           RPM.Types(RPM(..), Lead, SectionHeader, Header(..))

-- make the RPM types JSON-able from the bottom up
-- only doing the to-JSON instead of from-JSON, to avoid headaches and
-- because from JSON isn't terribly useful.
--
-- first, the easy ones, using template magic
deriveToJSON defaultOptions ''Lead
deriveToJSON defaultOptions ''SectionHeader

-- for Tags, do basically what the pretty implementation does: split the show output into
-- words and return the first two. JSON-ize it as { "name" : "WhateverTag", "value" : "stuff goes here" }
instance ToJSON Tag where
    toJSON t = let tagParts = init . init $ words $ show t
               in object [ "name"  .= (tagParts!!0),
                           "value" .= (tagParts!!1) ]

-- for Header, skip the headerStore ByteStream
instance ToJSON Header where
    toJSON hs = object [ "headerSectionHeader" .= toJSON (headerSectionHeader hs),
                         "headerTags"          .= toJSON (headerTags hs) ]

-- for the top-level RPM type, skip rpmArchive
instance ToJSON RPM where
    toJSON rpm = object [ "rpmLead"    .= toJSON (rpmLead rpm),
                          "rpmHeaders" .= toJSON (rpmHeaders rpm) ]

-- conduit to encode RPM into a JSON value. Errors are passed through
encodeC :: Monad m => Conduit (Either String RPM) m (Either String Value)
encodeC = awaitForever $ \case
    Left err  -> yield $ Left err
    Right rpm -> yield $ Right $ toJSON rpm

-- output sink
consumer :: Consumer (Either String Value) IO ()
consumer = awaitForever $ \case
    Left err   -> liftIO $ print err
    Right json -> liftIO $ BSL.putStr $ encodePretty json

main :: IO ()
main = do
    stdinC $$ parseRPMC =$ encodeC =$ consumer
    putStrLn ""
