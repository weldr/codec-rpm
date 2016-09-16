{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Conduit(($$), (=$), awaitForever, stdinC)
import           Control.Monad(liftM)
import           Control.Monad.Except(MonadError)
import           Control.Monad.IO.Class(liftIO)
import           Data.Aeson(Value(..), toJSON, ToJSON, object, (.=))
import           Data.Aeson.TH(deriveToJSON, defaultOptions)
import           Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit(Conduit, Consumer, yield)
import           Data.Data
import           Data.Word
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

-- Tags, wow. What we want to see depends on what's in it. In general, the content is:
--   - nothing at all.
--   - a string. might be something good, show it!
--   - an int. Probably don't care, but no harm in showing it anyway
--   - a list of strings or ints which could well be three miles long. do not show these, holy crap
--   - a bytestring, skip it
-- JSON-ize as { "name" : "WhateverTag", "value" : "maybe a value" }

-- first, some utility functions
tagName :: Tag -> String
tagName t = showConstr $ toConstr t

-- This takes the first constructor parameter of the tag, which is where the data goes, and
-- converts it to a TypeRep
tagType :: Tag -> TypeRep
tagType = gmapQi 0 typeOf

-- Use a cast to pull the first parameter out of the constructor.
tagValue :: (Typeable a) => Tag -> Maybe a
tagValue = gmapQi 0 cast

-- There's probably a better way to do this
-- type needs to be explicit on account of OverloadedStrings
stringType = typeOf ("" :: String)
word32Type = typeOf (0 :: Word32)
word64Type = typeOf (0 :: Word32)

instance ToJSON Tag where
    toJSON t = let namePair = "name" .= tagName t
                   -- Depending on the type of the thing, use a cast to pull
                   -- it out of. Go ahead and conver to JSON here so everything
                   -- is a common type.
                   value    = case tagType t of
                                stringType -> fmap toJSON (tagValue t :: Maybe String)
                                word32Type -> fmap toJSON (tagValue t :: Maybe Word32)
                                word64Type -> fmap toJSON (tagValue t :: Maybe Word64)
                                _          -> Nothing
                   -- If we have a value, it should be passed to the object below,
                   -- otherwise just pass name.
                   valueList = case value of
                                Just x  -> [ "value" .= x ]
                                Nothing -> []
               in 
               
               object (namePair : valueList)

-- for Header, skip the headerStore ByteStream
instance ToJSON Header where
    toJSON hs = object [ "headerSectionHeader" .= toJSON (headerSectionHeader hs),
                         "headerTags"          .= toJSON (headerTags hs) ]

-- for the top-level RPM type, skip rpmArchive
instance ToJSON RPM where
    toJSON rpm = object [ "rpmLead"    .= toJSON (rpmLead rpm),
                          "rpmHeaders" .= toJSON (rpmHeaders rpm) ]

-- conduit to encode RPM into a JSON value. Errors are passed through
encodeC :: MonadError e m => Conduit (Either e RPM) m (Either e Value)
encodeC = awaitForever $ \case
    Left err  -> yield $ Left err
    Right rpm -> yield $ Right $ toJSON rpm

-- output sink
consumer :: Show e => Consumer (Either e Value) IO ()
consumer = awaitForever $ \case
    Left err   -> liftIO $ print err
    Right json -> liftIO $ BSL.putStr $ encodePretty json

main :: IO ()
main = do
    stdinC $$ parseRPMC =$ encodeC =$ consumer
    putStrLn ""
