{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Codec.RPM.Parse
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: stable
-- Portability: portable
--
-- A module for creating 'RPM' records from various data sources.

module Codec.RPM.Parse(
#ifdef TEST
                       parseLead,
                       parseSectionHeader,
                       parseOneTag,
                       parseSection,
#endif
                       parseRPM,
                       parseRPMC)
 where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative((<$>))
#endif
import           Control.Monad(void)
import           Control.Monad.Except(MonadError, throwError)
import           Conduit((.|), Conduit, awaitForever, yield)
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString(Parser, anyWord8, count, take, takeByteString, word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Conduit.Attoparsec(ParseError(..), conduitParserEither)
import           Data.Maybe(mapMaybe)
import           Prelude hiding(take)

import Codec.RPM.Internal.Numbers(asWord32)
import Codec.RPM.Tags(Tag, mkTag)
import Codec.RPM.Types(Header(..), Lead(..), RPM(..), SectionHeader(..))

-- "a <$> b <$> c" looks better than "a . b <$> c"
{-# ANN parseLead "HLint: ignore Functor law" #-}
parseLead :: Parser Lead
parseLead = do
    -- Verify this is an RPM by checking the first four bytes.
    void $ word32be 0xedabeedb

    rpmMajor <- anyWord8
    rpmMinor <- anyWord8
    rpmType  <- anyWord16be
    rpmArchNum <- anyWord16be
    rpmName <- C.unpack <$> BS.takeWhile (/= 0) <$> take 66
    rpmOSNum <- anyWord16be
    rpmSigType <- anyWord16be

    -- Skip 16 reserved bytes at the end of the lead.
    void $ take 16
    
    return Lead { rpmMajor,
                  rpmMinor,
                  rpmType,
                  rpmArchNum,
                  rpmName,
                  rpmOSNum,
                  rpmSigType }

parseSectionHeader :: Parser SectionHeader
parseSectionHeader = do
    -- Verify this is a header section header by checking the first three bytes.
    void $ word8 0x8e >> word8 0xad >> word8 0xe8

    sectionVersion <- anyWord8
    -- Skip four reserved bytes.
    void $ take 4
    sectionCount <- anyWord32be
    sectionSize <- anyWord32be

    return SectionHeader { sectionVersion,
                           sectionCount,
                           sectionSize }

parseOneTag :: C.ByteString -> C.ByteString -> Maybe Tag
parseOneTag store bs | BS.length bs < 16 = Nothing
                     | otherwise = let
    tag = fromIntegral . asWord32 $ BS.take 4 bs
    ty  = fromIntegral . asWord32 $ BS.take 4 (BS.drop 4 bs)
    off = fromIntegral . asWord32 $ BS.take 4 (BS.drop 8 bs)
    cnt = fromIntegral . asWord32 $ BS.take 4 (BS.drop 12 bs)
 in
    mkTag store tag ty off cnt

parseSection :: Parser Header
parseSection = do
    headerSectionHeader <- parseSectionHeader
    -- Grab the tags as a list of bytestrings.  We need the store before we can process the tags, as
    -- that's where all the values for the tags are kept.  However, grabbing each individual tag here
    -- makes it a lot easier to process them later.
    rawTags <- count (fromIntegral $ sectionCount headerSectionHeader) (take 16)
    headerStore <- take (fromIntegral $ sectionSize headerSectionHeader)

    -- Now that we've got the store, process each tag by looking up its values in the store.
    -- NOTE: mapMaybe will reject tags which are Nothing
    let headerTags = mapMaybe (parseOneTag headerStore) rawTags

    return Header { headerSectionHeader,
                    headerTags,
                    headerStore }

-- | A parser (in the attoparsec sense of the term) that constructs 'Codec.RPM.Types.RPM' records.
-- The parser can be run against a 'Data.ByteString.ByteString' of RPM data using any of the usual
-- functions.  'Data.Attoparsec.ByteString.parse' and 'Data.Attoparsec.ByteString.parseOnly' are
-- especially useful:
--
-- > import Data.Attoparsec.ByteString(parse)
-- > import qualified Data.ByteString as BS
-- > s <- BS.readFile "some.rpm"
-- > result <- parse parseRPM s
--
-- The 'Data.Attoparsec.ByteString.Result' can then be examined directly or converted using
-- 'Data.Attoparsec.ByteString.maybeResult' (for converting it into a 'Maybe' 'RPM') or
-- 'Data.Attoparsec.ByteString.eitherResult' (for converting it into an 'Either' 'String' 'RPM').
-- In the latter case, the String contains any parse error that occurred when reading the
-- RPM data.
parseRPM :: Parser RPM
parseRPM = do
    -- First comes the (mostly useless) lead.
    rpmLead <- parseLead
    -- Then comes the signature, which is like a regular section except it's also padded.
    sig <- parseSection
    void $ take (signaturePadding sig)
    -- And then comes the real header.  There could be several, but for now there's only ever one.
    hdr <- parseSection
    rpmArchive <- takeByteString
    return RPM { rpmLead,
                 rpmSignatures=[sig],
                 rpmHeaders=[hdr],
                 rpmArchive }
 where
    signaturePadding :: Header -> Int
    signaturePadding hdr = let
        remainder = (sectionSize . headerSectionHeader) hdr `mod` 8
     in
        if remainder > 0 then fromIntegral $ 8 - remainder else 0

-- | Like 'parseRPM', but puts the result into a 'Conduit' as an 'Either', containing either a
-- 'ParseError' or an 'RPM'.  The result can be extracted with 'Control.Monad.Except.runExceptT',
-- like so:
--
-- > import Conduit((.|), runConduitRes, sourceFile)
-- > import Control.Monad.Except(runExceptT)
-- > result <- runExceptT $ runConduitRes $ sourceFile "some.rpm" .| parseRPMC .| someConsumer
--
-- On success, the 'RPM' record will be passed down the conduit for futher processing or
-- consumption.  Functions can be written to extract just one element out of the 'RPM' and
-- pass it along.  For instance:
--
-- > payloadC :: MonadError e m => Conduit RPM m BS.ByteStrin
-- > payloadC = awaitForever (yield . rpmArchive)
--
-- On error, the rest of the conduit will be skipped and the 'ParseError' will be returned
-- as the result to be dealt with.
parseRPMC :: MonadError String m => Conduit C.ByteString m RPM
parseRPMC =
    conduitParserEither parseRPM .| consumer
 where
    consumer = awaitForever $ either (throwError . errorMessage) (yield . snd)
