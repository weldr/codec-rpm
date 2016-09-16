{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module RPM.Parse(parseRPM,
                 parseRPMC)
 where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative((<$>))
#endif
import           Control.Monad.Error.Class(Error, strMsg)
import           Control.Monad.Except(MonadError, throwError)
import           Conduit((=$), awaitForever, yield)
import           Data.Attoparsec.ByteString(Parser, anyWord8, count, take, takeByteString, word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Conduit(Conduit)
import           Data.Conduit.Attoparsec(ParseError(..), conduitParserEither)
import           Data.Maybe(mapMaybe)
import           Data.Word(Word16, Word32)
import           Prelude hiding(take)

import RPM.Internal.Numbers(asWord16, asWord32)
import RPM.Tags(Tag, mkTag)
import RPM.Types(Header(..), Lead(..), RPM(..), SectionHeader(..))

takeWord16 :: Parser Word16
takeWord16 = asWord16 <$> take 2

takeWord32 :: Parser Word32
takeWord32 = asWord32 <$> take 4

parseLead :: Parser Lead
parseLead = do
    -- Verify this is an RPM by checking the first four bytes.
    word8 0xed >> word8 0xab >> word8 0xee >> word8 0xdb

    rpmMajor <- anyWord8
    rpmMinor <- anyWord8
    rpmType  <- takeWord16
    rpmArchNum <- takeWord16
    rpmName <- C.unpack <$> BS.takeWhile (/= 0) <$> take 66
    rpmOSNum <- takeWord16
    rpmSigType <- takeWord16

    -- Skip 16 reserved bytes at the end of the lead.
    take 16
    
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
    word8 0x8e >> word8 0xad >> word8 0xe8

    sectionVersion <- anyWord8
    -- Skip four reserved bytes.
    take 4
    sectionCount <- takeWord32
    sectionSize <- takeWord32

    return SectionHeader { sectionVersion,
                           sectionCount,
                           sectionSize }

parseOneTag :: C.ByteString -> C.ByteString -> Maybe Tag
parseOneTag store bs = let
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
    let headerTags = mapMaybe (parseOneTag headerStore) rawTags

    return Header { headerSectionHeader,
                    headerTags,
                    headerStore }

parseRPM :: Parser RPM
parseRPM = do
    -- First comes the (mostly useless) lead.
    rpmLead <- parseLead
    -- Then comes the signature, which is like a regular section except it's also padded.
    sig <- parseSection
    take (signaturePadding sig)
    -- And then comes the real header.  There could be several, but for now there's only ever one.
    hdr <- parseSection
    rpmArchive <- takeByteString
    return RPM { rpmLead,
                 rpmHeaders=[sig, hdr],
                 rpmArchive }
 where
    signaturePadding :: Header -> Int
    signaturePadding hdr = let
        remainder = (sectionSize . headerSectionHeader) hdr `mod` 8
     in
        if remainder > 0 then fromIntegral $ 8 - remainder else 0

-- Like parseRPM, but puts the resulting RPM into a Conduit.
parseRPMC :: (Error e, MonadError e m) => Conduit C.ByteString m (Either e RPM)
parseRPMC =
    conduitParserEither parseRPM =$ consumer
 where
    consumer = awaitForever $ \case
        Left err       -> throwError (strMsg $ errorMessage err)
        Right (_, rpm) -> yield $ Right rpm
