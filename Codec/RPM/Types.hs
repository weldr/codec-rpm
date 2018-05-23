{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Codec.RPM.Types
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: stable
-- Portability: portable

module Codec.RPM.Types(RPM(..),
                       Lead(..),
                       Header(..),
                       SectionHeader(..))

 where

import qualified Data.ByteString as BS
import           Data.Word(Word8, Word16, Word32)
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint(($$), hcat, nest, text, vcat)

import Codec.RPM.Tags

-- | The top level RPM record.  This contains everything in an RPM file, except for the
-- magic value.
data RPM = RPM {
    -- | The 'Lead', an obsolete record used for identifying RPMs.
    rpmLead :: Lead,
    -- | Special 'Header' entries that can be used to verify the integrity of an RPM.  This
    -- is represented as a list because it is technically possible for there to be multiple
    -- signature headers, but in practice there is only ever one.  This is the case even if
    -- multiple signatures are present.  This situation will be represented by multiple 'Tag's
    -- that can be examined to get each signature.  When checking signatures, note that they
    -- only apply to the 'rpmHeaders' and the 'rpmArchive'.
    rpmSignatures :: [Header],
    -- | 'Header' entries that contain all the metadata about an RPM.  There could technically
    -- be several entries here too, but in practice there is only ever one.  It is likely
    -- that each 'Header' will contain many 'Tag's, as RPMs tend to have a large amount of
    -- metadata.
    rpmHeaders :: [Header],
    -- | The contents of the RPM, stored as a compressed CPIO archive.
    rpmArchive :: BS.ByteString }
 deriving(Eq, Show)

instance Pretty RPM where
    pPrint RPM{..} =
        vcat [ text "RPM:",
               nest 2 (text "rpmLead = "    $$ nest 2 (pPrint rpmLead)),
               nest 2 (text "rpmSignatures = " $$ nest 2 (vcat $ map pPrint rpmSignatures)),
               nest 2 (text "rpmHeaders = " $$ nest 2 (vcat $ map pPrint rpmHeaders)),
               nest 2 (text "rpmArchive = ...") ]

-- | Following the magic value that identifies a data stream as an RPM, the Lead is the very
-- first part of the file.  Due to its small size and inflexibility, it is largely obsolete
-- and its use is discouraged even inside of the RPM library.  It is generally only used as
-- additional help beyond the magic value in verifying something is an RPM.  The lead is
-- only exposed here for completeness.
data Lead = Lead {
    -- | The major version number of this RPM, for instance 0x03 for version 3.x.
    rpmMajor    :: Word8,
    -- | The minor version number of this RPM, for instance 0x00 for version 3.0.
    rpmMinor    :: Word8,
    -- | Is this a binary package (0x0000) or a source package (0x0001)?  Other types
    -- may be defined in the future.
    rpmType     :: Word16,
    -- | What platform was this package built for?  x86 is 0x0001.  Many other values
    -- are defined.  See \/usr\/lib\/rpm\/rpmrc for the possibilities.
    rpmArchNum  :: Word16,
    -- | The package name, as a NEVRA.  This name is constrained to 66 bytes.  Shorter
    -- names are padded with nulls.
    rpmName     :: String,
    -- | What operating system was this package built for?  Linux is 0x0001.  Many other
    -- values are defined.  See \/usr\/lib\/rpm\/rpmrc for the possibilities.
    rpmOSNum    :: Word16,
    -- | What type of signature is used in this RPM?  For now, this appears to always
    -- be set to 0x0005.
    rpmSigType  :: Word16 }
 deriving(Eq, Show)

instance Pretty Lead where
    pPrint Lead{..} =
        vcat [ text "Lead:",
               nest 2 $ hcat [ text "rpmMajor:   ", text (show rpmMajor) ],
               nest 2 $ hcat [ text "rpmMinor:   ", text (show rpmMinor) ],
               nest 2 $ hcat [ text "rpmType:    ", text (show rpmType) ],
               nest 2 $ hcat [ text "rpmArchNum: ", text (show rpmArchNum) ],
               nest 2 $ hcat [ text "rpmName:    ", text rpmName ],
               nest 2 $ hcat [ text "rpmOSNum:   ", text (show rpmOSNum) ],
               nest 2 $ hcat [ text "rpmSigType: ", text (show rpmSigType) ] ]

-- | A Header represents a block of metadata.  It is used twice in the RPM - as the
-- representation for signatures and as the representation for regular metadata.  Internally,
-- the header is a list of tag descriptors followed by a data store.  These descriptors
-- index into the store and explain what type of thing should be found and how many things
-- should be read.
--
-- Here, the hard work of figuring that out is already done and the results provided as a
-- list of 'Tag's.  The raw store itself is provided for completeness, in case further
-- processing needs to be done on the RPM.  For most users, this will never be needed.
data Header = Header {
    -- | Each header begins with its own 'SectionHeader', describing what type of header
    -- follows and how many entries that header contains.
    headerSectionHeader :: SectionHeader,
    -- | A list of 'Tag' entries and their values.  There are many, many types of tags.
    headerTags :: [Tag],
    -- | The raw header store.
    headerStore :: BS.ByteString }
 deriving(Eq, Show)

instance Pretty Header where
    pPrint Header{..} =
        vcat [ text "Header:",
               nest 2 $ text "headerSectionHeader = " $$ nest 2 (pPrint headerSectionHeader),
               nest 2 $ text "headerTags = "          $$ nest 2 (vcat $ map pPrint headerTags),
               nest 2 $ text "headerStore = ..." ]

-- | The SectionHeader is useful in parsing an RPM.  It allows for figuring out where
-- each section occurs, how large it is, and so forth.  It is likely not useful for
-- consumers of this libary.  Just like with the top-level 'RPM' record, section headers
-- are preceeded with a magic value that is not exposed here.
data SectionHeader = SectionHeader {
    -- | The version of this header structure, currently only 0x01.
    sectionVersion  :: Word8,
    -- | How many 'Tag' entries are stored in this header?
    sectionCount    :: Word32,
    -- | What is the size of the data store in this header?
    sectionSize     :: Word32 }
 deriving(Eq, Show)

instance Pretty SectionHeader where
    pPrint SectionHeader{..} =
        vcat [ text "SectionHeader:",
               nest 2 $ hcat [ text "sectionHeader: ", text (show sectionVersion) ],
               nest 2 $ hcat [ text "sectionCount:  ", text (show sectionCount) ],
               nest 2 $ hcat [ text "sectionSize:   ", text (show sectionSize) ] ]
