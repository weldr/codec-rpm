{-# LANGUAGE RecordWildCards #-}

module RPM.Types
 where

import           Control.Monad.Except(ExceptT)
import           Control.Monad.Trans.Resource(ResourceT)
import qualified Data.ByteString as BS
import           Data.Word(Word8, Word16, Word32)
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint((<>), ($$), nest, text, vcat)

import RPM.Tags

-- The monad transformer stack all our Producers, Consumers, and Conduits operate in.  IO is at the bottom,
-- as it must be if it's in any transformer stack.  Its use is obvious - we're reading and writing files
-- on disk.  ExceptT allows us to return one of two values from the pipeline - an error string, or the
-- result of the computation.  On error, all the other pipeline sections will be skipped.  ResourceT
-- keeps track of open file handles for us, opening and closing them as needed.
type RPMMonad = ResourceT (ExceptT String IO)

data RPM = RPM {
    rpmLead :: Lead,
    rpmHeaders :: [Header],
    rpmArchive :: BS.ByteString }
 deriving(Eq, Show)

instance Pretty RPM where
    pPrint RPM{..} =
        vcat [ text "RPM:",
               nest 2 (text "rpmLead = "    $$ nest 2 (pPrint rpmLead)),
               nest 2 (text "rpmHeaders = " $$ nest 2 (vcat $ map pPrint rpmHeaders)),
               nest 2 (text "rpmArchive = ...") ]

data Lead = Lead {
    rpmMajor    :: Word8,
    rpmMinor    :: Word8,
    rpmType     :: Word16,
    rpmArchNum  :: Word16,
    rpmName     :: String,
    rpmOSNum    :: Word16,
    rpmSigType  :: Word16 }
 deriving(Eq, Show)

instance Pretty Lead where
    pPrint Lead{..} =
        vcat [ text "Lead:",
               nest 2 $ text "rpmMajor:   " <> text (show rpmMajor),
               nest 2 $ text "rpmMinor:   " <> text (show rpmMinor),
               nest 2 $ text "rpmType:    " <> text (show rpmType),
               nest 2 $ text "rpmArchNum: " <> text (show rpmArchNum),
               nest 2 $ text "rpmName:    " <> text rpmName,
               nest 2 $ text "rpmOSNum:   " <> text (show rpmOSNum),
               nest 2 $ text "rpmSigType: " <> text (show rpmSigType) ]

data Header = Header {
    headerSectionHeader :: SectionHeader,
    headerTags :: [Tag],
    headerStore :: BS.ByteString }
 deriving(Eq, Show)

instance Pretty Header where
    pPrint Header{..} =
        vcat [ text "Header:",
               nest 2 $ text "headerSectionHeader = " $$ nest 2 (pPrint headerSectionHeader),
               nest 2 $ text "headerTags = "          $$ nest 2 (vcat $ map pPrint headerTags),
               nest 2 $ text "headerStore = ..." ]

data SectionHeader = SectionHeader {
    sectionVersion  :: Word8,
    sectionCount    :: Word32,
    sectionSize     :: Word32 }
 deriving(Eq, Show)

instance Pretty SectionHeader where
    pPrint SectionHeader{..} =
        vcat [ text "SectionHeader:",
               nest 2 $ text "sectionHeader: " <> text (show sectionVersion),
               nest 2 $ text "sectionCount:  " <> text (show sectionCount),
               nest 2 $ text "sectionSize:   " <> text (show sectionSize) ]
