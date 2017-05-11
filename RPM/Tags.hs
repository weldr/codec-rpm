-- Copyright (C) 2016-2017 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable #-}

module RPM.Tags(Tag(..),
                Null(..),
                findByteStringTag,
                findTag,
                findStringTag,
                findStringListTag,
                findWord16Tag,
                findWord16ListTag,
                findWord32Tag,
                findWord32ListTag,
                mkTag,
                tagValue)
 where

import           Data.Bits((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Data(Data, cast, gmapQi, showConstr, toConstr)
import           Data.List(find)
import           Data.Maybe(fromMaybe, listToMaybe)
import           Data.Typeable(Typeable)
import           Data.Word
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint(text)

import RPM.Internal.Numbers

{-# ANN module "HLint: ignore Use camelCase" #-}

-- The character lists are actually lists of characters, ignore the suggestions
-- to use String instead
{-# ANN module "HLint: ignore Use String" #-}

data Tag = DEPRECATED                   Tag
         | INTERNAL                     Tag
         | OBSOLETE                     Tag
         | UNIMPLEMENTED                Tag
         | UNUSED                       Tag

         | HeaderImage                  Null
         | HeaderSignatures             Null
         | HeaderImmutable              Null
         | HeaderRegions                Null
         | HeaderI18NTable              [String]

         | SigBase                      Null
         | SigSize                      Word32
         | SigLEMD5_1                   Null
         | SigPGP                       BS.ByteString
         | SigLEMD5_2                   Null
         | SigMD5                       BS.ByteString
         | SigGPG                       BS.ByteString
         | SigPGP5                      Null
         | SigBadSHA1_1                 Null
         | SigBadSHA1_2                 Null
         | PubKeys                      [String]
         | DSAHeader                    BS.ByteString
         | RSAHeader                    BS.ByteString
         | SHA1Header                   String
         | LongSigSize                  Word64
         | LongArchiveSize              Word64

         | Name                         String
         | Version                      String
         | Release                      String
         | Epoch                        Word32
         | Summary                      BS.ByteString
         | Description                  BS.ByteString
         | BuildTime                    Word32
         | BuildHost                    String
         | InstallTime                  Word32
         | Size                         Word32
         | Distribution                 String
         | Vendor                       String
         | GIF                          BS.ByteString
         | XPM                          BS.ByteString
         | License                      String
         | Packager                     String
         | Group                        BS.ByteString
         | ChangeLog                    [String]
         | Source                       [String]
         | Patch                        [String]
         | URL                          String
         | OS                           String
         | Arch                         String
         | PreIn                        String
         | PostIn                       String
         | PreUn                        String
         | PostUn                       String
         | OldFileNames                 [String]
         | FileSizes                    [Word32]
         | FileStates                   [Char]
         | FileModes                    [Word16]
         | FileUIDs                     [Word32]
         | FileGIDs                     [Word32]
         | FileRDevs                    [Word16]
         | FileMTimes                   [Word32]
         | FileMD5s                     [String]
         | FileLinkTos                  [String]
         | FileFlags                    [Word32]
         | Root                         Null
         | FileUserName                 [String]
         | FileGroupName                [String]
         | Exclude                      Null
         | Exclusive                    Null
         | Icon                         BS.ByteString
         | SourceRPM                    String
         | FileVerifyFlags              [Word32]
         | ArchiveSize                  Word32
         | ProvideName                  [String]
         | RequireFlags                 [Word32]
         | RequireName                  [String]
         | RequireVersion               [String]
         | NoSource                     [Word32]
         | NoPatch                      [Word32]
         | ConflictFlags                [Word32]
         | ConflictName                 [String]
         | ConflictVersion              [String]
         | DefaultPrefix                String
         | BuildRoot                    String
         | InstallPrefix                String
         | ExcludeArch                  [String]
         | ExcludeOS                    [String]
         | ExclusiveArch                [String]
         | ExclusiveOS                  [String]
         | AutoReqProv                  String
         | RPMVersion                   String
         | TriggerScripts               [String]
         | TriggerName                  [String]
         | TriggerVersion               [String]
         | TriggerFlags                 [Word32]
         | TriggerIndex                 [Word32]
         | VerifyScript                 String
         | ChangeLogTime                [Word32]
         | ChangeLogName                [String]
         | ChangeLogText                [String]
         | BrokenMD5                    Null
         | PreReq                       Null
         | PreInProg                    [String]
         | PostInProg                   [String]
         | PreUnProg                    [String]
         | PostUnProg                   [String]
         | BuildArchs                   [String]
         | ObsoleteName                 [String]
         | VerifyScriptProg             [String]
         | TriggerScriptProg            [String]
         | DocDir                       Null
         | Cookie                       String
         | FileDevices                  [Word32]
         | FileINodes                   [Word32]
         | FileLangs                    [String]
         | Prefixes                     [String]
         | InstPrefixes                 [String]
         | TriggerIn                    Null
         | TriggerUn                    Null
         | TriggerPostUn                Null
         | AutoReq                      Null
         | AutoProv                     Null
         | Capability                   Word32
         | SourcePackage                Word32
         | OldOrigFileNames             Null
         | BuildPreReq                  Null
         | BuildRequires                Null
         | BuildConflicts               Null
         | BuildMacros                  Null
         | ProvideFlags                 [Word32]
         | ProvideVersion               [String]
         | ObsoleteFlags                [Word32]
         | ObsoleteVersion              [String]
         | DirIndexes                   [Word32]
         | BaseNames                    [String]
         | DirNames                     [String]
         | OrigDirIndexes               [Word32]
         | OrigBaseNames                [String]
         | OrigDirNames                 [String]
         | OptFlags                     String
         | DistURL                      String
         | PayloadFormat                String
         | PayloadCompressor            String
         | PayloadFlags                 String
         | InstallColor                 Word32
         | InstallTID                   Word32
         | RemoveTID                    Word32
         | SHA1RHN                      Null
         | RHNPlatform                  String
         | Platform                     String
         | PatchesName                  [String]
         | PatchesFlags                 [Word32]
         | PatchesVersion               [String]
         | CacheCTime                   Word32
         | CachePkgPath                 String
         | CachePkgSize                 Word32
         | CachePkgMTime                Word32
         | FileColors                   [Word32]
         | FileClass                    [Word32]
         | ClassDict                    [String]
         | FileDependsX                 [Word32]
         | FileDependsN                 [Word32]
         | DependsDict                  [(Word32, Word32)]
         | SourcePkgID                  BS.ByteString
         | FileContexts                 [String]
         | FSContexts                   [String]
         | ReContexts                   [String]
         | Policies                     [String]
         | PreTrans                     String
         | PostTrans                    String
         | PreTransProg                 [String]
         | PostTransProg                [String]
         | DistTag                      String
         | OldSuggestsName              [String]
         | OldSuggestsVersion           [String]
         | OldSuggestsFlags             [Word32]
         | OldEnhancesName              [String]
         | OldEnhancesVersion           [String]
         | OldEnhancesFlags             [Word32]
         | Priority                     [Word32]
         | CVSID                        String
         | BLinkPkgID                   [String]
         | BLinkHdrID                   [String]
         | BLinkNEVRA                   [String]
         | FLinkPkgID                   [String]
         | FLinkHdrID                   [String]
         | FLinkNEVRA                   [String]
         | PackageOrigin                String
         | TriggerPreIn                 Null
         | BuildSuggests                Null
         | BuildEnhances                Null
         | ScriptStates                 [Word32]
         | ScriptMetrics                [Word32]
         | BuildCPUClock                Word32
         | FileDigestAlgos              [Word32]
         | Variants                     [String]
         | XMajor                       Word32
         | XMinor                       Word32
         | RepoTag                      String
         | Keywords                     [String]
         | BuildPlatforms               [String]
         | PackageColor                 Word32
         | PackagePrefColor             Word32
         | XattrsDict                   [String]
         | FileXattrsx                  [Word32]
         | DepAttrsDict                 [String]
         | ConflictAttrsx               [Word32]
         | ObsoleteAttrsx               [Word32]
         | ProvideAttrsx                [Word32]
         | RequireAttrsx                [Word32]
         | BuildProvides                Null
         | BuildObsoletes               Null
         | DBInstance                   Word32
         | NVRA                         String

         | FileNames                    [String]
         | FileProvide                  [String]
         | FileRequire                  [String]
         | FSNames                      [String]
         | FSSizes                      [Word64]
         | TriggerConds                 [String]
         | TriggerType                  [String]
         | OrigFileNames                [String]
         | LongFileSizes                [Word64]
         | LongSize                     Word64
         | FileCaps                     [String]
         | FileDigestAlgo               Word32
         | BugURL                       String
         | EVR                          String
         | NVR                          String
         | NEVR                         String
         | NEVRA                        String
         | HeaderColor                  Word32
         | Verbose                      Word32
         | EpochNum                     Word32
         | PreInFlags                   Word32
         | PostInFlags                  Word32
         | PreUnFlags                   Word32
         | PostUnFlags                  Word32
         | PreTransFlags                Word32
         | PostTransFlags               Word32
         | VerifyScriptFlags            Word32
         | TriggerScriptFlags           [Word32]
         | Collections                  [String]
         | PolicyNames                  [String]
         | PolicyTypes                  [String]
         | PolicyTypesIndexes           [Word32]
         | PolicyFlags                  [Word32]
         | PolicyVCS                    String
         | OrderName                    [String]
         | OrderVersion                 [String]
         | OrderFlags                   [Word32]
         | MSSFManifest                 [String]
         | MSSFDomain                   [String]
         | InstFileNames                [String]
         | RequireNEVRs                 [String]
         | ProvideNEVRs                 [String]
         | ObsoleteNEVRs                [String]
         | ConflictNEVRs                [String]
         | FileNLinks                   [Word32]
         | RecommendName                [String]
         | RecommendVersion             [String]
         | RecommendFlags               [Word32]
         | SuggestName                  [String]
         | SuggestVersion               [String]
         | SuggestFlags                 [Word32]
         | SupplementName               [String]
         | SupplementVersion            [String]
         | SupplementFlags              [Word32]
         | EnhanceName                  [String]
         | EnhanceVersion               [String]
         | EnhanceFlags                 [Word32]
         | RecommendNEVRs               [String]
         | SuggestNEVRs                 [String]
         | SupplementNEVRs              [String]
         | EnhanceNEVRs                 [String]
         | Encoding                     String
         | FileTriggerIn                Null
         | FileTriggerUn                Null
         | FileTriggerPostUn            Null
         | FileTriggerScripts           [String]
         | FileTriggerScriptProg        [String]
         | FileTriggerScriptFlags       [Word32]
         | FileTriggerName              [String]
         | FileTriggerIndex             [Word32]
         | FileTriggerVersion           [String]
         | FileTriggerFlags             [Word32]
         | TransFileTriggerIn           Null
         | TransFileTriggerUn           Null
         | TransFileTriggerPostUn       Null
         | TransFileTriggerScripts      [String]
         | TransFileTriggerScriptProg   [String]
         | TransFileTriggerScriptFlags  [Word32]
         | TransFileTriggerName         [String]
         | TransFileTriggerIndex        [Word32]
         | TransFileTriggerVersion      [String]
         | TransFileTriggerFlags        [Word32]
         | RemovePathPostFixes          String
         | FileTriggerPriorities        [Word32]
         | TransFileTriggerPriorities   [Word32]
         | FileTriggerConds             [String]
         | FileTriggerType              [String]
         | TransFileTriggerConds        [String]
         | TransFileTriggerType         [String]
         | FileSignatures               [String]
         | FileSignatureLength          Word32
  deriving(Eq, Show, Data, Typeable)

instance Pretty Tag where
    -- This is a lot quicker than having to provide a Pretty instance that takes every
    -- single Tag into account.
    pPrint = text . show

data Null = Null
 deriving(Eq, Show, Data, Typeable)

mkTag :: BS.ByteString -> Int -> Word32 -> Word32 -> Word32 -> Maybe Tag
mkTag store tag ty offset count = case tag of
    61      -> maker mkNull          >>=                 \v -> Just $ HeaderImage v
    62      -> maker mkNull          >>=                 \v -> Just $ HeaderSignatures v
    63      -> maker mkNull          >>=                 \v -> Just $ HeaderImmutable v
    64      -> maker mkNull          >>=                 \v -> Just $ HeaderRegions v
    100     -> maker mkStringArray   >>=                 \v -> Just $ HeaderI18NTable v

    256     -> maker mkNull          >>=                 \v -> Just $ SigBase v
    257     -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ SigSize v
    258     -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ SigLEMD5_1 v
    259     -> maker mkBinary        >>=                 \v -> Just $ SigPGP v
    260     -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ SigLEMD5_2 v
    261     -> maker mkBinary        >>=                 \v -> Just $ SigMD5 v
    262     -> maker mkBinary        >>=                 \v -> Just $ SigGPG v
    263     -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ SigPGP5 v
    264     -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ SigBadSHA1_1 v
    265     -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ SigBadSHA1_2 v
    266     -> maker mkStringArray   >>=                 \v -> Just $ PubKeys v
    267     -> maker mkBinary        >>=                 \v -> Just $ DSAHeader v
    268     -> maker mkBinary        >>=                 \v -> Just $ RSAHeader v
    269     -> maker mkString        >>=                 \v -> Just $ SHA1Header v
    270     -> maker mkWord64        >>= listToMaybe >>= \v -> Just $ LongSigSize v
    271     -> maker mkWord64        >>= listToMaybe >>= \v -> Just $ LongArchiveSize v

    1000    -> maker mkString        >>=                 \v -> Just $ Name v
    1001    -> maker mkString        >>=                 \v -> Just $ Version v
    1002    -> maker mkString        >>=                 \v -> Just $ Release v
    1003    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ Epoch v
    1004    -> maker mkI18NString    >>=                 \v -> Just $ Summary v
    1005    -> maker mkI18NString    >>=                 \v -> Just $ Description v
    1006    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ BuildTime v
    1007    -> maker mkString        >>=                 \v -> Just $ BuildHost v
    1008    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ InstallTime v
    1009    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ Size v
    1010    -> maker mkString        >>=                 \v -> Just $ Distribution v
    1011    -> maker mkString        >>=                 \v -> Just $ Vendor v
    1012    -> maker mkBinary        >>=                 \v -> Just $ GIF v
    1013    -> maker mkBinary        >>=                 \v -> Just $ XPM v
    1014    -> maker mkString        >>=                 \v -> Just $ License v
    1015    -> maker mkString        >>=                 \v -> Just $ Packager v
    1016    -> maker mkI18NString    >>=                 \v -> Just $ Group v
    1017    -> maker mkStringArray   >>=                 \v -> Just $ INTERNAL $ ChangeLog v
    1018    -> maker mkStringArray   >>=                 \v -> Just $ Source v
    1019    -> maker mkStringArray   >>=                 \v -> Just $ Patch v
    1020    -> maker mkString        >>=                 \v -> Just $ URL v
    1021    -> maker mkString        >>=                 \v -> Just $ OS v
    1022    -> maker mkString        >>=                 \v -> Just $ Arch v
    1023    -> maker mkString        >>=                 \v -> Just $ PreIn v
    1024    -> maker mkString        >>=                 \v -> Just $ PostIn v
    1025    -> maker mkString        >>=                 \v -> Just $ PreUn v
    1026    -> maker mkString        >>=                 \v -> Just $ PostUn v
    1027    -> maker mkStringArray   >>=                 \v -> Just $ OBSOLETE $ OldFileNames v
    1028    -> maker mkWord32        >>=                 \v -> Just $ FileSizes v
    1029    -> maker mkChar          >>=                 \v -> Just $ FileStates v
    1030    -> maker mkWord16        >>=                 \v -> Just $ FileModes v
    1031    -> maker mkWord32        >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ FileUIDs v
    1032    -> maker mkWord32        >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ FileGIDs v
    1033    -> maker mkWord16        >>=                 \v -> Just $ FileRDevs v
    1034    -> maker mkWord32        >>=                 \v -> Just $ FileMTimes v
    1035    -> maker mkStringArray   >>=                 \v -> Just $ FileMD5s v
    1036    -> maker mkStringArray   >>=                 \v -> Just $ FileLinkTos v
    1037    -> maker mkWord32        >>=                 \v -> Just $ FileFlags v
    1038    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ Root v
    1039    -> maker mkStringArray   >>=                 \v -> Just $ FileUserName v
    1040    -> maker mkStringArray   >>=                 \v -> Just $ FileGroupName v
    1041    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ Exclude v
    1042    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ Exclusive v
    1043    -> maker mkBinary        >>=                 \v -> Just $ Icon v
    1044    -> maker mkString        >>=                 \v -> Just $ SourceRPM v
    1045    -> maker mkWord32        >>=                 \v -> Just $ FileVerifyFlags v
    1046    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ ArchiveSize v
    1047    -> maker mkStringArray   >>=                 \v -> Just $ ProvideName v
    1048    -> maker mkWord32        >>=                 \v -> Just $ RequireFlags v
    1049    -> maker mkStringArray   >>=                 \v -> Just $ RequireName v
    1050    -> maker mkStringArray   >>=                 \v -> Just $ RequireVersion v
    1051    -> maker mkWord32        >>=                 \v -> Just $ NoSource v
    1052    -> maker mkWord32        >>=                 \v -> Just $ NoPatch v
    1053    -> maker mkWord32        >>=                 \v -> Just $ ConflictFlags v
    1054    -> maker mkStringArray   >>=                 \v -> Just $ ConflictName v
    1055    -> maker mkStringArray   >>=                 \v -> Just $ ConflictVersion v
    1056    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ DEPRECATED $ DefaultPrefix v
    1057    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ BuildRoot v
    1058    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ DEPRECATED $ InstallPrefix v
    1059    -> maker mkStringArray   >>=                 \v -> Just $ ExcludeArch v
    1060    -> maker mkStringArray   >>=                 \v -> Just $ ExcludeOS v
    1061    -> maker mkStringArray   >>=                 \v -> Just $ ExclusiveArch v
    1062    -> maker mkStringArray   >>=                 \v -> Just $ ExclusiveOS v
    1063    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ AutoReqProv v
    1064    -> maker mkString        >>=                 \v -> Just $ RPMVersion v
    1065    -> maker mkStringArray   >>=                 \v -> Just $ TriggerScripts v
    1066    -> maker mkStringArray   >>=                 \v -> Just $ TriggerName v
    1067    -> maker mkStringArray   >>=                 \v -> Just $ TriggerVersion v
    1068    -> maker mkWord32        >>=                 \v -> Just $ TriggerFlags v
    1069    -> maker mkWord32        >>=                 \v -> Just $ TriggerIndex v
    1079    -> maker mkString        >>=                 \v -> Just $ VerifyScript v
    1080    -> maker mkWord32        >>=                 \v -> Just $ ChangeLogTime v
    1081    -> maker mkStringArray   >>=                 \v -> Just $ ChangeLogName v
    1082    -> maker mkStringArray   >>=                 \v -> Just $ ChangeLogText v
    1083    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ BrokenMD5 v
    1084    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ PreReq v
    1085    -> maker mkStringArray   >>=                 \v -> Just $ PreInProg v
    1086    -> maker mkStringArray   >>=                 \v -> Just $ PostInProg v
    1087    -> maker mkStringArray   >>=                 \v -> Just $ PreUnProg v
    1088    -> maker mkStringArray   >>=                 \v -> Just $ PostUnProg v
    1089    -> maker mkStringArray   >>=                 \v -> Just $ BuildArchs v
    1090    -> maker mkStringArray   >>=                 \v -> Just $ ObsoleteName v
    1091    -> maker mkStringArray   >>=                 \v -> Just $ VerifyScriptProg v
    1092    -> maker mkStringArray   >>=                 \v -> Just $ TriggerScriptProg v
    1093    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ DocDir v
    1094    -> maker mkString        >>=                 \v -> Just $ Cookie v
    1095    -> maker mkWord32        >>=                 \v -> Just $ FileDevices v
    1096    -> maker mkWord32        >>=                 \v -> Just $ FileINodes v
    1097    -> maker mkStringArray   >>=                 \v -> Just $ FileLangs v
    1098    -> maker mkStringArray   >>=                 \v -> Just $ Prefixes v
    1099    -> maker mkStringArray   >>=                 \v -> Just $ InstPrefixes v
    1100    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TriggerIn v
    1101    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TriggerUn v
    1102    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TriggerPostUn v
    1103    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ AutoReq v
    1104    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ AutoProv v
    1105    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ INTERNAL $ OBSOLETE $ Capability v
    1106    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ SourcePackage v
    1107    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ OldOrigFileNames v
    1108    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ BuildPreReq v
    1109    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ BuildRequires v
    1110    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ BuildConflicts v
    1111    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ UNUSED $ BuildMacros v
    1112    -> maker mkWord32        >>=                 \v -> Just $ ProvideFlags v
    1113    -> maker mkStringArray   >>=                 \v -> Just $ ProvideVersion v
    1114    -> maker mkWord32        >>=                 \v -> Just $ ObsoleteFlags v
    1115    -> maker mkStringArray   >>=                 \v -> Just $ ObsoleteVersion v
    1116    -> maker mkWord32        >>=                 \v -> Just $ DirIndexes v
    1117    -> maker mkStringArray   >>=                 \v -> Just $ BaseNames v
    1118    -> maker mkStringArray   >>=                 \v -> Just $ DirNames v
    1119    -> maker mkWord32        >>=                 \v -> Just $ OrigDirIndexes v
    1120    -> maker mkStringArray   >>=                 \v -> Just $ OrigBaseNames v
    1121    -> maker mkStringArray   >>=                 \v -> Just $ OrigDirNames v
    1122    -> maker mkString        >>=                 \v -> Just $ OptFlags v
    1123    -> maker mkString        >>=                 \v -> Just $ DistURL v
    1124    -> maker mkString        >>=                 \v -> Just $ PayloadFormat v
    1125    -> maker mkString        >>=                 \v -> Just $ PayloadCompressor v
    1126    -> maker mkString        >>=                 \v -> Just $ PayloadFlags v
    1127    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ InstallColor v
    1128    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ InstallTID v
    1129    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ RemoveTID v
    1130    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ SHA1RHN v
    1131    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ RHNPlatform v
    1132    -> maker mkString        >>=                 \v -> Just $ Platform v
    1133    -> maker mkStringArray   >>=                 \v -> Just $ DEPRECATED $ PatchesName v
    1134    -> maker mkWord32        >>=                 \v -> Just $ DEPRECATED $ PatchesFlags v
    1135    -> maker mkStringArray   >>=                 \v -> Just $ DEPRECATED $ PatchesVersion v
    1136    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ INTERNAL $ OBSOLETE $ CacheCTime v
    1137    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ OBSOLETE $ CachePkgPath v
    1138    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ INTERNAL $ OBSOLETE $ CachePkgSize v
    1139    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ INTERNAL $ OBSOLETE $ CachePkgMTime v
    1140    -> maker mkWord32        >>=                 \v -> Just $ FileColors v
    1141    -> maker mkWord32        >>=                 \v -> Just $ FileClass v
    1142    -> maker mkStringArray   >>=                 \v -> Just $ ClassDict v
    1143    -> maker mkWord32        >>=                 \v -> Just $ FileDependsX v
    1144    -> maker mkWord32        >>=                 \v -> Just $ FileDependsN v
    1145    -> maker mkWord32        >>=                 \v -> Just $ DependsDict (map (\x -> ((x `shiftR` 24) .&. 0xff, x .&. 0x00ffffff)) v)
    1146    -> maker mkBinary        >>=                 \v -> Just $ SourcePkgID v
    1147    -> maker mkStringArray   >>=                 \v -> Just $ OBSOLETE $ FileContexts v
    1148    -> maker mkStringArray   >>=                 \v -> Just $ FSContexts v
    1149    -> maker mkStringArray   >>=                 \v -> Just $ ReContexts v
    1150    -> maker mkStringArray   >>=                 \v -> Just $ Policies v
    1151    -> maker mkString        >>=                 \v -> Just $ PreTrans v
    1152    -> maker mkString        >>=                 \v -> Just $ PostTrans v
    1153    -> maker mkStringArray   >>=                 \v -> Just $ PreTransProg v
    1154    -> maker mkStringArray   >>=                 \v -> Just $ PostTransProg v
    1155    -> maker mkString        >>=                 \v -> Just $ DistTag v
    1156    -> maker mkStringArray   >>=                 \v -> Just $ OBSOLETE $ OldSuggestsName v
    1157    -> maker mkStringArray   >>=                 \v -> Just $ OBSOLETE $ OldSuggestsVersion v
    1158    -> maker mkWord32        >>=                 \v -> Just $ OBSOLETE $ OldSuggestsFlags v
    1159    -> maker mkStringArray   >>=                 \v -> Just $ OBSOLETE $ OldEnhancesName v
    1160    -> maker mkStringArray   >>=                 \v -> Just $ OBSOLETE $ OldEnhancesVersion v
    1161    -> maker mkWord32        >>=                 \v -> Just $ OBSOLETE $ OldEnhancesFlags v
    1162    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ Priority v
    1163    -> maker mkString        >>=                 \v -> Just $ UNIMPLEMENTED $ CVSID v
    1164    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ BLinkPkgID v
    1165    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ BLinkHdrID v
    1166    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ BLinkNEVRA v
    1167    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ FLinkPkgID v
    1168    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ FLinkHdrID v
    1169    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ FLinkNEVRA v
    1170    -> maker mkString        >>=                 \v -> Just $ UNIMPLEMENTED $ PackageOrigin v
    1171    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TriggerPreIn v
    1172    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ UNIMPLEMENTED $ BuildSuggests v
    1173    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ UNIMPLEMENTED $ BuildEnhances v
    1174    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ ScriptStates v
    1175    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ ScriptMetrics v
    1176    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ UNIMPLEMENTED $ BuildCPUClock v
    1177    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ FileDigestAlgos v
    1178    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ Variants v
    1179    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ UNIMPLEMENTED $ XMajor v
    1180    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ UNIMPLEMENTED $ XMinor v
    1181    -> maker mkString        >>=                 \v -> Just $ UNIMPLEMENTED $ RepoTag v
    1182    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ Keywords v
    1183    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ BuildPlatforms v
    1184    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ UNIMPLEMENTED $ PackageColor v
    1185    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ UNIMPLEMENTED $ PackagePrefColor v
    1186    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ XattrsDict v
    1187    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ FileXattrsx v
    1188    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ DepAttrsDict v
    1189    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ ConflictAttrsx v
    1190    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ ObsoleteAttrsx v
    1191    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ ProvideAttrsx v
    1192    -> maker mkWord32        >>=                 \v -> Just $ UNIMPLEMENTED $ RequireAttrsx v
    1193    -> maker mkNull          >>=                 \v -> Just $ UNIMPLEMENTED $ BuildProvides v
    1194    -> maker mkNull          >>=                 \v -> Just $ UNIMPLEMENTED $ BuildObsoletes v
    1195    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ DBInstance v
    1196    -> maker mkString        >>=                 \v -> Just $ NVRA v

    5000    -> maker mkStringArray   >>=                 \v -> Just $ FileNames v
    5001    -> maker mkStringArray   >>=                 \v -> Just $ FileProvide v
    5002    -> maker mkStringArray   >>=                 \v -> Just $ FileRequire v
    5003    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ FSNames v
    5004    -> maker mkWord64        >>=                 \v -> Just $ UNIMPLEMENTED $ FSSizes v
    5005    -> maker mkStringArray   >>=                 \v -> Just $ TriggerConds v
    5006    -> maker mkStringArray   >>=                 \v -> Just $ TriggerType v
    5007    -> maker mkStringArray   >>=                 \v -> Just $ OrigFileNames v
    5008    -> maker mkWord64        >>=                 \v -> Just $ LongFileSizes v
    5009    -> maker mkWord64        >>= listToMaybe >>= \v -> Just $ LongSize v
    5010    -> maker mkStringArray   >>=                 \v -> Just $ FileCaps v
    5011    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ FileDigestAlgo v
    5012    -> maker mkString        >>=                 \v -> Just $ BugURL v
    5013    -> maker mkString        >>=                 \v -> Just $ EVR v
    5014    -> maker mkString        >>=                 \v -> Just $ NVR v
    5015    -> maker mkString        >>=                 \v -> Just $ NEVR v
    5016    -> maker mkString        >>=                 \v -> Just $ NEVRA v
    5017    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ HeaderColor v
    5018    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ Verbose v
    5019    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ EpochNum v
    5020    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ PreInFlags v
    5021    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ PostInFlags v
    5022    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ PreUnFlags v
    5023    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ PostUnFlags v
    5024    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ PreTransFlags v
    5025    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ PostTransFlags v
    5026    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ VerifyScriptFlags v
    5027    -> maker mkWord32        >>=                 \v -> Just $ TriggerScriptFlags v
    5029    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ Collections v
    5030    -> maker mkStringArray   >>=                 \v -> Just $ PolicyNames v
    5031    -> maker mkStringArray   >>=                 \v -> Just $ PolicyTypes v
    5032    -> maker mkWord32        >>=                 \v -> Just $ PolicyTypesIndexes v
    5033    -> maker mkWord32        >>=                 \v -> Just $ PolicyFlags v
    5034    -> maker mkString        >>=                 \v -> Just $ PolicyVCS v
    5035    -> maker mkStringArray   >>=                 \v -> Just $ OrderName v
    5036    -> maker mkStringArray   >>=                 \v -> Just $ OrderVersion v
    5037    -> maker mkWord32        >>=                 \v -> Just $ OrderFlags v
    5038    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ MSSFManifest v
    5039    -> maker mkStringArray   >>=                 \v -> Just $ UNIMPLEMENTED $ MSSFDomain v
    5040    -> maker mkStringArray   >>=                 \v -> Just $ InstFileNames v
    5041    -> maker mkStringArray   >>=                 \v -> Just $ RequireNEVRs v
    5042    -> maker mkStringArray   >>=                 \v -> Just $ ProvideNEVRs v
    5043    -> maker mkStringArray   >>=                 \v -> Just $ ObsoleteNEVRs v
    5044    -> maker mkStringArray   >>=                 \v -> Just $ ConflictNEVRs v
    5045    -> maker mkWord32        >>=                 \v -> Just $ FileNLinks v
    5046    -> maker mkStringArray   >>=                 \v -> Just $ RecommendName v
    5047    -> maker mkStringArray   >>=                 \v -> Just $ RecommendVersion v
    5048    -> maker mkWord32        >>=                 \v -> Just $ RecommendFlags v
    5049    -> maker mkStringArray   >>=                 \v -> Just $ SuggestName v
    5050    -> maker mkStringArray   >>=                 \v -> Just $ SuggestVersion v
    5051    -> maker mkWord32        >>=                 \v -> Just $ SuggestFlags v
    5052    -> maker mkStringArray   >>=                 \v -> Just $ SupplementName v
    5053    -> maker mkStringArray   >>=                 \v -> Just $ SupplementVersion v
    5054    -> maker mkWord32        >>=                 \v -> Just $ SupplementFlags v
    5055    -> maker mkStringArray   >>=                 \v -> Just $ EnhanceName v
    5056    -> maker mkStringArray   >>=                 \v -> Just $ EnhanceVersion v
    5057    -> maker mkWord32        >>=                 \v -> Just $ EnhanceFlags v
    5058    -> maker mkStringArray   >>=                 \v -> Just $ RecommendNEVRs v
    5059    -> maker mkStringArray   >>=                 \v -> Just $ SuggestNEVRs v
    5060    -> maker mkStringArray   >>=                 \v -> Just $ SupplementNEVRs v
    5061    -> maker mkStringArray   >>=                 \v -> Just $ EnhanceNEVRs v
    5062    -> maker mkString        >>=                 \v -> Just $ Encoding v
    5063    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ FileTriggerIn v
    5064    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ FileTriggerUn v
    5065    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ FileTriggerPostUn v
    5066    -> maker mkStringArray   >>=                 \v -> Just $ FileTriggerScripts v
    5067    -> maker mkStringArray   >>=                 \v -> Just $ FileTriggerScriptProg v
    5068    -> maker mkWord32        >>=                 \v -> Just $ FileTriggerScriptFlags v
    5069    -> maker mkStringArray   >>=                 \v -> Just $ FileTriggerName v
    5070    -> maker mkWord32        >>=                 \v -> Just $ FileTriggerIndex v
    5071    -> maker mkStringArray   >>=                 \v -> Just $ FileTriggerVersion v
    5072    -> maker mkWord32        >>=                 \v -> Just $ FileTriggerFlags v
    5073    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TransFileTriggerIn v
    5074    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TransFileTriggerUn v
    5075    -> maker mkNull          >>=                 \v -> Just $ INTERNAL $ TransFileTriggerPostUn v
    5076    -> maker mkStringArray   >>=                 \v -> Just $ TransFileTriggerScripts v
    5077    -> maker mkStringArray   >>=                 \v -> Just $ TransFileTriggerScriptProg v
    5078    -> maker mkWord32        >>=                 \v -> Just $ TransFileTriggerScriptFlags v
    5079    -> maker mkStringArray   >>=                 \v -> Just $ TransFileTriggerName v
    5080    -> maker mkWord32        >>=                 \v -> Just $ TransFileTriggerIndex v
    5081    -> maker mkStringArray   >>=                 \v -> Just $ TransFileTriggerVersion v
    5082    -> maker mkWord32        >>=                 \v -> Just $ TransFileTriggerFlags v
    5083    -> maker mkString        >>=                 \v -> Just $ INTERNAL $ RemovePathPostFixes v
    5084    -> maker mkWord32        >>=                 \v -> Just $ FileTriggerPriorities v
    5085    -> maker mkWord32        >>=                 \v -> Just $ TransFileTriggerPriorities v
    5086    -> maker mkStringArray   >>=                 \v -> Just $ FileTriggerConds v
    5087    -> maker mkStringArray   >>=                 \v -> Just $ FileTriggerType v
    5088    -> maker mkStringArray   >>=                 \v -> Just $ TransFileTriggerConds v
    5089    -> maker mkStringArray   >>=                 \v -> Just $ TransFileTriggerType v
    5090    -> maker mkStringArray   >>=                 \v -> Just $ FileSignatures v
    5091    -> maker mkWord32        >>= listToMaybe >>= \v -> Just $ FileSignatureLength v

    _       -> Nothing
 where
    maker fn = fn store ty offset count

mkNull :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Null
mkNull _ ty _ _ | ty == 0    = Just Null
                | otherwise  = Nothing

mkChar :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Char]
mkChar store ty offset count | ty == 1   = Just $ C.unpack $ BS.take count' start
                             | otherwise = Nothing
 where
    count' = fromIntegral count
    start = BS.drop (fromIntegral offset) store

mkWord16 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word16]
mkWord16 store ty offset count | ty == 3     = Just $ readWords store 2 asWord16 offsets
                               | otherwise   = Nothing
 where
    offsets = map (\n -> offset + (n*2)) [0 .. count-1]

mkWord32 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word32]
mkWord32 store ty offset count | ty == 4     = Just $ readWords store 4 asWord32 offsets
                               | otherwise   = Nothing
 where
    offsets = map (\n -> offset + (n*4)) [0 .. count-1]

mkWord64 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [Word64]
mkWord64 store ty offset count | ty == 5     = Just $ readWords store 8 asWord64 offsets
                               | otherwise   = Nothing
 where
    offsets = map (\n -> offset + (n*8)) [0 .. count-1]

mkString :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe String
mkString store ty offset _ | ty == 6   = Just $ C.unpack $ BS.takeWhile (/= 0) start
                           | otherwise = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkBinary :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe BS.ByteString
mkBinary store ty offset count | ty == 7     = Just $ BS.take count' start
                               | otherwise   = Nothing
 where
    count' = fromIntegral count
    start  = BS.drop (fromIntegral offset) store

mkStringArray :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe [String]
mkStringArray store ty offset count | ty == 8    = Just $ map C.unpack $ readStrings start count
                                    | otherwise  = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkI18NString :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe BS.ByteString
mkI18NString store ty offset _ | ty == 9     = Just $ BS.takeWhile (/= 0) start
                               | otherwise   = Nothing
 where
    start  = BS.drop (fromIntegral offset) store

-- I don't know how to split a ByteString up into chunks of a given size, so here's what I'm doing.  Take
-- a list of offsets of where in the ByteString to read.  Skip to each of those offsets, grab size bytes, and
-- convert those bytes into the type using the given conversion function.  Return that list.
{-# ANN readWords "HLint: ignore Eta reduce" #-}
readWords :: BS.ByteString -> Int -> (BS.ByteString -> a) -> [Word32] -> [a]
readWords bs size conv offsets = map (\offset -> conv $ BS.take size $ BS.drop (fromIntegral offset) bs) offsets

readStrings :: BS.ByteString -> Word32 -> [BS.ByteString]
readStrings bytestring count  = take (fromIntegral count) $ BS.split 0 bytestring

-- | Given a 'Tag' name and a list of 'Tag's, find the match and return it as a Maybe.
findTag :: String -> [Tag] -> Maybe Tag
findTag name = find (\t -> name == showConstr (toConstr t))

-- | Given a 'Tag' name and a list of 'Tag's, find the match, convert it into a
-- 'ByteString', and return it as a Maybe.
findByteStringTag :: String -> [Tag] -> Maybe BS.ByteString
findByteStringTag name tags = findTag name tags >>= \t -> tagValue t :: Maybe BS.ByteString

-- | Given a 'Tag' name and a list of 'Tag's, find the match, convert it into a
-- String, and return it as a Maybe.
findStringTag :: String -> [Tag] -> Maybe String
findStringTag name tags = findTag name tags >>= \t -> tagValue t :: Maybe String

-- | Given a 'Tag' name and a list of 'Tag's, find all matches, convert them into
-- Strings, and return as a list.  If no results are found, return an empty list.
findStringListTag :: String -> [Tag] -> [String]
findStringListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [String]

-- | Given a 'Tag' name and a list of 'Tag's, find the match convert it into a
-- Word16, and return it as a Maybe.
findWord16Tag :: String -> [Tag] -> Maybe Word16
findWord16Tag name tags = findTag name tags >>= \t -> tagValue t :: Maybe Word16

-- | Given a 'Tag' name and a list of 'Tag's, find all matches, convert them into
-- Word16, and return as a list.  if no results are found, return an empty list.
findWord16ListTag :: String -> [Tag] -> [Word16]
findWord16ListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [Word16]

-- | Given a 'Tag' name and a list of 'Tag's, find the match convert it into a
-- Word32, and return it as a Maybe.
findWord32Tag :: String -> [Tag] -> Maybe Word32
findWord32Tag name tags = findTag name tags >>= \t -> tagValue t :: Maybe Word32

-- | Given a 'Tag' name and a list of 'Tag's, find all matches, convert them into
-- Word32, and return as a list.  if no results are found, return an empty list.
findWord32ListTag :: String -> [Tag] -> [Word32]
findWord32ListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [Word32]

-- | Given a 'Tag', return its type.
tagValue :: Typeable a => Tag -> Maybe a
tagValue = gmapQi 0 cast
