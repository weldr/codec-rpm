module RPM.Tags(Tag(..),
                NullTag(..),
                CharTag(..),
                Word8Tag(..),
                Word16Tag(..),
                Word32Tag(..),
                Word64Tag(..),
                StringTag(..),
                BinaryTag(..),
                StringArrayTag(..),
                I18NStringTag(..),
                mkTag)
 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Word
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint(text)

import RPM.Internal.Numbers

data Tag = HeaderImage              NullTag Word32 Word32
         | HeaderSignatures         NullTag Word32 Word32
         | HeaderImmutable          NullTag Word32 Word32
         | HeaderRegions            NullTag Word32 Word32
         | HeaderI18NTable          StringArrayTag Word32 Word32

         | SigBase                  NullTag Word32 Word32
         | SigSize                  Word32Tag Word32 Word32
         | SigLEMD5_1               NullTag Word32 Word32
         | SigPGP                   BinaryTag Word32 Word32
         | SigLEMD5_2               NullTag Word32 Word32
         | SigMD5                   BinaryTag Word32 Word32
         | SigGPG                   BinaryTag Word32 Word32
         | SigPGP5                  NullTag Word32 Word32
         | SigBadSHA1_1             NullTag Word32 Word32
         | SigBadSHA1_2             NullTag Word32 Word32
         | PubKeys                  StringArrayTag Word32 Word32
         | DSAHeader                BinaryTag Word32 Word32
         | RSAHeader                BinaryTag Word32 Word32
         | SHA1Header               StringTag Word32 Word32
         | LongSigSize              Word64Tag Word32 Word32
         | LongArchiveSize          Word64Tag Word32 Word32

         | Name                     StringTag Word32 Word32
         | Version                  StringTag Word32 Word32
         | Release                  StringTag Word32 Word32
         | Epoch                    Word32Tag Word32 Word32
         | Summary                  I18NStringTag Word32 Word32
         | Description              I18NStringTag Word32 Word32
         | BuildTime                Word32Tag Word32 Word32
         | BuildHost                StringTag Word32 Word32
         | InstallTime              Word32Tag Word32 Word32
         | Size                     Word32Tag Word32 Word32
         | Distribution             StringTag Word32 Word32
         | Vendor                   StringTag Word32 Word32
         | GIF                      BinaryTag Word32 Word32
         | XPM                      BinaryTag Word32 Word32
         | License                  StringTag Word32 Word32
         | Packager                 StringTag Word32 Word32
         | Group                    I18NStringTag Word32 Word32
         | ChangeLog                StringArrayTag Word32 Word32
         | Source                   StringArrayTag Word32 Word32
         | Patch                    StringArrayTag Word32 Word32
         | URL                      StringTag Word32 Word32
         | OS                       StringTag Word32 Word32
         | Arch                     StringTag Word32 Word32
         | PreIn                    StringTag Word32 Word32
         | PostIn                   StringTag Word32 Word32
         | PreUn                    StringTag Word32 Word32
         | PostUn                   StringTag Word32 Word32
         | OldFileNames             StringArrayTag Word32 Word32
         | FileSizes                Word32Tag Word32 Word32
         | FileStates               CharTag Word32 Word32
         | FileModes                Word16Tag Word32 Word32
         | FileUIDs                 Word32Tag Word32 Word32
         | FileGIDs                 Word32Tag Word32 Word32
         | FileRDevs                Word16Tag Word32 Word32
         | FileMTimes               Word32Tag Word32 Word32
         | FileMD5s                 StringArrayTag Word32 Word32
         | FileLinkTos              StringArrayTag Word32 Word32
         | FileFlags                Word32Tag Word32 Word32
         | Root                     NullTag Word32 Word32
         | FileUserName             StringArrayTag Word32 Word32
         | FileGroupName            StringArrayTag Word32 Word32
         | Exclude                  NullTag Word32 Word32
         | Exclusive                NullTag Word32 Word32
         | Icon                     BinaryTag Word32 Word32
         | SourceRPM                StringTag Word32 Word32
         | FileVerifyFlags          Word32Tag Word32 Word32
         | ArchiveSize              Word32Tag Word32 Word32
         | ProvideName              StringArrayTag Word32 Word32
         | RequireFlags             Word32Tag Word32 Word32
         | RequireName              StringArrayTag Word32 Word32
         | RequireVersion           StringArrayTag Word32 Word32
         | NoSource                 Word32Tag Word32 Word32
         | NoPatch                  Word32Tag Word32 Word32
         | ConflictFlags            Word32Tag Word32 Word32
         | ConflictName             StringArrayTag Word32 Word32
         | ConflictVersion          StringArrayTag Word32 Word32
         | DefaultPrefix            StringTag Word32 Word32
         | BuildRoot                StringTag Word32 Word32
         | InstallPrefix            StringTag Word32 Word32
         | ExcludeArch              StringArrayTag Word32 Word32
         | ExcludeOS                StringArrayTag Word32 Word32
         | ExclusiveArch            StringArrayTag Word32 Word32
         | ExclusiveOS              StringArrayTag Word32 Word32
         | AutoReqProv              StringTag Word32 Word32
         | RPMVersion               StringTag Word32 Word32
         | TriggerScripts           StringArrayTag Word32 Word32
         | TriggerName              StringArrayTag Word32 Word32
         | TriggerVersion           StringArrayTag Word32 Word32
         | TriggerFlags             Word32Tag Word32 Word32
         | TriggerIndex             Word32Tag Word32 Word32
         | VerifyScript             StringTag Word32 Word32
         | ChangeLogTime            Word32Tag Word32 Word32
         | ChangeLogName            StringArrayTag Word32 Word32
         | ChangeLogText            StringArrayTag Word32 Word32
         | BrokenMD5                NullTag Word32 Word32
         | PreReq                   NullTag Word32 Word32
         | PreInProg                StringArrayTag Word32 Word32
         | PostInProg               StringArrayTag Word32 Word32
         | PreUnProg                StringArrayTag Word32 Word32
         | PostUnProg               StringArrayTag Word32 Word32
         | BuildArchs               StringArrayTag Word32 Word32
         | ObsoleteName             StringArrayTag Word32 Word32
         | VerifyScriptProg         StringArrayTag Word32 Word32
         | TriggerScriptProg        StringArrayTag Word32 Word32
         | DocDir                   NullTag Word32 Word32
         | Cookie                   StringTag Word32 Word32
         | FileDevices              Word32Tag Word32 Word32
         | FileINodes               Word32Tag Word32 Word32
         | FileLangs                StringArrayTag Word32 Word32
         | Prefixes                 StringArrayTag Word32 Word32
         | InstPrefixes             StringArrayTag Word32 Word32
         | TriggerIn                NullTag Word32 Word32
         | TriggerUn                NullTag Word32 Word32
         | TriggerPostUn            NullTag Word32 Word32
         | AutoReq                  NullTag Word32 Word32
         | AutoProv                 NullTag Word32 Word32
         | Capability               Word32Tag Word32 Word32
         | SourcePackage            Word32Tag Word32 Word32
         | OldOrigFileNames         NullTag Word32 Word32
         | BuildPreReq              NullTag Word32 Word32
         | BuildRequires            NullTag Word32 Word32
         | BuildConflicts           NullTag Word32 Word32
         | BuildMacros              NullTag Word32 Word32
         | ProvideFlags             Word32Tag Word32 Word32
         | ProvideVersion           StringArrayTag Word32 Word32
         | ObsoleteFlags            Word32Tag Word32 Word32
         | ObsoleteVersion          StringArrayTag Word32 Word32
         | DirIndexes               Word32Tag Word32 Word32
         | BaseNames                StringArrayTag Word32 Word32
         | DirNames                 StringArrayTag Word32 Word32
         | OrigDirIndexes           Word32Tag Word32 Word32
         | OrigBaseNames            StringArrayTag Word32 Word32
         | OrigDirNames             StringArrayTag Word32 Word32
         | OptFlags                 StringTag Word32 Word32
         | DistURL                  StringTag Word32 Word32
         | PayloadFormat            StringTag Word32 Word32
         | PayloadCompressor        StringTag Word32 Word32
         | PayloadFlags             StringTag Word32 Word32
         | InstallColor             Word32Tag Word32 Word32
         | InstallTID               Word32Tag Word32 Word32
         | RemoveTID                Word32Tag Word32 Word32
         | SHA1RHN                  NullTag Word32 Word32
         | RHNPlatform              StringTag Word32 Word32
         | Platform                 StringTag Word32 Word32
         | PatchesName              StringArrayTag Word32 Word32
         | PatchesFlags             Word32Tag Word32 Word32
         | PatchesVersion           StringArrayTag Word32 Word32
         | CacheCTime               Word32Tag Word32 Word32
         | CachePkgPath             StringTag Word32 Word32
         | CachePkgSize             Word32Tag Word32 Word32
         | CachePkgMTime            Word32Tag Word32 Word32
         | FileColors               Word32Tag Word32 Word32
         | FileClass                Word32Tag Word32 Word32
         | ClassDict                StringArrayTag Word32 Word32
         | FileDependsX             Word32Tag Word32 Word32
         | FileDependsN             Word32Tag Word32 Word32
         | DependsDict              Word32Tag Word32 Word32
         | SourcePkgID              BinaryTag Word32 Word32
         | FileContexts             StringArrayTag Word32 Word32
         | FSContexts               StringArrayTag Word32 Word32
         | ReContexts               StringArrayTag Word32 Word32
         | Policies                 StringArrayTag Word32 Word32
         | PreTrans                 StringTag Word32 Word32
         | PostTrans                StringTag Word32 Word32
         | PreTransProg             StringArrayTag Word32 Word32
         | PostTransProg            StringArrayTag Word32 Word32
         | DistTag                  StringTag Word32 Word32
         | OldSuggestsName          StringArrayTag Word32 Word32
         | OldSuggestsVersion       StringArrayTag Word32 Word32
         | OldSuggestsFlags         Word32Tag Word32 Word32
         | OldEnhancesName          StringArrayTag Word32 Word32
         | OldEnhancesVersion       StringArrayTag Word32 Word32
         | OldEnhancesFlags         Word32Tag Word32 Word32
         | Priority                 Word32Tag Word32 Word32
         | CVSID                    StringTag Word32 Word32
         | BLinkPkgID               StringArrayTag Word32 Word32
         | BLinkHdrID               StringArrayTag Word32 Word32
         | BLinkNEVRA               StringArrayTag Word32 Word32
         | FLinkPkgID               StringArrayTag Word32 Word32
         | FLinkHdrID               StringArrayTag Word32 Word32
         | FLinkNEVRA               StringArrayTag Word32 Word32
         | PackageOrigin            StringTag Word32 Word32
         | TriggerPreIn             NullTag Word32 Word32
         | BuildSuggests            NullTag Word32 Word32
         | BuildEnhances            NullTag Word32 Word32
         | ScriptStates             Word32Tag Word32 Word32
         | ScriptMetrics            Word32Tag Word32 Word32
         | BuildCPUClock            Word32Tag Word32 Word32
         | FileDigestAlgos          Word32Tag Word32 Word32
         | Variants                 StringArrayTag Word32 Word32
         | XMajor                   Word32Tag Word32 Word32
         | XMinor                   Word32Tag Word32 Word32
         | RepoTag                  StringTag Word32 Word32
         | Keywords                 StringArrayTag Word32 Word32
         | BuildPlatforms           StringArrayTag Word32 Word32
         | PackageColor             Word32Tag Word32 Word32
         | PackagePrefColor         Word32Tag Word32 Word32
         | XattrsDict               StringArrayTag Word32 Word32
         | FileXattrsx              Word32Tag Word32 Word32
         | DepAttrsDict             StringArrayTag Word32 Word32
         | ConflictAttrsx           Word32Tag Word32 Word32
         | ObsoleteAttrsx           Word32Tag Word32 Word32
         | ProvideAttrsx            Word32Tag Word32 Word32
         | RequireAttrsx            Word32Tag Word32 Word32
         | BuildProvides            NullTag Word32 Word32
         | BuildObsoletes           NullTag Word32 Word32
         | DBInstance               Word32Tag Word32 Word32
         | NVRA                     StringTag Word32 Word32

         | FileNames                StringArrayTag Word32 Word32
         | FileProvide              StringArrayTag Word32 Word32
         | FileRequire              StringArrayTag Word32 Word32
         | FSNames                  StringArrayTag Word32 Word32
         | FSSizes                  Word64Tag Word32 Word32
         | TriggerConds             StringArrayTag Word32 Word32
         | TriggerType              StringArrayTag Word32 Word32
         | OrigFileNames            StringArrayTag Word32 Word32
         | LongFileSizes            Word64Tag Word32 Word32
         | LongSize                 Word64Tag Word32 Word32
         | FileCaps                 StringArrayTag Word32 Word32
         | FileDigestAlgo           Word32Tag Word32 Word32
         | BugURL                   StringTag Word32 Word32
         | EVR                      StringTag Word32 Word32
         | NVR                      StringTag Word32 Word32
         | NEVR                     StringTag Word32 Word32
         | NEVRA                    StringTag Word32 Word32
         | HeaderColor              Word32Tag Word32 Word32
         | Verbose                  Word32Tag Word32 Word32
         | EpochNum                 Word32Tag Word32 Word32
         | PreInFlags               Word32Tag Word32 Word32
         | PostInFlags              Word32Tag Word32 Word32
         | PreUnFlags               Word32Tag Word32 Word32
         | PostUnFlags              Word32Tag Word32 Word32
         | PreTransFlags            Word32Tag Word32 Word32
         | PostTransFlags           Word32Tag Word32 Word32
         | VerifyScriptFlags        Word32Tag Word32 Word32
         | TriggerScriptFlags       Word32Tag Word32 Word32
         | Collections              StringArrayTag Word32 Word32
         | PolicyNames              StringArrayTag Word32 Word32
         | PolicyTypes              StringArrayTag Word32 Word32
         | PolicyTypesIndexes       Word32Tag Word32 Word32
         | PolicyFlags              Word32Tag Word32 Word32
         | PolicyVCS                StringTag Word32 Word32
         | OrderName                StringArrayTag Word32 Word32
         | OrderVersion             StringArrayTag Word32 Word32
         | OrderFlags               Word32Tag Word32 Word32
         | MSSFManifest             StringArrayTag Word32 Word32
         | MSSFDomain               StringArrayTag Word32 Word32
         | InstFileNames            StringArrayTag Word32 Word32
         | RequireNEVRs             StringArrayTag Word32 Word32
         | ProvideNEVRs             StringArrayTag Word32 Word32
         | ObsoleteNEVRs            StringArrayTag Word32 Word32
         | ConflictNEVRs            StringArrayTag Word32 Word32
         | FileNLinks               Word32Tag Word32 Word32
         | RecommendName            StringArrayTag Word32 Word32
         | RecommendVersion         StringArrayTag Word32 Word32
         | RecommendFlags           Word32Tag Word32 Word32
         | SuggestName              StringArrayTag Word32 Word32
         | SuggestVersion           StringArrayTag Word32 Word32
         | SuggestFlags             Word32Tag Word32 Word32
         | SupplementName           StringArrayTag Word32 Word32
         | SupplementVersion        StringArrayTag Word32 Word32
         | SupplementFlags          Word32Tag Word32 Word32
         | EnhanceName              StringArrayTag Word32 Word32
         | EnhanceVersion           StringArrayTag Word32 Word32
         | EnhanceFlags             Word32Tag Word32 Word32
         | RecommendNEVRs           StringArrayTag Word32 Word32
         | SuggestNEVRs             StringArrayTag Word32 Word32
         | SupplementNEVRs          StringArrayTag Word32 Word32
         | EnhanceNEVRs             StringArrayTag Word32 Word32
         | Encoding                 StringTag Word32 Word32
         | FileTriggerIn            NullTag Word32 Word32
         | FileTriggerUn            NullTag Word32 Word32
         | FileTriggerPostUn        NullTag Word32 Word32
         | FileTriggerScripts       StringArrayTag Word32 Word32
         | FileTriggerScriptProg    StringArrayTag Word32 Word32
         | FileTriggerScriptFlags   Word32Tag Word32 Word32
         | FileTriggerName          StringArrayTag Word32 Word32
         | FileTriggerIndex         Word32Tag Word32 Word32
         | FileTriggerVersion       StringArrayTag Word32 Word32
         | FileTriggerFlags         Word32Tag Word32 Word32
         | TransFileTriggerIn       NullTag Word32 Word32
         | TransFileTriggerUn       NullTag Word32 Word32
         | TransFileTriggerPostUn   NullTag Word32 Word32
         | TransFileTriggerScripts  StringArrayTag Word32 Word32
         | TransFileTriggerScriptProg    StringArrayTag Word32 Word32
         | TransFileTriggerScriptFlags   Word32Tag Word32 Word32
         | TransFileTriggerName     StringArrayTag Word32 Word32
         | TransFileTriggerIndex    Word32Tag Word32 Word32
         | TransFileTriggerVersion  StringArrayTag Word32 Word32
         | TransFileTriggerFlags    Word32Tag Word32 Word32
         | RemovePathPostFixes      StringTag Word32 Word32
         | FileTriggerPriorities    Word32Tag Word32 Word32
         | TransFileTriggerPriorities    Word32Tag Word32 Word32
         | FileTriggerConds         StringArrayTag Word32 Word32
         | FileTriggerType          StringArrayTag Word32 Word32
         | TransFileTriggerConds    StringArrayTag Word32 Word32
         | TransFileTriggerType     StringArrayTag Word32 Word32
         | FileSignatures           StringArrayTag Word32 Word32
         | FileSignatureLength      Word32Tag Word32 Word32
  deriving(Eq, Show)

instance Pretty Tag where
    -- Drop the last two Word32s from the pretty printed version of a tag, since who cares?
    -- This is a lot quicker than having to provide a Pretty instance that takes every
    -- single Tag into account.
    pPrint t = text . unwords $ init . init $ words (show t)

data    NullTag         = NullTag
 deriving(Eq, Show)

newtype CharTag         = CharTag Char
 deriving(Eq, Show)

newtype Word8Tag        = Word8Tag [Word8]
 deriving(Eq, Show)

newtype Word16Tag       = Word16Tag [Word16]
 deriving(Eq, Show)

newtype Word32Tag       = Word32Tag [Word32]
 deriving(Eq, Show)

newtype Word64Tag       = Word64Tag [Word64]
 deriving(Eq, Show)

newtype StringTag       = StringTag String
 deriving(Eq, Show)

newtype BinaryTag       = BinaryTag BS.ByteString
 deriving(Eq, Show)

newtype StringArrayTag  = StringArrayTag [String]
 deriving(Eq, Show)

newtype I18NStringTag   = I18NStringTag BS.ByteString
 deriving(Eq, Show)

mkTag :: BS.ByteString -> Int -> Word32 -> Word32 -> Word32 -> Maybe Tag
mkTag store tag ty offset count = case tag of
    61      -> maker mkNullTag          >>= \v -> Just $ HeaderImage v offset count
    62      -> maker mkNullTag          >>= \v -> Just $ HeaderSignatures v offset count
    63      -> maker mkNullTag          >>= \v -> Just $ HeaderImmutable v offset count
    64      -> maker mkNullTag          >>= \v -> Just $ HeaderRegions v offset count
    100     -> maker mkStringArrayTag   >>= \v -> Just $ HeaderI18NTable v offset count

    256     -> maker mkNullTag          >>= \v -> Just $ SigBase v offset count
    257     -> maker mkWord32Tag        >>= \v -> Just $ SigSize v offset count
    258     -> maker mkNullTag          >>= \v -> Just $ SigLEMD5_1 v offset count
    259     -> maker mkBinaryTag        >>= \v -> Just $ SigPGP v offset count
    260     -> maker mkNullTag          >>= \v -> Just $ SigLEMD5_2 v offset count
    261     -> maker mkBinaryTag        >>= \v -> Just $ SigMD5 v offset count
    262     -> maker mkBinaryTag        >>= \v -> Just $ SigGPG v offset count
    263     -> maker mkNullTag          >>= \v -> Just $ SigPGP5 v offset count
    264     -> maker mkNullTag          >>= \v -> Just $ SigBadSHA1_1 v offset count
    265     -> maker mkNullTag          >>= \v -> Just $ SigBadSHA1_2 v offset count
    266     -> maker mkStringArrayTag   >>= \v -> Just $ PubKeys v offset count
    267     -> maker mkBinaryTag        >>= \v -> Just $ DSAHeader v offset count
    268     -> maker mkBinaryTag        >>= \v -> Just $ RSAHeader v offset count
    269     -> maker mkStringTag        >>= \v -> Just $ SHA1Header v offset count
    270     -> maker mkWord64Tag        >>= \v -> Just $ LongSigSize v offset count
    271     -> maker mkWord64Tag        >>= \v -> Just $ LongArchiveSize v offset count

    1000    -> maker mkStringTag        >>= \v -> Just $ Name v offset count
    1001    -> maker mkStringTag        >>= \v -> Just $ Version v offset count
    1002    -> maker mkStringTag        >>= \v -> Just $ Release v offset count
    1003    -> maker mkWord32Tag        >>= \v -> Just $ Epoch v offset count
    1004    -> maker mkI18NStringTag    >>= \v -> Just $ Summary v offset count
    1005    -> maker mkI18NStringTag    >>= \v -> Just $ Description v offset count
    1006    -> maker mkWord32Tag        >>= \v -> Just $ BuildTime v offset count
    1007    -> maker mkStringTag        >>= \v -> Just $ BuildHost v offset count
    1008    -> maker mkWord32Tag        >>= \v -> Just $ InstallTime v offset count
    1009    -> maker mkWord32Tag        >>= \v -> Just $ Size v offset count
    1010    -> maker mkStringTag        >>= \v -> Just $ Distribution v offset count
    1011    -> maker mkStringTag        >>= \v -> Just $ Vendor v offset count
    1012    -> maker mkBinaryTag        >>= \v -> Just $ GIF v offset count
    1013    -> maker mkBinaryTag        >>= \v -> Just $ XPM v offset count
    1014    -> maker mkStringTag        >>= \v -> Just $ License v offset count
    1015    -> maker mkStringTag        >>= \v -> Just $ Packager v offset count
    1016    -> maker mkI18NStringTag    >>= \v -> Just $ Group v offset count
    1017    -> maker mkStringArrayTag   >>= \v -> Just $ ChangeLog v offset count
    1018    -> maker mkStringArrayTag   >>= \v -> Just $ Source v offset count
    1019    -> maker mkStringArrayTag   >>= \v -> Just $ Patch v offset count
    1020    -> maker mkStringTag        >>= \v -> Just $ URL v offset count
    1021    -> maker mkStringTag        >>= \v -> Just $ OS v offset count
    1022    -> maker mkStringTag        >>= \v -> Just $ Arch v offset count
    1023    -> maker mkStringTag        >>= \v -> Just $ PreIn v offset count
    1024    -> maker mkStringTag        >>= \v -> Just $ PostIn v offset count
    1025    -> maker mkStringTag        >>= \v -> Just $ PreUn v offset count
    1026    -> maker mkStringTag        >>= \v -> Just $ PostUn v offset count
    1027    -> maker mkStringArrayTag   >>= \v -> Just $ OldFileNames v offset count
    1028    -> maker mkWord32Tag        >>= \v -> Just $ FileSizes v offset count
    1029    -> maker mkCharTag          >>= \v -> Just $ FileStates v offset count
    1030    -> maker mkWord16Tag        >>= \v -> Just $ FileModes v offset count
    1031    -> maker mkWord32Tag        >>= \v -> Just $ FileUIDs v offset count
    1032    -> maker mkWord32Tag        >>= \v -> Just $ FileGIDs v offset count
    1033    -> maker mkWord16Tag        >>= \v -> Just $ FileRDevs v offset count
    1034    -> maker mkWord32Tag        >>= \v -> Just $ FileMTimes v offset count
    1035    -> maker mkStringArrayTag   >>= \v -> Just $ FileMD5s v offset count
    1036    -> maker mkStringArrayTag   >>= \v -> Just $ FileLinkTos v offset count
    1037    -> maker mkWord32Tag        >>= \v -> Just $ FileFlags v offset count
    1038    -> maker mkNullTag          >>= \v -> Just $ Root v offset count
    1039    -> maker mkStringArrayTag   >>= \v -> Just $ FileUserName v offset count
    1040    -> maker mkStringArrayTag   >>= \v -> Just $ FileGroupName v offset count
    1041    -> maker mkNullTag          >>= \v -> Just $ Exclude v offset count
    1042    -> maker mkNullTag          >>= \v -> Just $ Exclusive v offset count
    1043    -> maker mkBinaryTag        >>= \v -> Just $ Icon v offset count
    1044    -> maker mkStringTag        >>= \v -> Just $ SourceRPM v offset count
    1045    -> maker mkWord32Tag        >>= \v -> Just $ FileVerifyFlags v offset count
    1046    -> maker mkWord32Tag        >>= \v -> Just $ ArchiveSize v offset count
    1047    -> maker mkStringArrayTag   >>= \v -> Just $ ProvideName v offset count
    1048    -> maker mkWord32Tag        >>= \v -> Just $ RequireFlags v offset count
    1049    -> maker mkStringArrayTag   >>= \v -> Just $ RequireName v offset count
    1050    -> maker mkStringArrayTag   >>= \v -> Just $ RequireVersion v offset count
    1051    -> maker mkWord32Tag        >>= \v -> Just $ NoSource v offset count
    1052    -> maker mkWord32Tag        >>= \v -> Just $ NoPatch v offset count
    1053    -> maker mkWord32Tag        >>= \v -> Just $ ConflictFlags v offset count
    1054    -> maker mkStringArrayTag   >>= \v -> Just $ ConflictName v offset count
    1055    -> maker mkStringArrayTag   >>= \v -> Just $ ConflictVersion v offset count
    1056    -> maker mkStringTag        >>= \v -> Just $ DefaultPrefix v offset count
    1057    -> maker mkStringTag        >>= \v -> Just $ BuildRoot v offset count
    1058    -> maker mkStringTag        >>= \v -> Just $ InstallPrefix v offset count
    1059    -> maker mkStringArrayTag   >>= \v -> Just $ ExcludeArch v offset count
    1060    -> maker mkStringArrayTag   >>= \v -> Just $ ExcludeOS v offset count
    1061    -> maker mkStringArrayTag   >>= \v -> Just $ ExclusiveArch v offset count
    1062    -> maker mkStringArrayTag   >>= \v -> Just $ ExclusiveOS v offset count
    1063    -> maker mkStringTag        >>= \v -> Just $ AutoReqProv v offset count
    1064    -> maker mkStringTag        >>= \v -> Just $ RPMVersion v offset count
    1065    -> maker mkStringArrayTag   >>= \v -> Just $ TriggerScripts v offset count
    1066    -> maker mkStringArrayTag   >>= \v -> Just $ TriggerName v offset count
    1067    -> maker mkStringArrayTag   >>= \v -> Just $ TriggerVersion v offset count
    1068    -> maker mkWord32Tag        >>= \v -> Just $ TriggerFlags v offset count
    1069    -> maker mkWord32Tag        >>= \v -> Just $ TriggerIndex v offset count
    1079    -> maker mkStringTag        >>= \v -> Just $ VerifyScript v offset count
    1080    -> maker mkWord32Tag        >>= \v -> Just $ ChangeLogTime v offset count
    1081    -> maker mkStringArrayTag   >>= \v -> Just $ ChangeLogName v offset count
    1082    -> maker mkStringArrayTag   >>= \v -> Just $ ChangeLogText v offset count
    1083    -> maker mkNullTag          >>= \v -> Just $ BrokenMD5 v offset count
    1084    -> maker mkNullTag          >>= \v -> Just $ PreReq v offset count
    1085    -> maker mkStringArrayTag   >>= \v -> Just $ PreInProg v offset count
    1086    -> maker mkStringArrayTag   >>= \v -> Just $ PostInProg v offset count
    1087    -> maker mkStringArrayTag   >>= \v -> Just $ PreUnProg v offset count
    1088    -> maker mkStringArrayTag   >>= \v -> Just $ PostUnProg v offset count
    1089    -> maker mkStringArrayTag   >>= \v -> Just $ BuildArchs v offset count
    1090    -> maker mkStringArrayTag   >>= \v -> Just $ ObsoleteName v offset count
    1091    -> maker mkStringArrayTag   >>= \v -> Just $ VerifyScriptProg v offset count
    1092    -> maker mkStringArrayTag   >>= \v -> Just $ TriggerScriptProg v offset count
    1093    -> maker mkNullTag          >>= \v -> Just $ DocDir v offset count
    1094    -> maker mkStringTag        >>= \v -> Just $ Cookie v offset count
    1095    -> maker mkWord32Tag        >>= \v -> Just $ FileDevices v offset count
    1096    -> maker mkWord32Tag        >>= \v -> Just $ FileINodes v offset count
    1097    -> maker mkStringArrayTag   >>= \v -> Just $ FileLangs v offset count
    1098    -> maker mkStringArrayTag   >>= \v -> Just $ Prefixes v offset count
    1099    -> maker mkStringArrayTag   >>= \v -> Just $ InstPrefixes v offset count
    1100    -> maker mkNullTag          >>= \v -> Just $ TriggerIn v offset count
    1101    -> maker mkNullTag          >>= \v -> Just $ TriggerUn v offset count
    1102    -> maker mkNullTag          >>= \v -> Just $ TriggerPostUn v offset count
    1103    -> maker mkNullTag          >>= \v -> Just $ AutoReq v offset count
    1104    -> maker mkNullTag          >>= \v -> Just $ AutoProv v offset count
    1105    -> maker mkWord32Tag        >>= \v -> Just $ Capability v offset count
    1106    -> maker mkWord32Tag        >>= \v -> Just $ SourcePackage v offset count
    1107    -> maker mkNullTag          >>= \v -> Just $ OldOrigFileNames v offset count
    1108    -> maker mkNullTag          >>= \v -> Just $ BuildPreReq v offset count
    1109    -> maker mkNullTag          >>= \v -> Just $ BuildRequires v offset count
    1110    -> maker mkNullTag          >>= \v -> Just $ BuildConflicts v offset count
    1111    -> maker mkNullTag          >>= \v -> Just $ BuildMacros v offset count
    1112    -> maker mkWord32Tag        >>= \v -> Just $ ProvideFlags v offset count
    1113    -> maker mkStringArrayTag   >>= \v -> Just $ ProvideVersion v offset count
    1114    -> maker mkWord32Tag        >>= \v -> Just $ ObsoleteFlags v offset count
    1115    -> maker mkStringArrayTag   >>= \v -> Just $ ObsoleteVersion v offset count
    1116    -> maker mkWord32Tag        >>= \v -> Just $ DirIndexes v offset count
    1117    -> maker mkStringArrayTag   >>= \v -> Just $ BaseNames v offset count
    1118    -> maker mkStringArrayTag   >>= \v -> Just $ DirNames v offset count
    1119    -> maker mkWord32Tag        >>= \v -> Just $ OrigDirIndexes v offset count
    1120    -> maker mkStringArrayTag   >>= \v -> Just $ OrigBaseNames v offset count
    1121    -> maker mkStringArrayTag   >>= \v -> Just $ OrigDirNames v offset count
    1122    -> maker mkStringTag        >>= \v -> Just $ OptFlags v offset count
    1123    -> maker mkStringTag        >>= \v -> Just $ DistURL v offset count
    1124    -> maker mkStringTag        >>= \v -> Just $ PayloadFormat v offset count
    1125    -> maker mkStringTag        >>= \v -> Just $ PayloadCompressor v offset count
    1126    -> maker mkStringTag        >>= \v -> Just $ PayloadFlags v offset count
    1127    -> maker mkWord32Tag        >>= \v -> Just $ InstallColor v offset count
    1128    -> maker mkWord32Tag        >>= \v -> Just $ InstallTID v offset count
    1129    -> maker mkWord32Tag        >>= \v -> Just $ RemoveTID v offset count
    1130    -> maker mkNullTag          >>= \v -> Just $ SHA1RHN v offset count
    1131    -> maker mkStringTag        >>= \v -> Just $ RHNPlatform v offset count
    1132    -> maker mkStringTag        >>= \v -> Just $ Platform v offset count
    1133    -> maker mkStringArrayTag   >>= \v -> Just $ PatchesName v offset count
    1134    -> maker mkWord32Tag        >>= \v -> Just $ PatchesFlags v offset count
    1135    -> maker mkStringArrayTag   >>= \v -> Just $ PatchesVersion v offset count
    1136    -> maker mkWord32Tag        >>= \v -> Just $ CacheCTime v offset count
    1137    -> maker mkStringTag        >>= \v -> Just $ CachePkgPath v offset count
    1138    -> maker mkWord32Tag        >>= \v -> Just $ CachePkgSize v offset count
    1139    -> maker mkWord32Tag        >>= \v -> Just $ CachePkgMTime v offset count
    1140    -> maker mkWord32Tag        >>= \v -> Just $ FileColors v offset count
    1141    -> maker mkWord32Tag        >>= \v -> Just $ FileClass v offset count
    1142    -> maker mkStringArrayTag   >>= \v -> Just $ ClassDict v offset count
    1143    -> maker mkWord32Tag        >>= \v -> Just $ FileDependsX v offset count
    1144    -> maker mkWord32Tag        >>= \v -> Just $ FileDependsN v offset count
    1145    -> maker mkWord32Tag        >>= \v -> Just $ DependsDict v offset count
    1146    -> maker mkBinaryTag        >>= \v -> Just $ SourcePkgID v offset count
    1147    -> maker mkStringArrayTag   >>= \v -> Just $ FileContexts v offset count
    1148    -> maker mkStringArrayTag   >>= \v -> Just $ FSContexts v offset count
    1149    -> maker mkStringArrayTag   >>= \v -> Just $ ReContexts v offset count
    1150    -> maker mkStringArrayTag   >>= \v -> Just $ Policies v offset count
    1151    -> maker mkStringTag        >>= \v -> Just $ PreTrans v offset count
    1152    -> maker mkStringTag        >>= \v -> Just $ PostTrans v offset count
    1153    -> maker mkStringArrayTag   >>= \v -> Just $ PreTransProg v offset count
    1154    -> maker mkStringArrayTag   >>= \v -> Just $ PostTransProg v offset count
    1155    -> maker mkStringTag        >>= \v -> Just $ DistTag v offset count
    1156    -> maker mkStringArrayTag   >>= \v -> Just $ OldSuggestsName v offset count
    1157    -> maker mkStringArrayTag   >>= \v -> Just $ OldSuggestsVersion v offset count
    1158    -> maker mkWord32Tag        >>= \v -> Just $ OldSuggestsFlags v offset count
    1159    -> maker mkStringArrayTag   >>= \v -> Just $ OldEnhancesName v offset count
    1160    -> maker mkStringArrayTag   >>= \v -> Just $ OldEnhancesVersion v offset count
    1161    -> maker mkWord32Tag        >>= \v -> Just $ OldEnhancesFlags v offset count
    1162    -> maker mkWord32Tag        >>= \v -> Just $ Priority v offset count
    1163    -> maker mkStringTag        >>= \v -> Just $ CVSID v offset count
    1164    -> maker mkStringArrayTag   >>= \v -> Just $ BLinkPkgID v offset count
    1165    -> maker mkStringArrayTag   >>= \v -> Just $ BLinkHdrID v offset count
    1166    -> maker mkStringArrayTag   >>= \v -> Just $ BLinkNEVRA v offset count
    1167    -> maker mkStringArrayTag   >>= \v -> Just $ FLinkPkgID v offset count
    1168    -> maker mkStringArrayTag   >>= \v -> Just $ FLinkHdrID v offset count
    1169    -> maker mkStringArrayTag   >>= \v -> Just $ FLinkNEVRA v offset count
    1170    -> maker mkStringTag        >>= \v -> Just $ PackageOrigin v offset count
    1171    -> maker mkNullTag          >>= \v -> Just $ TriggerPreIn v offset count
    1172    -> maker mkNullTag          >>= \v -> Just $ BuildSuggests v offset count
    1173    -> maker mkNullTag          >>= \v -> Just $ BuildEnhances v offset count
    1174    -> maker mkWord32Tag        >>= \v -> Just $ ScriptStates v offset count
    1175    -> maker mkWord32Tag        >>= \v -> Just $ ScriptMetrics v offset count
    1176    -> maker mkWord32Tag        >>= \v -> Just $ BuildCPUClock v offset count
    1177    -> maker mkWord32Tag        >>= \v -> Just $ FileDigestAlgos v offset count
    1178    -> maker mkStringArrayTag   >>= \v -> Just $ Variants v offset count
    1179    -> maker mkWord32Tag        >>= \v -> Just $ XMajor v offset count
    1180    -> maker mkWord32Tag        >>= \v -> Just $ XMinor v offset count
    1181    -> maker mkStringTag        >>= \v -> Just $ RepoTag v offset count
    1182    -> maker mkStringArrayTag   >>= \v -> Just $ Keywords v offset count
    1183    -> maker mkStringArrayTag   >>= \v -> Just $ BuildPlatforms v offset count
    1184    -> maker mkWord32Tag        >>= \v -> Just $ PackageColor v offset count
    1185    -> maker mkWord32Tag        >>= \v -> Just $ PackagePrefColor v offset count
    1186    -> maker mkStringArrayTag   >>= \v -> Just $ XattrsDict v offset count
    1187    -> maker mkWord32Tag        >>= \v -> Just $ FileXattrsx v offset count
    1188    -> maker mkStringArrayTag   >>= \v -> Just $ DepAttrsDict v offset count
    1189    -> maker mkWord32Tag        >>= \v -> Just $ ConflictAttrsx v offset count
    1190    -> maker mkWord32Tag        >>= \v -> Just $ ObsoleteAttrsx v offset count
    1191    -> maker mkWord32Tag        >>= \v -> Just $ ProvideAttrsx v offset count
    1192    -> maker mkWord32Tag        >>= \v -> Just $ RequireAttrsx v offset count
    1193    -> maker mkNullTag          >>= \v -> Just $ BuildProvides v offset count
    1194    -> maker mkNullTag          >>= \v -> Just $ BuildObsoletes v offset count
    1195    -> maker mkWord32Tag        >>= \v -> Just $ DBInstance v offset count
    1196    -> maker mkStringTag        >>= \v -> Just $ NVRA v offset count

    5000    -> maker mkStringArrayTag   >>= \v -> Just $ FileNames v offset count
    5001    -> maker mkStringArrayTag   >>= \v -> Just $ FileProvide v offset count
    5002    -> maker mkStringArrayTag   >>= \v -> Just $ FileRequire v offset count
    5003    -> maker mkStringArrayTag   >>= \v -> Just $ FSNames v offset count
    5004    -> maker mkWord64Tag        >>= \v -> Just $ FSSizes v offset count
    5005    -> maker mkStringArrayTag   >>= \v -> Just $ TriggerConds v offset count
    5006    -> maker mkStringArrayTag   >>= \v -> Just $ TriggerType v offset count
    5007    -> maker mkStringArrayTag   >>= \v -> Just $ OrigFileNames v offset count
    5008    -> maker mkWord64Tag        >>= \v -> Just $ LongFileSizes v offset count
    5009    -> maker mkWord64Tag        >>= \v -> Just $ LongSize v offset count
    5010    -> maker mkStringArrayTag   >>= \v -> Just $ FileCaps v offset count
    5011    -> maker mkWord32Tag        >>= \v -> Just $ FileDigestAlgo v offset count
    5012    -> maker mkStringTag        >>= \v -> Just $ BugURL v offset count
    5013    -> maker mkStringTag        >>= \v -> Just $ EVR v offset count
    5014    -> maker mkStringTag        >>= \v -> Just $ NVR v offset count
    5015    -> maker mkStringTag        >>= \v -> Just $ NEVR v offset count
    5016    -> maker mkStringTag        >>= \v -> Just $ NEVRA v offset count
    5017    -> maker mkWord32Tag        >>= \v -> Just $ HeaderColor v offset count
    5018    -> maker mkWord32Tag        >>= \v -> Just $ Verbose v offset count
    5019    -> maker mkWord32Tag        >>= \v -> Just $ EpochNum v offset count
    5020    -> maker mkWord32Tag        >>= \v -> Just $ PreInFlags v offset count
    5021    -> maker mkWord32Tag        >>= \v -> Just $ PostInFlags v offset count
    5022    -> maker mkWord32Tag        >>= \v -> Just $ PreUnFlags v offset count
    5023    -> maker mkWord32Tag        >>= \v -> Just $ PostUnFlags v offset count
    5024    -> maker mkWord32Tag        >>= \v -> Just $ PreTransFlags v offset count
    5025    -> maker mkWord32Tag        >>= \v -> Just $ PostTransFlags v offset count
    5026    -> maker mkWord32Tag        >>= \v -> Just $ VerifyScriptFlags v offset count
    5027    -> maker mkWord32Tag        >>= \v -> Just $ TriggerScriptFlags v offset count
    5029    -> maker mkStringArrayTag   >>= \v -> Just $ Collections v offset count
    5030    -> maker mkStringArrayTag   >>= \v -> Just $ PolicyNames v offset count
    5031    -> maker mkStringArrayTag   >>= \v -> Just $ PolicyTypes v offset count
    5032    -> maker mkWord32Tag        >>= \v -> Just $ PolicyTypesIndexes v offset count
    5033    -> maker mkWord32Tag        >>= \v -> Just $ PolicyFlags v offset count
    5034    -> maker mkStringTag        >>= \v -> Just $ PolicyVCS v offset count
    5035    -> maker mkStringArrayTag   >>= \v -> Just $ OrderName v offset count
    5036    -> maker mkStringArrayTag   >>= \v -> Just $ OrderVersion v offset count
    5037    -> maker mkWord32Tag        >>= \v -> Just $ OrderFlags v offset count
    5038    -> maker mkStringArrayTag   >>= \v -> Just $ MSSFManifest v offset count
    5039    -> maker mkStringArrayTag   >>= \v -> Just $ MSSFDomain v offset count
    5040    -> maker mkStringArrayTag   >>= \v -> Just $ InstFileNames v offset count
    5041    -> maker mkStringArrayTag   >>= \v -> Just $ RequireNEVRs v offset count
    5042    -> maker mkStringArrayTag   >>= \v -> Just $ ProvideNEVRs v offset count
    5043    -> maker mkStringArrayTag   >>= \v -> Just $ ObsoleteNEVRs v offset count
    5044    -> maker mkStringArrayTag   >>= \v -> Just $ ConflictNEVRs v offset count
    5045    -> maker mkWord32Tag        >>= \v -> Just $ FileNLinks v offset count
    5046    -> maker mkStringArrayTag   >>= \v -> Just $ RecommendName v offset count
    5047    -> maker mkStringArrayTag   >>= \v -> Just $ RecommendVersion v offset count
    5048    -> maker mkWord32Tag        >>= \v -> Just $ RecommendFlags v offset count
    5049    -> maker mkStringArrayTag   >>= \v -> Just $ SuggestName v offset count
    5050    -> maker mkStringArrayTag   >>= \v -> Just $ SuggestVersion v offset count
    5051    -> maker mkWord32Tag        >>= \v -> Just $ SuggestFlags v offset count
    5052    -> maker mkStringArrayTag   >>= \v -> Just $ SupplementName v offset count
    5053    -> maker mkStringArrayTag   >>= \v -> Just $ SupplementVersion v offset count
    5054    -> maker mkWord32Tag        >>= \v -> Just $ SupplementFlags v offset count
    5055    -> maker mkStringArrayTag   >>= \v -> Just $ EnhanceName v offset count
    5056    -> maker mkStringArrayTag   >>= \v -> Just $ EnhanceVersion v offset count
    5057    -> maker mkWord32Tag        >>= \v -> Just $ EnhanceFlags v offset count
    5058    -> maker mkStringArrayTag   >>= \v -> Just $ RecommendNEVRs v offset count
    5059    -> maker mkStringArrayTag   >>= \v -> Just $ SuggestNEVRs v offset count
    5060    -> maker mkStringArrayTag   >>= \v -> Just $ SupplementNEVRs v offset count
    5061    -> maker mkStringArrayTag   >>= \v -> Just $ EnhanceNEVRs v offset count
    5062    -> maker mkStringTag        >>= \v -> Just $ Encoding v offset count
    5063    -> maker mkNullTag          >>= \v -> Just $ FileTriggerIn v offset count
    5064    -> maker mkNullTag          >>= \v -> Just $ FileTriggerUn v offset count
    5065    -> maker mkNullTag          >>= \v -> Just $ FileTriggerPostUn v offset count
    5066    -> maker mkStringArrayTag   >>= \v -> Just $ FileTriggerScripts v offset count
    5067    -> maker mkStringArrayTag   >>= \v -> Just $ FileTriggerScriptProg v offset count
    5068    -> maker mkWord32Tag        >>= \v -> Just $ FileTriggerScriptFlags v offset count
    5069    -> maker mkStringArrayTag   >>= \v -> Just $ FileTriggerName v offset count
    5070    -> maker mkWord32Tag        >>= \v -> Just $ FileTriggerIndex v offset count
    5071    -> maker mkStringArrayTag   >>= \v -> Just $ FileTriggerVersion v offset count
    5072    -> maker mkWord32Tag        >>= \v -> Just $ FileTriggerFlags v offset count
    5073    -> maker mkNullTag          >>= \v -> Just $ TransFileTriggerIn v offset count
    5074    -> maker mkNullTag          >>= \v -> Just $ TransFileTriggerUn v offset count
    5075    -> maker mkNullTag          >>= \v -> Just $ TransFileTriggerPostUn v offset count
    5076    -> maker mkStringArrayTag   >>= \v -> Just $ TransFileTriggerScripts v offset count
    5077    -> maker mkStringArrayTag   >>= \v -> Just $ TransFileTriggerScriptProg v offset count
    5078    -> maker mkWord32Tag        >>= \v -> Just $ TransFileTriggerScriptFlags v offset count
    5079    -> maker mkStringArrayTag   >>= \v -> Just $ TransFileTriggerName v offset count
    5080    -> maker mkWord32Tag        >>= \v -> Just $ TransFileTriggerIndex v offset count
    5081    -> maker mkStringArrayTag   >>= \v -> Just $ TransFileTriggerVersion v offset count
    5082    -> maker mkWord32Tag        >>= \v -> Just $ TransFileTriggerFlags v offset count
    5083    -> maker mkStringTag        >>= \v -> Just $ RemovePathPostFixes v offset count
    5084    -> maker mkWord32Tag        >>= \v -> Just $ FileTriggerPriorities v offset count
    5085    -> maker mkWord32Tag        >>= \v -> Just $ TransFileTriggerPriorities v offset count
    5086    -> maker mkStringArrayTag   >>= \v -> Just $ FileTriggerConds v offset count
    5087    -> maker mkStringArrayTag   >>= \v -> Just $ FileTriggerType v offset count
    5088    -> maker mkStringArrayTag   >>= \v -> Just $ TransFileTriggerConds v offset count
    5089    -> maker mkStringArrayTag   >>= \v -> Just $ TransFileTriggerType v offset count
    5090    -> maker mkStringArrayTag   >>= \v -> Just $ FileSignatures v offset count
    5091    -> maker mkWord32Tag        >>= \v -> Just $ FileSignatureLength v offset count

    _       -> Nothing
 where
    maker fn = fn store ty offset count

mkNullTag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe NullTag
mkNullTag _ ty _ _ | ty == 0    = Just NullTag
                   | otherwise  = Nothing

mkCharTag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe CharTag
mkCharTag store ty offset _ | ty == 1   = Just $ CharTag $ C.head $ BS.take 1 start
                            | otherwise = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkWord8Tag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Word8Tag
mkWord8Tag store ty offset count | ty == 2      = Just $ Word8Tag $ readWords 1 asWord8 start count
                                 | otherwise    = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkWord16Tag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Word16Tag
mkWord16Tag store ty offset count | ty == 3     = Just $ Word16Tag $ readWords 2 asWord16 start count
                                  | otherwise   = Nothing
 where
    start  = BS.drop (fromIntegral offset) store

mkWord32Tag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Word32Tag
mkWord32Tag store ty offset count | ty == 4     = Just $ Word32Tag $ readWords 4 asWord32 start count
                                  | otherwise   = Nothing
 where
    start  = BS.drop (fromIntegral offset) store

mkWord64Tag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe Word64Tag
mkWord64Tag store ty offset count | ty == 5     = Just $ Word64Tag $ readWords 8 asWord64 start count
                                  | otherwise   = Nothing
 where
    start  = BS.drop (fromIntegral offset) store

mkStringTag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe StringTag
mkStringTag store ty offset _ | ty == 6   = Just $ StringTag $ C.unpack $ BS.takeWhile (/= 0) start
                              | otherwise = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkBinaryTag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe BinaryTag
mkBinaryTag store ty offset count | ty == 7     = Just $ BinaryTag $ BS.take count' start
                                  | otherwise   = Nothing
 where
    count' = fromIntegral count
    start  = BS.drop (fromIntegral offset) store

mkStringArrayTag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe StringArrayTag
mkStringArrayTag store ty offset count | ty == 8    = Just $ StringArrayTag $ map C.unpack $ readStrings start count
                                       | otherwise  = Nothing
 where
    start = BS.drop (fromIntegral offset) store

mkI18NStringTag :: BS.ByteString -> Word32 -> Word32 -> Word32 -> Maybe I18NStringTag
mkI18NStringTag store ty offset _ | ty == 9     = Just $ I18NStringTag $ BS.takeWhile (/= 0) start
                                  | otherwise   = Nothing
 where
    start  = BS.drop (fromIntegral offset) store

readWords :: Int -> (BS.ByteString -> a) -> BS.ByteString -> Word32 -> [a]
readWords size conv bytestring count = let
    doit _ _  _ 0   a = a
    doit s fn b cnt a = let
        (bytes, remainder) = BS.splitAt size b
     in
        doit s fn remainder (cnt - 1) (a ++ [fn bytes])
 in
    doit size conv bytestring count []

readStrings :: BS.ByteString -> Word32 -> [BS.ByteString]
readStrings bytestring count = let
    doit _ 0 a = a
    doit b c a = let
        (str, remainder) = BS.span (/= 0) b
     in
        -- The drop call is because span will leave the string parked on the leading null
        -- character, so all subsequent calls will see that first and return a "" for str.
        -- Just drop that leading character and move on.
        doit (BS.drop 1 remainder) (c - 1) (a ++ [str])
 in
    doit bytestring count []
