module RPM.TagsSpec (spec) where

import Test.Hspec
import Data.Foldable(forM_)
import RPM.Tags(mkTag, Tag(..), Null(..))
import qualified Data.ByteString as BS

{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "RPM.Tags" $
  describe "mkTag" $ do
    it "returns Nothing for unsupported `tag' value" $ do
      let store = BS.pack [0]
      let tag = mkTag store 9999 0 0 0
      tag `shouldBe` Nothing

    -- tests for tags which use mkNull
    forM_ [61, 62, 63, 64,
           256, 258, 260, 263, 264, 265,
           1038, 1041, 1042, 1083, 1084, 1093,
           1100, 1101, 1102, 1103, 1104, 1107, 1108, 1109, 1110, 1111,
           1130, 1171, 1172, 1173, 1193, 1194,
           5063, 5064, 5065, 5073, 5074, 5075] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 0") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 0 0 0
        tag == Just (HeaderImage Null) || -- 61
            tag == Just (HeaderSignatures Null) || -- 62
            tag == Just (HeaderImmutable Null) || -- 63
            tag == Just (HeaderRegions Null) || -- 64
            tag == Just (SigBase Null) || -- 256
            tag == Just (INTERNAL (OBSOLETE (SigLEMD5_1 Null))) || -- 258
            tag == Just (INTERNAL (OBSOLETE (SigLEMD5_2 Null))) || -- 260
            tag == Just (INTERNAL (OBSOLETE (SigPGP5 Null))) || -- 263
            tag == Just (INTERNAL (OBSOLETE (SigBadSHA1_1 Null))) || -- 264
            tag == Just (INTERNAL (OBSOLETE (SigBadSHA1_2 Null))) || -- 265
            tag == Just (INTERNAL (OBSOLETE (Root Null))) || -- 1038
            tag == Just (INTERNAL (OBSOLETE (Exclude Null))) || -- 1041
            tag == Just (INTERNAL (OBSOLETE (Exclusive Null))) || -- 1042
            tag == Just (INTERNAL (OBSOLETE (BrokenMD5 Null))) || -- 1083
            tag == Just (INTERNAL (PreReq Null)) || -- 1084
            tag == Just (INTERNAL (DocDir Null)) || -- 1093
            tag == Just (INTERNAL (TriggerIn Null)) || -- 1100
            tag == Just (INTERNAL (TriggerUn Null)) || -- 1101
            tag == Just (INTERNAL (TriggerPostUn Null)) || -- 1102
            tag == Just (INTERNAL (AutoReq Null)) || -- 1103
            tag == Just (INTERNAL (AutoProv Null)) || -- 1104
            tag == Just (INTERNAL (OBSOLETE (OldOrigFileNames Null))) || -- 1107
            tag == Just (INTERNAL (BuildPreReq Null)) || -- 1108
            tag == Just (INTERNAL (BuildRequires Null)) || -- 1109
            tag == Just (INTERNAL (BuildConflicts Null)) || -- 1110
            tag == Just (INTERNAL (UNUSED (BuildMacros Null))) || -- 1111
            tag == Just (INTERNAL (OBSOLETE (SHA1RHN Null))) || -- 1130
            tag == Just (INTERNAL (TriggerPreIn Null)) || -- 1171
            tag == Just (INTERNAL (UNIMPLEMENTED (BuildSuggests Null))) || -- 1172
            tag == Just (INTERNAL (UNIMPLEMENTED (BuildEnhances Null))) || -- 1173
            tag == Just (UNIMPLEMENTED (BuildProvides Null)) || -- 1193
            tag == Just (UNIMPLEMENTED (BuildObsoletes Null)) || -- 1194
            tag == Just (INTERNAL (FileTriggerIn Null)) || -- 5063
            tag == Just (INTERNAL (FileTriggerUn Null)) || -- 5064
            tag == Just (INTERNAL (FileTriggerPostUn Null)) || -- 5065
            tag == Just (INTERNAL (TransFileTriggerIn Null)) || -- 5073
            tag == Just (INTERNAL (TransFileTriggerUn Null)) || -- 5074
            tag == Just (INTERNAL (TransFileTriggerPostUn Null)) -- 5075
            `shouldBe` True

    -- tests for tags which use mkWord16
    forM_ [1030, 1033] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 3") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BS.pack [0,5]
        let tag = mkTag store tagInt 3 0 1
        tag == Just (FileModes [5]) ||
            tag == Just (FileRDevs [5])
            `shouldBe` True

    -- tests for tags which use mkWord32
    forM_ [257,
            1003, 1006, 1008, 1009,
            1028, 1031, 1032, 1034, 1037,
            1045, 1046, 1048, 1051, 1052, 1053,
            1068, 1069, 1080, 1095, 1096,
            1105, 1106, 1112, 1114, 1116, 1119,
            1127, 1128, 1129, 1134, 1136, 1138, 1139,
            1140, 1141, 1143, 1144, 1145, 1158, 1161, 1162,
            1174, 1175, 1176, 1177, 1179,
            1180, 1184, 1185, 1187, 1189,
            1190, 1191, 1192, 1195,
            5011, 5017, 5018, 5019,
            5020, 5021, 5022, 5023, 5024, 5025, 5026, 5027,
            5032, 5033, 5037, 5045, 5048, 5051, 5054, 5057,
            5068, 5070, 5072, 5078, 5080, 5082, 5084, 5085,
            5091] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 4") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BS.pack [0,0,0,5]
        let tag = mkTag store tagInt 4 0 1

        tag == Just (SigSize 5) || -- 257
            tag == Just (Epoch 5) || -- 1003
            tag == Just (BuildTime 5) || -- 1006
            tag == Just (InstallTime 5) || -- 1008
            tag == Just (Size 5) || -- 1009
            tag == Just (FileSizes [5]) || -- 1028
            tag == Just (INTERNAL (OBSOLETE (FileUIDs [5]))) || -- 1031
            tag == Just (INTERNAL (OBSOLETE (FileGIDs [5]))) || -- 1032
            tag == Just (FileMTimes [5]) || -- 1034
            tag == Just (FileFlags [5]) || -- 1037
            tag == Just (FileVerifyFlags [5]) || -- 1045
            tag == Just (ArchiveSize 5) || -- 1046
            tag == Just (RequireFlags [5]) || -- 1048
            tag == Just (NoSource [5]) || -- 1051
            tag == Just (NoPatch [5]) || -- 1052
            tag == Just (ConflictFlags [5]) || -- 1053
            tag == Just (TriggerFlags [5]) || -- 1068
            tag == Just (TriggerIndex [5]) || -- 1069
            tag == Just (ChangeLogTime [5]) || -- 1080
            tag == Just (FileDevices [5]) || -- 1095
            tag == Just (FileINodes [5]) || -- 1096
            tag == Just (INTERNAL (OBSOLETE (Capability 5))) || -- 1105
            tag == Just (SourcePackage 5) || -- 1106
            tag == Just (ProvideFlags [5]) || -- 1112
            tag == Just (ObsoleteFlags [5]) || -- 1114
            tag == Just (DirIndexes [5]) || -- 1116
            tag == Just (OrigDirIndexes [5]) || -- 1119
            tag == Just (InstallColor 5) || -- 1127
            tag == Just (InstallTID 5) || -- 1128
            tag == Just (RemoveTID 5) || -- 1129
            tag == Just (DEPRECATED (PatchesFlags [5])) || -- 1134
            tag == Just (INTERNAL (OBSOLETE (CacheCTime 5))) || -- 1136
            tag == Just (INTERNAL (OBSOLETE (CachePkgSize 5))) || -- 1138
            tag == Just (INTERNAL (OBSOLETE (CachePkgMTime 5))) || -- 1139
            tag == Just (FileColors [5]) || -- 1140
            tag == Just (FileClass [5]) || -- 1141
            tag == Just (FileDependsX [5]) || -- 1143
            tag == Just (FileDependsN [5]) || -- 1144
            -- note: for tagInt == 1145 the return value of mkTag uses bitwise operations
            -- here we use precalculated values
            tag == Just (DependsDict [(0, 5)]) || -- 1145
            tag == Just (OBSOLETE (OldSuggestsFlags [5])) || -- 1158
            tag == Just (OBSOLETE (OldEnhancesFlags [5])) || -- 1161
            tag == Just (UNIMPLEMENTED (Priority [5])) || -- 1162
            tag == Just (UNIMPLEMENTED (ScriptStates [5])) || -- 1174
            tag == Just (UNIMPLEMENTED (ScriptMetrics [5])) || -- 1175
            tag == Just (UNIMPLEMENTED (BuildCPUClock 5)) || -- 1176
            tag == Just (UNIMPLEMENTED (FileDigestAlgos [5])) || -- 1177
            tag == Just (UNIMPLEMENTED (XMajor 5)) || -- 1179
            tag == Just (UNIMPLEMENTED (XMinor 5)) || -- 1180
            tag == Just (UNIMPLEMENTED (PackageColor 5)) || -- 1184
            tag == Just (UNIMPLEMENTED (PackagePrefColor 5)) || -- 1185
            tag == Just (UNIMPLEMENTED (FileXattrsx [5])) || -- 1187
            tag == Just (UNIMPLEMENTED (ConflictAttrsx [5])) || -- 1189
            tag == Just (UNIMPLEMENTED (ObsoleteAttrsx [5])) || -- 1190
            tag == Just (UNIMPLEMENTED (ProvideAttrsx [5])) || -- 1191
            tag == Just (UNIMPLEMENTED (RequireAttrsx [5])) || -- 1192
            tag == Just (DBInstance 5) || -- 1195
            tag == Just (FileDigestAlgo 5) || -- 5011
            tag == Just (HeaderColor 5) || -- 5017
            tag == Just (Verbose 5) || -- 5018
            tag == Just (EpochNum 5) || -- 5019
            tag == Just (PreInFlags 5) || -- 5020
            tag == Just (PostInFlags 5) || -- 5021
            tag == Just (PreUnFlags 5) || -- 5022
            tag == Just (PostUnFlags 5) || -- 5023
            tag == Just (PreTransFlags 5) || -- 5024
            tag == Just (PostTransFlags 5) || -- 5025
            tag == Just (VerifyScriptFlags 5) || -- 5026
            tag == Just (TriggerScriptFlags [5]) || -- 5027
            tag == Just (PolicyTypesIndexes [5]) || -- 5032
            tag == Just (PolicyFlags [5]) || -- 5033
            tag == Just (OrderFlags [5]) || -- 5037
            tag == Just (FileNLinks [5]) || -- 5045
            tag == Just (RecommendFlags [5]) || -- 5048
            tag == Just (SuggestFlags [5]) || -- 5051
            tag == Just (SupplementFlags [5]) || -- 5054
            tag == Just (EnhanceFlags [5]) || -- 5057
            tag == Just (FileTriggerScriptFlags [5]) || -- 5068
            tag == Just (FileTriggerIndex [5]) || -- 5070
            tag == Just (FileTriggerFlags [5]) || -- 5072
            tag == Just (TransFileTriggerScriptFlags [5]) || -- 5078
            tag == Just (TransFileTriggerIndex [5]) || -- 5080
            tag == Just (TransFileTriggerFlags [5]) || -- 5082
            tag == Just (FileTriggerPriorities [5]) || -- 5084
            tag == Just (TransFileTriggerPriorities [5]) || -- 5085
            tag == Just (FileSignatureLength 5) -- 5091
            `shouldBe` True

    -- tests for tags which use mkWord64
    forM_ [270, 271, 5004, 5008, 5009] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 5") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BS.pack [0,0,0,0,0,0,0,5]
        let tag = mkTag store tagInt 5 0 1

        tag == Just (LongSigSize 5) || -- 270
            tag == Just (LongArchiveSize 5) || -- 271
            tag == Just (UNIMPLEMENTED (FSSizes [5])) || -- 5004
            tag == Just (LongFileSizes [5]) || -- 5008
            tag == Just (LongSize 5) -- 5009
            `shouldBe` True
