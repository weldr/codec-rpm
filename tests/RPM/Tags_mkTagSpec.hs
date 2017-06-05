module RPM.Tags_mkTagSpec (spec) where

import Test.Hspec
import Data.Foldable(forM_)
import RPM.Tags(mkTag, Tag(..), Null(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "RPM.Tags.mkTag" $ do
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

    -- tests for tags which use mkChar
    forM_ [1029] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 1") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "5"
        let tag = mkTag store tagInt 1 0 1

        tag == Just (FileStates "5") -- 1029
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

    -- tests for tags which use mkString
    forM_ [269,
            1000, 1001, 1002, 1007, 1010, 1011, 1014, 1015,
            1020, 1021, 1022, 1023, 1024, 1025, 1026, 1044,
            1056, 1057, 1058, 1063, 1064, 1079, 1094,
            1122, 1123, 1124, 1125, 1126, 1131, 1132, 1137,
            1151, 1152, 1155, 1163, 1170, 1181, 1196,
            5012, 5013, 5014, 5015, 5016, 5034, 5062, 5083] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 6") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 6 0 7

        tag == Just (SHA1Header "test-me") || -- 269
            tag == Just (Name "test-me") || -- 1000
            tag == Just (Version "test-me") || -- 1001
            tag == Just (Release "test-me") || -- 1002
            tag == Just (BuildHost "test-me") || -- 1007
            tag == Just (Distribution "test-me") || -- 1010
            tag == Just (Vendor "test-me") || -- 1011
            tag == Just (License "test-me") || -- 1014
            tag == Just (Packager "test-me") || -- 1015
            tag == Just (URL "test-me") || -- 1020
            tag == Just (OS "test-me") || -- 1021
            tag == Just (Arch "test-me") || -- 1022
            tag == Just (PreIn "test-me") || -- 1023
            tag == Just (PostIn "test-me") || -- 1024
            tag == Just (PreUn "test-me") || -- 1025
            tag == Just (PostUn "test-me") || -- 1026
            tag == Just (SourceRPM "test-me") || -- 1044
            tag == Just (INTERNAL (DEPRECATED (DefaultPrefix "test-me"))) || -- 1056
            tag == Just (INTERNAL (OBSOLETE (BuildRoot "test-me"))) || -- 1057
            tag == Just (INTERNAL (DEPRECATED (InstallPrefix "test-me"))) || -- 1058
            tag == Just (INTERNAL (AutoReqProv "test-me")) || -- 1063
            tag == Just (RPMVersion "test-me") || -- 1064
            tag == Just (VerifyScript "test-me") || -- 1079
            tag == Just (Cookie "test-me") || -- 1094
            tag == Just (OptFlags "test-me") || -- 1122
            tag == Just (DistURL "test-me") || -- 1123
            tag == Just (PayloadFormat "test-me") || -- 1124
            tag == Just (PayloadCompressor "test-me") || -- 1125
            tag == Just (PayloadFlags "test-me") || -- 1126
            tag == Just (INTERNAL (OBSOLETE (RHNPlatform "test-me"))) || -- 1131
            tag == Just (Platform "test-me") || -- 1132
            tag == Just (INTERNAL (OBSOLETE (CachePkgPath "test-me"))) || -- 1137
            tag == Just (PreTrans "test-me") || -- 1151
            tag == Just (PostTrans "test-me") || -- 1152
            tag == Just (DistTag "test-me") || -- 1155
            tag == Just (UNIMPLEMENTED (CVSID "test-me")) || -- 1163
            tag == Just (UNIMPLEMENTED (PackageOrigin "test-me")) || -- 1170
            tag == Just (UNIMPLEMENTED (RepoTag "test-me")) || -- 1181
            tag == Just (NVRA "test-me") || -- 1196
            tag == Just (BugURL "test-me") || -- 5012
            tag == Just (EVR "test-me") || -- 5013
            tag == Just (NVR "test-me") || -- 5014
            tag == Just (NEVR "test-me") || -- 5015
            tag == Just (NEVRA "test-me") || -- 5016
            tag == Just (PolicyVCS "test-me") || -- 5034
            tag == Just (Encoding "test-me") || -- 5062
            tag == Just (INTERNAL (RemovePathPostFixes "test-me")) -- 5083
            `shouldBe` True

    -- tests for tags which use mkBinary
    forM_ [259, 261, 262, 267, 268,
            1012, 1013, 1043, 1146] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 7") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 7 0 7

        tag == Just (SigPGP store) || -- 259
            tag == Just (SigMD5 store) || -- 261
            tag == Just (SigGPG store) || -- 262
            tag == Just (DSAHeader store) || -- 267
            tag == Just (RSAHeader store) || -- 268
            tag == Just (GIF store) || -- 1012
            tag == Just (XPM store) || -- 1013
            tag == Just (Icon store) || -- 1043
            tag == Just (SourcePkgID store) -- 1146
            `shouldBe` True

    -- tests for tags which use mkStringArray
    forM_ [100, 266,
            1017, 1018, 1019, 1027, 1035, 1036, 1039, 1040,
            1047, 1049, 1050, 1054, 1055, 1059, 1060, 1061,
            1062, 1065, 1066, 1067, 1081, 1082, 1085, 1086,
            1087, 1088, 1089, 1090, 1091, 1092, 1097, 1098,
            1099, 1113, 1115, 1117, 1118, 1120, 1121, 1133,
            1135, 1142, 1147, 1148, 1149, 1150, 1153, 1154, 1156,
            1157, 1159, 1160, 1164, 1165, 1166, 1167, 1167,
            1168, 1169, 1178, 1182, 1183, 1186, 1188,
            5000, 5001, 5002, 5003, 5005, 5006, 5007, 5010,
            5029, 5030, 5031, 5035, 5036, 5038, 5039, 5040,
            5041, 5042, 5043, 5044, 5046, 5047, 5049, 5050,
            5052, 5053, 5055, 5056, 5058, 5059, 5060, 5061,
            5066, 5067, 5069, 5071, 5076, 5077, 5079, 5081,
             5086, 5087, 5088, 5089, 5090] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 8") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 8 0 7

        tag == Just (HeaderI18NTable ["test-me"]) || -- 100
            tag == Just (PubKeys ["test-me"]) || -- 266
            tag == Just (INTERNAL (ChangeLog ["test-me"])) || -- 1017
            tag == Just (Source ["test-me"]) || -- 1018
            tag == Just (Patch ["test-me"]) || -- 1019
            tag == Just (OBSOLETE (OldFileNames ["test-me"])) || -- 1027
            tag == Just (FileMD5s ["test-me"]) || -- 1035
            tag == Just (FileLinkTos ["test-me"]) || -- 1036
            tag == Just (FileUserName ["test-me"]) || -- 1039
            tag == Just (FileGroupName ["test-me"]) || -- 1040
            tag == Just (ProvideName ["test-me"]) || -- 1047
            tag == Just (RequireName ["test-me"]) || -- 1049
            tag == Just (RequireVersion ["test-me"]) || -- 1050
            tag == Just (ConflictName ["test-me"]) || -- 1054
            tag == Just (ConflictVersion ["test-me"]) || -- 1055
            tag == Just (ExcludeArch ["test-me"]) || -- 1059
            tag == Just (ExcludeOS ["test-me"]) || -- 1060
            tag == Just (ExclusiveArch ["test-me"]) || -- 1061
            tag == Just (ExclusiveOS ["test-me"]) || -- 1062
            tag == Just (TriggerScripts ["test-me"]) || -- 1065
            tag == Just (TriggerName ["test-me"]) || -- 1066
            tag == Just (TriggerVersion ["test-me"]) || -- 1067
            tag == Just (ChangeLogName ["test-me"]) || -- 1081
            tag == Just (ChangeLogText ["test-me"]) || -- 1082
            tag == Just (PreInProg ["test-me"]) || -- 1085
            tag == Just (PostInProg ["test-me"]) || -- 1086
            tag == Just (PreUnProg ["test-me"]) || -- 1087
            tag == Just (PostUnProg ["test-me"]) || -- 1088
            tag == Just (BuildArchs ["test-me"]) || -- 1089
            tag == Just (ObsoleteName ["test-me"]) || -- 1090
            tag == Just (VerifyScriptProg ["test-me"]) || -- 1091
            tag == Just (TriggerScriptProg ["test-me"]) || -- 1092
            tag == Just (FileLangs ["test-me"]) || -- 1097
            tag == Just (Prefixes ["test-me"]) || -- 1098
            tag == Just (InstPrefixes ["test-me"]) || -- 1099
            tag == Just (ProvideVersion ["test-me"]) || -- 1113
            tag == Just (ObsoleteVersion ["test-me"]) || -- 1115
            tag == Just (BaseNames ["test-me"]) || -- 1117
            tag == Just (DirNames ["test-me"]) || -- 1118
            tag == Just (OrigBaseNames ["test-me"]) || -- 1120
            tag == Just (OrigDirNames ["test-me"]) || -- 1121
            tag == Just (DEPRECATED (PatchesName ["test-me"])) || -- 1133
            tag == Just (DEPRECATED (PatchesVersion ["test-me"])) || -- 1135
            tag == Just (ClassDict ["test-me"]) || -- 1142
            tag == Just (OBSOLETE (FileContexts ["test-me"])) || -- 1147
            tag == Just (FSContexts ["test-me"]) || -- 1148
            tag == Just (ReContexts ["test-me"]) || -- 1149
            tag == Just (Policies ["test-me"]) || -- 1150
            tag == Just (PreTransProg ["test-me"]) || -- 1153
            tag == Just (PostTransProg ["test-me"]) || -- 1154
            tag == Just (OBSOLETE (OldSuggestsName ["test-me"])) || -- 1156
            tag == Just (OBSOLETE (OldSuggestsVersion ["test-me"])) || -- 1157
            tag == Just (OBSOLETE (OldEnhancesName ["test-me"])) || -- 1159
            tag == Just (OBSOLETE (OldEnhancesVersion ["test-me"])) || -- 1160
            tag == Just (UNIMPLEMENTED (BLinkPkgID ["test-me"])) || -- 1164
            tag == Just (UNIMPLEMENTED (BLinkHdrID ["test-me"])) || -- 1165
            tag == Just (UNIMPLEMENTED (BLinkNEVRA ["test-me"])) || -- 1166
            tag == Just (UNIMPLEMENTED (FLinkPkgID ["test-me"])) || -- 1167
            tag == Just (UNIMPLEMENTED (FLinkHdrID ["test-me"])) || -- 1168
            tag == Just (UNIMPLEMENTED (FLinkNEVRA ["test-me"])) || -- 1169
            tag == Just (UNIMPLEMENTED (Variants["test-me"])) || -- 1178
            tag == Just (UNIMPLEMENTED (Keywords["test-me"])) || -- 1182
            tag == Just (UNIMPLEMENTED (BuildPlatforms["test-me"])) || -- 1183
            tag == Just (UNIMPLEMENTED (XattrsDict["test-me"])) || -- 1186
            tag == Just (UNIMPLEMENTED (DepAttrsDict["test-me"])) || -- 1188
            tag == Just (FileNames ["test-me"]) || -- 5000
            tag == Just (FileProvide ["test-me"]) || -- 5001
            tag == Just (FileRequire ["test-me"]) || -- 5002
            tag == Just (UNIMPLEMENTED (FSNames ["test-me"])) || -- 5003
            tag == Just (TriggerConds ["test-me"]) || -- 5005
            tag == Just (TriggerType ["test-me"]) || -- 5006
            tag == Just (OrigFileNames ["test-me"]) || -- 5007
            tag == Just (FileCaps ["test-me"]) || -- 5010
            tag == Just (UNIMPLEMENTED (Collections ["test-me"])) || -- 5029
            tag == Just (PolicyNames ["test-me"]) || -- 5030
            tag == Just (PolicyTypes ["test-me"]) || -- 5031
            tag == Just (OrderName ["test-me"]) || -- 5035
            tag == Just (OrderVersion ["test-me"]) || -- 5036
            tag == Just (UNIMPLEMENTED (MSSFManifest ["test-me"])) || -- 5038
            tag == Just (UNIMPLEMENTED (MSSFDomain ["test-me"])) || -- 5039
            tag == Just (InstFileNames ["test-me"]) || -- 5040
            tag == Just (RequireNEVRs ["test-me"]) || -- 5041
            tag == Just (ProvideNEVRs ["test-me"]) || -- 5042
            tag == Just (ObsoleteNEVRs ["test-me"]) || -- 5043
            tag == Just (ConflictNEVRs ["test-me"]) || -- 5044
            tag == Just (RecommendName ["test-me"]) || -- 5046
            tag == Just (RecommendVersion ["test-me"]) || -- 5047
            tag == Just (SuggestName ["test-me"]) || -- 5049
            tag == Just (SuggestVersion ["test-me"]) || -- 5050
            tag == Just (SupplementName ["test-me"]) || -- 5052
            tag == Just (SupplementVersion ["test-me"]) || -- 5053
            tag == Just (EnhanceName ["test-me"]) || -- 5055
            tag == Just (EnhanceVersion ["test-me"]) || -- 5056
            tag == Just (RecommendNEVRs ["test-me"]) || -- 5058
            tag == Just (SuggestNEVRs ["test-me"]) || -- 5059
            tag == Just (SupplementNEVRs ["test-me"]) || -- 5060
            tag == Just (EnhanceNEVRs ["test-me"]) || -- 5061
            tag == Just (FileTriggerScripts ["test-me"]) || -- 5066
            tag == Just (FileTriggerScriptProg ["test-me"]) || -- 5067
            tag == Just (FileTriggerName ["test-me"]) || -- 5069
            tag == Just (FileTriggerVersion ["test-me"]) || -- 5071
            tag == Just (TransFileTriggerScripts ["test-me"]) || -- 5076
            tag == Just (TransFileTriggerScriptProg ["test-me"]) || -- 5077
            tag == Just (TransFileTriggerName ["test-me"]) || -- 5079
            tag == Just (TransFileTriggerVersion ["test-me"]) || -- 5081
            tag == Just (FileTriggerConds ["test-me"]) || -- 5086
            tag == Just (FileTriggerType ["test-me"]) || -- 5087
            tag == Just (TransFileTriggerConds ["test-me"]) || -- 5088
            tag == Just (TransFileTriggerType ["test-me"]) || -- 5089
            tag == Just (FileSignatures ["test-me"]) -- 5090
            `shouldBe` True

    -- tests for tags which use mkI18NString
    forM_ [1004, 1005, 1016] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 9") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 9 0 7

        tag == Just (Summary store) || -- 1004
            tag == Just (Description store) || -- 1005
            tag == Just (Group store) -- 1016
            `shouldBe` True

    -- tests for tags which use mkString
    forM_ [269,
            1000, 1001, 1002, 1007, 1010, 1011, 1014, 1015,
            1020, 1021, 1022, 1023, 1024, 1025, 1026, 1044,
            1056, 1057, 1058, 1063, 1064, 1079, 1094,
            1122, 1123, 1124, 1125, 1126, 1131, 1132, 1137,
            1151, 1152, 1155, 1163, 1170, 1181, 1196,
            5012, 5013, 5014, 5015, 5016, 5034, 5062, 5083] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 6") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 6 0 7

        tag == Just (SHA1Header "test-me") || -- 269
            tag == Just (Name "test-me") || -- 1000
            tag == Just (Version "test-me") || -- 1001
            tag == Just (Release "test-me") || -- 1002
            tag == Just (BuildHost "test-me") || -- 1007
            tag == Just (Distribution "test-me") || -- 1010
            tag == Just (Vendor "test-me") || -- 1011
            tag == Just (License "test-me") || -- 1014
            tag == Just (Packager "test-me") || -- 1015
            tag == Just (URL "test-me") || -- 1020
            tag == Just (OS "test-me") || -- 1021
            tag == Just (Arch "test-me") || -- 1022
            tag == Just (PreIn "test-me") || -- 1023
            tag == Just (PostIn "test-me") || -- 1024
            tag == Just (PreUn "test-me") || -- 1025
            tag == Just (PostUn "test-me") || -- 1026
            tag == Just (SourceRPM "test-me") || -- 1044
            tag == Just (INTERNAL (DEPRECATED (DefaultPrefix "test-me"))) || -- 1056
            tag == Just (INTERNAL (OBSOLETE (BuildRoot "test-me"))) || -- 1057
            tag == Just (INTERNAL (DEPRECATED (InstallPrefix "test-me"))) || -- 1058
            tag == Just (INTERNAL (AutoReqProv "test-me")) || -- 1063
            tag == Just (RPMVersion "test-me") || -- 1064
            tag == Just (VerifyScript "test-me") || -- 1079
            tag == Just (Cookie "test-me") || -- 1094
            tag == Just (OptFlags "test-me") || -- 1122
            tag == Just (DistURL "test-me") || -- 1123
            tag == Just (PayloadFormat "test-me") || -- 1124
            tag == Just (PayloadCompressor "test-me") || -- 1125
            tag == Just (PayloadFlags "test-me") || -- 1126
            tag == Just (INTERNAL (OBSOLETE (RHNPlatform "test-me"))) || -- 1131
            tag == Just (Platform "test-me") || -- 1132
            tag == Just (INTERNAL (OBSOLETE (CachePkgPath "test-me"))) || -- 1137
            tag == Just (PreTrans "test-me") || -- 1151
            tag == Just (PostTrans "test-me") || -- 1152
            tag == Just (DistTag "test-me") || -- 1155
            tag == Just (UNIMPLEMENTED (CVSID "test-me")) || -- 1163
            tag == Just (UNIMPLEMENTED (PackageOrigin "test-me")) || -- 1170
            tag == Just (UNIMPLEMENTED (RepoTag "test-me")) || -- 1181
            tag == Just (NVRA "test-me") || -- 1196
            tag == Just (BugURL "test-me") || -- 5012
            tag == Just (EVR "test-me") || -- 5013
            tag == Just (NVR "test-me") || -- 5014
            tag == Just (NEVR "test-me") || -- 5015
            tag == Just (NEVRA "test-me") || -- 5016
            tag == Just (PolicyVCS "test-me") || -- 5034
            tag == Just (Encoding "test-me") || -- 5062
            tag == Just (INTERNAL (RemovePathPostFixes "test-me")) -- 5083
            `shouldBe` True

    -- tests for tags which use mkStringArray
    forM_ [100, 266,
            1017, 1018, 1019, 1027, 1035, 1036, 1039, 1040,
            1047, 1049, 1050, 1054, 1055, 1059, 1060, 1061,
            1062, 1065, 1066, 1067, 1081, 1082, 1085, 1086,
            1087, 1088, 1089, 1090, 1091, 1092, 1097, 1098,
            1099, 1113, 1115, 1117, 1118, 1120, 1121, 1133,
            1135, 1142, 1147, 1148, 1149, 1150, 1153, 1154, 1156,
            1157, 1159, 1160, 1164, 1165, 1166, 1167, 1167,
            1168, 1169, 1178, 1182, 1183, 1186, 1188,
            5000, 5001, 5002, 5003, 5005, 5006, 5007, 5010,
            5029, 5030, 5031, 5035, 5036, 5038, 5039, 5040,
            5041, 5042, 5043, 5044, 5046, 5047, 5049, 5050,
            5052, 5053, 5055, 5056, 5058, 5059, 5060, 5061,
            5066, 5067, 5069, 5071, 5076, 5077, 5079, 5081,
             5086, 5087, 5088, 5089, 5090] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 8") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 8 0 7

        tag == Just (HeaderI18NTable ["test-me"]) || -- 100
            tag == Just (PubKeys ["test-me"]) || -- 266
            tag == Just (INTERNAL (ChangeLog ["test-me"])) || -- 1017
            tag == Just (Source ["test-me"]) || -- 1018
            tag == Just (Patch ["test-me"]) || -- 1019
            tag == Just (OBSOLETE (OldFileNames ["test-me"])) || -- 1027
            tag == Just (FileMD5s ["test-me"]) || -- 1035
            tag == Just (FileLinkTos ["test-me"]) || -- 1036
            tag == Just (FileUserName ["test-me"]) || -- 1039
            tag == Just (FileGroupName ["test-me"]) || -- 1040
            tag == Just (ProvideName ["test-me"]) || -- 1047
            tag == Just (RequireName ["test-me"]) || -- 1049
            tag == Just (RequireVersion ["test-me"]) || -- 1050
            tag == Just (ConflictName ["test-me"]) || -- 1054
            tag == Just (ConflictVersion ["test-me"]) || -- 1055
            tag == Just (ExcludeArch ["test-me"]) || -- 1059
            tag == Just (ExcludeOS ["test-me"]) || -- 1060
            tag == Just (ExclusiveArch ["test-me"]) || -- 1061
            tag == Just (ExclusiveOS ["test-me"]) || -- 1062
            tag == Just (TriggerScripts ["test-me"]) || -- 1065
            tag == Just (TriggerName ["test-me"]) || -- 1066
            tag == Just (TriggerVersion ["test-me"]) || -- 1067
            tag == Just (ChangeLogName ["test-me"]) || -- 1081
            tag == Just (ChangeLogText ["test-me"]) || -- 1082
            tag == Just (PreInProg ["test-me"]) || -- 1085
            tag == Just (PostInProg ["test-me"]) || -- 1086
            tag == Just (PreUnProg ["test-me"]) || -- 1087
            tag == Just (PostUnProg ["test-me"]) || -- 1088
            tag == Just (BuildArchs ["test-me"]) || -- 1089
            tag == Just (ObsoleteName ["test-me"]) || -- 1090
            tag == Just (VerifyScriptProg ["test-me"]) || -- 1091
            tag == Just (TriggerScriptProg ["test-me"]) || -- 1092
            tag == Just (FileLangs ["test-me"]) || -- 1097
            tag == Just (Prefixes ["test-me"]) || -- 1098
            tag == Just (InstPrefixes ["test-me"]) || -- 1099
            tag == Just (ProvideVersion ["test-me"]) || -- 1113
            tag == Just (ObsoleteVersion ["test-me"]) || -- 1115
            tag == Just (BaseNames ["test-me"]) || -- 1117
            tag == Just (DirNames ["test-me"]) || -- 1118
            tag == Just (OrigBaseNames ["test-me"]) || -- 1120
            tag == Just (OrigDirNames ["test-me"]) || -- 1121
            tag == Just (DEPRECATED (PatchesName ["test-me"])) || -- 1133
            tag == Just (DEPRECATED (PatchesVersion ["test-me"])) || -- 1135
            tag == Just (ClassDict ["test-me"]) || -- 1142
            tag == Just (OBSOLETE (FileContexts ["test-me"])) || -- 1147
            tag == Just (FSContexts ["test-me"]) || -- 1148
            tag == Just (ReContexts ["test-me"]) || -- 1149
            tag == Just (Policies ["test-me"]) || -- 1150
            tag == Just (PreTransProg ["test-me"]) || -- 1153
            tag == Just (PostTransProg ["test-me"]) || -- 1154
            tag == Just (OBSOLETE (OldSuggestsName ["test-me"])) || -- 1156
            tag == Just (OBSOLETE (OldSuggestsVersion ["test-me"])) || -- 1157
            tag == Just (OBSOLETE (OldEnhancesName ["test-me"])) || -- 1159
            tag == Just (OBSOLETE (OldEnhancesVersion ["test-me"])) || -- 1160
            tag == Just (UNIMPLEMENTED (BLinkPkgID ["test-me"])) || -- 1164
            tag == Just (UNIMPLEMENTED (BLinkHdrID ["test-me"])) || -- 1165
            tag == Just (UNIMPLEMENTED (BLinkNEVRA ["test-me"])) || -- 1166
            tag == Just (UNIMPLEMENTED (FLinkPkgID ["test-me"])) || -- 1167
            tag == Just (UNIMPLEMENTED (FLinkHdrID ["test-me"])) || -- 1168
            tag == Just (UNIMPLEMENTED (FLinkNEVRA ["test-me"])) || -- 1169
            tag == Just (UNIMPLEMENTED (Variants["test-me"])) || -- 1178
            tag == Just (UNIMPLEMENTED (Keywords["test-me"])) || -- 1182
            tag == Just (UNIMPLEMENTED (BuildPlatforms["test-me"])) || -- 1183
            tag == Just (UNIMPLEMENTED (XattrsDict["test-me"])) || -- 1186
            tag == Just (UNIMPLEMENTED (DepAttrsDict["test-me"])) || -- 1188
            tag == Just (FileNames ["test-me"]) || -- 5000
            tag == Just (FileProvide ["test-me"]) || -- 5001
            tag == Just (FileRequire ["test-me"]) || -- 5002
            tag == Just (UNIMPLEMENTED (FSNames ["test-me"])) || -- 5003
            tag == Just (TriggerConds ["test-me"]) || -- 5005
            tag == Just (TriggerType ["test-me"]) || -- 5006
            tag == Just (OrigFileNames ["test-me"]) || -- 5007
            tag == Just (FileCaps ["test-me"]) || -- 5010
            tag == Just (UNIMPLEMENTED (Collections ["test-me"])) || -- 5029
            tag == Just (PolicyNames ["test-me"]) || -- 5030
            tag == Just (PolicyTypes ["test-me"]) || -- 5031
            tag == Just (OrderName ["test-me"]) || -- 5035
            tag == Just (OrderVersion ["test-me"]) || -- 5036
            tag == Just (UNIMPLEMENTED (MSSFManifest ["test-me"])) || -- 5038
            tag == Just (UNIMPLEMENTED (MSSFDomain ["test-me"])) || -- 5039
            tag == Just (InstFileNames ["test-me"]) || -- 5040
            tag == Just (RequireNEVRs ["test-me"]) || -- 5041
            tag == Just (ProvideNEVRs ["test-me"]) || -- 5042
            tag == Just (ObsoleteNEVRs ["test-me"]) || -- 5043
            tag == Just (ConflictNEVRs ["test-me"]) || -- 5044
            tag == Just (RecommendName ["test-me"]) || -- 5046
            tag == Just (RecommendVersion ["test-me"]) || -- 5047
            tag == Just (SuggestName ["test-me"]) || -- 5049
            tag == Just (SuggestVersion ["test-me"]) || -- 5050
            tag == Just (SupplementName ["test-me"]) || -- 5052
            tag == Just (SupplementVersion ["test-me"]) || -- 5053
            tag == Just (EnhanceName ["test-me"]) || -- 5055
            tag == Just (EnhanceVersion ["test-me"]) || -- 5056
            tag == Just (RecommendNEVRs ["test-me"]) || -- 5058
            tag == Just (SuggestNEVRs ["test-me"]) || -- 5059
            tag == Just (SupplementNEVRs ["test-me"]) || -- 5060
            tag == Just (EnhanceNEVRs ["test-me"]) || -- 5061
            tag == Just (FileTriggerScripts ["test-me"]) || -- 5066
            tag == Just (FileTriggerScriptProg ["test-me"]) || -- 5067
            tag == Just (FileTriggerName ["test-me"]) || -- 5069
            tag == Just (FileTriggerVersion ["test-me"]) || -- 5071
            tag == Just (TransFileTriggerScripts ["test-me"]) || -- 5076
            tag == Just (TransFileTriggerScriptProg ["test-me"]) || -- 5077
            tag == Just (TransFileTriggerName ["test-me"]) || -- 5079
            tag == Just (TransFileTriggerVersion ["test-me"]) || -- 5081
            tag == Just (FileTriggerConds ["test-me"]) || -- 5086
            tag == Just (FileTriggerType ["test-me"]) || -- 5087
            tag == Just (TransFileTriggerConds ["test-me"]) || -- 5088
            tag == Just (TransFileTriggerType ["test-me"]) || -- 5089
            tag == Just (FileSignatures ["test-me"]) -- 5090
            `shouldBe` True

    -- tests for tags which use mkI18NString
    forM_ [1004, 1005, 1016] $ \tagInt -> do
      it ("returns Nothing for `tag' == " ++ show tagInt ++ " and `ty' != 9") $ do
        let store = BS.pack [0]
        let tag = mkTag store tagInt 99 0 0
        tag `shouldBe` Nothing

      it ("returns correct value for `tag' == " ++ show tagInt) $ do
        let store = BC.pack "test-me"
        let tag = mkTag store tagInt 9 0 7

        tag == Just (Summary store) || -- 1004
            tag == Just (Description store) || -- 1005
            tag == Just (Group store) -- 1016
            `shouldBe` True
