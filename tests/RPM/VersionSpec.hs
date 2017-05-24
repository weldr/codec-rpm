module RPM.VersionSpec (main, spec) where

import Test.Hspec
import Data.Foldable(forM_)
import RPM.Version(DepRequirement(..), EVR(..), parseDepRequirement, parseEVR, satisfies, vercmp)
import qualified RPM.Version as RPM(DepOrdering(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "RPM.Version.vercmp" $ do
        let vercmpCases = [
                         ("1.0", "1.0", EQ),
                         ("1.0", "2.0", LT),
                         ("2.0", "1.0", GT),

                         ("2.0.1", "2.0.1", EQ),
                         ("2.0", "2.0.1", LT),
                         ("2.0.1", "2.0", GT),

                         ("2.0.1a", "2.0.1a", EQ),
                         ("2.0.1a", "2.0.1", GT),
                         ("2.0.1", "2.0.1a", LT),

                         ("5.5p1", "5.5p1", EQ),
                         ("5.5p1", "5.5p2", LT),
                         ("5.5p2", "5.5p1", GT),

                         ("5.5p10", "5.5p10", EQ),
                         ("5.5p1", "5.5p10", LT),
                         ("5.5p10", "5.5p1", GT),

                         ("10xyz", "10.1xyz", LT),
                         ("10.1xyz", "10xyz", GT),

                         ("xyz10", "xyz10", EQ),
                         ("xyz10", "xyz10.1", LT),
                         ("xyz10.1", "xyz10", GT),

                         ("xyz.4", "xyz.4", EQ),
                         ("xyz.4", "8", LT),
                         ("8", "xyz.4", GT),
                         ("xyz.4", "2", LT),
                         ("2", "xyz.4", GT),

                         ("5.5p2", "5.6p1", LT),
                         ("5.6p1", "5.5p2", GT),

                         ("5.6p1", "6.5p1", LT),
                         ("6.5p1", "5.6p1", GT),

                         ("6.0.rc1", "6.0", GT),
                         ("6.0", "6.0.rc1", LT),

                         ("10b2", "10a1", GT),
                         ("10a2", "10b2", LT),
                         ("1.0aa", "1.0aa", EQ),
                         ("1.0a", "1.0aa", LT),
                         ("1.0aa", "1.0a", GT),

                         ("10.0001", "10.0001", EQ),
                         ("10.0001", "10.1", EQ),
                         ("10.1", "10.0001", EQ),
                         ("10.0001", "10.0039", LT),
                         ("10.0039", "10.0001", GT),

                         ("4.999.9", "5.0", LT),
                         ("5.0", "4.999.9", GT),

                         ("20101121", "20101121", EQ),
                         ("20101121", "20101122", LT),
                         ("20101122", "20101121", GT),

                         ("2_0", "2_0", EQ),
                         ("2.0", "2_0", EQ),
                         ("2_0", "2.0", EQ),

                         ("a", "a", EQ),
                         ("a+", "a+", EQ),
                         ("a+", "a_", EQ),
                         ("a_", "a+", EQ),
                         ("+a", "+a", EQ),
                         ("+a", "_a", EQ),
                         ("_a", "+a", EQ),
                         ("+_", "+_", EQ),
                         ("_+", "+_", EQ),
                         ("_+", "_+", EQ),
                         ("+", "_", EQ),
                         ("_", "+", EQ),

                         ("1.0~rc1", "1.0~rc1", EQ),
                         ("1.0~rc1", "1.0", LT),
                         ("1.0", "1.0~rc1", GT),
                         ("1.0~rc1", "1.0~rc2", LT),
                         ("1.0~rc2", "1.0~rc1", GT),
                         ("1.0~rc1~git123", "1.0~rc1~git123", EQ),
                         ("1.0~rc1~git123", "1.0~rc1", LT),
                         ("1.0~rc1", "1.0~rc1~git123", GT)
                       ]

        forM_ vercmpCases $ \(verA, verB, ord) ->
          it (verA ++ " " ++ show ord ++ " " ++ verB) $
            vercmp verA verB `shouldBe` ord

    describe "RPM.Version.EVR Ord" $ do
        let ordCases = [
                     (EVR{epoch=Nothing, version="1.0", release="1"}, EVR{epoch=Nothing, version="1.0", release="1"}, EQ),
                     (EVR{epoch=Just 0,  version="1.0", release="1"}, EVR{epoch=Nothing, version="1.0", release="1"}, EQ),
                     (EVR{epoch=Just 1,  version="1.0", release="1"}, EVR{epoch=Nothing, version="1.0", release="1"}, GT),
                     (EVR{epoch=Nothing, version="1.0", release="1"}, EVR{epoch=Nothing, version="1.1", release="1"}, LT),
                     (EVR{epoch=Nothing, version="1.0", release="1"}, EVR{epoch=Nothing, version="1.0", release="2"}, LT),

                     (EVR{epoch=Just 8,  version="3.6.9", release="11.fc100"}, EVR{epoch=Just 3,  version="3.6.9", release="11.fc100"}, GT),
                     (EVR{epoch=Just 8,  version="3.6.9", release="11.fc100"}, EVR{epoch=Just 11, version="3.6.9", release="11.fc100"}, LT),
                     (EVR{epoch=Just 8,  version="3.6.9", release="11.fc100"}, EVR{epoch=Just 8,  version="7.0",   release="11.fc100"}, LT),
                     (EVR{epoch=Just 8,  version="3.6.9", release="11.fc100"}, EVR{epoch=Just 8,  version="",      release="11.fc100"}, GT),
                     (EVR{epoch=Just 8,  version="",      release="11.fc100"}, EVR{epoch=Just 8,  version="",      release="11.fc100"}, EQ),

                     (EVR{epoch=Nothing, version="1.1",  release="1"}, EVR{epoch=Nothing, version="1.01", release="1"}, EQ),
                     (EVR{epoch=Nothing, version="1..1", release="1"}, EVR{epoch=Nothing, version="1.1",  release="1"}, EQ)
                   ]

        forM_ ordCases $ \(verA, verB, ord) ->
            it (show verA ++ " " ++ show ord ++ " " ++ show verB) $
                compare verA verB `shouldBe` ord

    describe "RPM.Version.satisfies" $ do
        let satisfiesCases = [
              ("no", "match", False),

              ("thing",          "thing",          True),
              ("thing",          "thing >= 1.0-1", True),
              ("thing >= 1.0-1", "thing",          True),

              ("thing = 1.0-1",  "thing = 1.0-1",  True),
              ("thing = 1.0-1",  "thing >= 1.0-1", True),
              ("thing = 1.0-1",  "thing > 1.0-1",  False),
              ("thing = 1.0-1",  "thing < 1.0-1",  False),
              ("thing = 1.0-1",  "thing <= 1.0-1", True),

              ("thing = 1.0",    "thing = 1.0-9",   True),
              ("thing = 1.0",    "thing < 1.0-9",   True),
              ("thing = 1.0",    "thing <= 1.0-9",  True),
              ("thing = 1.0",    "thing >= 1.0-9",  True),
              ("thing = 1.0",    "thing > 1.0-9",   True),

              ("thing = 1.0",    "thing = 1.0",     True),
              ("thing = 1.0",    "thing < 1.0",     False),
              ("thing = 1.0",    "thing > 1.0",     False),
              ("thing = 1.0",    "thing >= 1.0",    True),
              ("thing = 1.0",    "thing <= 1.0",    True),

              ("thing < 1.0",    "thing = 1.0",     False),
              ("thing < 1.0",    "thing < 1.0",     True),
              ("thing < 1.0",    "thing > 1.0",     False),
              ("thing < 1.0",    "thing >= 1.0",    False),
              ("thing < 1.0",    "thing <= 1.0",    True),

              ("thing > 1.0",    "thing = 1.0",     False),
              ("thing > 1.0",    "thing < 1.0",     False),
              ("thing > 1.0",    "thing > 1.0",     True),
              ("thing > 1.0",    "thing >= 1.0",    True),
              ("thing > 1.0",    "thing <= 1.0",    False),

              ("thing >= 1.0",   "thing = 1.0",     True),
              ("thing >= 1.0",   "thing < 1.0",     False),
              ("thing >= 1.0",   "thing > 1.0",     True),
              ("thing >= 1.0",   "thing >= 1.0",    True),
              ("thing >= 1.0",   "thing <= 1.0",    True),

              ("thing <= 1.0",   "thing = 1.0",     True),
              ("thing <= 1.0",   "thing < 1.0",     True),
              ("thing <= 1.0",   "thing > 1.0",     False),
              ("thing <= 1.0",   "thing >= 1.0",    True),
              ("thing <= 1.0",   "thing <= 1.0",    True),

              ("thing <= 1.0",   "thing = 1.0-9",   True),
              ("thing <= 1.0",   "thing < 1.0-9",   True),
              ("thing <= 1.0",   "thing <= 1.0-9",  True),
              ("thing <= 1.0",   "thing >= 1.0-9",  True),
              ("thing <= 1.0",   "thing > 1.0-9",   True),

              ("thing >= 1.0",   "thing = 1.0-9",   True),
              ("thing >= 1.0",   "thing < 1.0-9",   True),
              ("thing >= 1.0",   "thing <= 1.0-9",  True),
              ("thing >= 1.0",   "thing >= 1.0-9",  True),
              ("thing >= 1.0",   "thing > 1.0-9",   True),

              ("thing = 1.0-9",  "thing = 1.0-9",   True),
              ("thing < 1.0-9",  "thing = 1.0-9",   False),
              ("thing <= 1.0-9", "thing = 1.0-9",   True),
              ("thing > 1.0-9",  "thing = 1.0-9",   False),
              ("thing >= 1.0-9", "thing = 1.0-9",   True),

              ("thing >= 1.0-1", "thing = 1.0-1",  True),
              ("thing >= 1.0-1", "thing >= 1.0-1", True),
              ("thing >= 1.0-1", "thing > 1.0-1",  True),
              ("thing >= 1.0-1", "thing < 1.0-1",  False),
              ("thing >= 1.0-1", "thing <= 1.0-1", True),

              ("thing > 1.0-1",  "thing = 1.0-1",  False),
              ("thing > 1.0-1",  "thing >= 1.0-1", True),
              ("thing > 1.0-1",  "thing > 1.0-1",  True),
              ("thing > 1.0-1",  "thing < 1.0-1",  False),
              ("thing > 1.0-1",  "thing <= 1.0-1", False),

              ("thing < 1.0-1",  "thing = 1.0-1",  False),
              ("thing < 1.0-1",  "thing >= 1.0-1", False),
              ("thing < 1.0-1",  "thing > 1.0-1",  False),
              ("thing < 1.0-1",  "thing < 1.0-1",  True),
              ("thing < 1.0-1",  "thing <= 1.0-1", True),

              ("thing <= 1.0-1", "thing = 1.0-1",  True),
              ("thing <= 1.0-1", "thing >= 1.0-1", True),
              ("thing <= 1.0-1", "thing > 1.0-1",  False),
              ("thing <= 1.0-1", "thing < 1.0-1",  True),
              ("thing <= 1.0-1", "thing <= 1.0-1", True),

              ("thing = 9.0",    "thing = 1.0-1",  False),
              ("thing = 9.0",    "thing >= 1.0-1", True),
              ("thing = 9.0",    "thing > 1.0-1",  True),
              ("thing = 9.0",    "thing <= 1.0-1", False),
              ("thing = 9.0",    "thing < 1.0-1",  False),

              ("thing < 9.0",    "thing = 1.0-1",  True),
              ("thing < 9.0",    "thing >= 1.0-1", True),
              ("thing < 9.0",    "thing > 1.0-1",  True),
              ("thing < 9.0",    "thing <= 1.0-1", True),
              ("thing < 9.0",    "thing < 1.0-1",  True),

              ("thing <= 9.0",   "thing = 1.0-1",  True),
              ("thing <= 9.0",   "thing >= 1.0-1", True),
              ("thing <= 9.0",   "thing > 1.0-1",  True),
              ("thing <= 9.0",   "thing <= 1.0-1", True),
              ("thing <= 9.0",   "thing < 1.0-1",  True),

              ("thing > 9.0",    "thing = 1.0-1",  False),
              ("thing > 9.0",    "thing >= 1.0-1", True),
              ("thing > 9.0",    "thing > 1.0-1",  True),
              ("thing > 9.0",    "thing <= 1.0-1", False),
              ("thing > 9.0",    "thing < 1.0-1",  False),

              ("thing >= 9.0",   "thing = 1.0-1",  False),
              ("thing >= 9.0",   "thing >= 1.0-1", True),
              ("thing >= 9.0",   "thing > 1.0-1",  True),
              ("thing >= 9.0",   "thing <= 1.0-1", False),
              ("thing >= 9.0",   "thing < 1.0-1",  False),

              ("thing = 1.0",    "thing = 9.0-1",  False),
              ("thing = 1.0",    "thing >= 9.0-1", False),
              ("thing = 1.0",    "thing > 9.0-1",  False),
              ("thing = 1.0",    "thing <= 9.0-1", True),
              ("thing = 1.0",    "thing < 9.0-1",  True),

              ("thing < 1.0",    "thing = 9.0-1",  False),
              ("thing < 1.0",    "thing >= 9.0-1", False),
              ("thing < 1.0",    "thing > 9.0-1",  False),
              ("thing < 1.0",    "thing <= 9.0-1", True),
              ("thing < 1.0",    "thing < 9.0-1",  True),

              ("thing <= 1.0",   "thing = 9.0-1",  False),
              ("thing <= 1.0",   "thing >= 9.0-1", False),
              ("thing <= 1.0",   "thing > 9.0-1",  False),
              ("thing <= 1.0",   "thing <= 9.0-1", True),
              ("thing <= 1.0",   "thing < 9.0-1",  True),

              ("thing >= 1.0",   "thing = 9.0-1",  True),
              ("thing >= 1.0",   "thing >= 9.0-1", True),
              ("thing >= 1.0",   "thing > 9.0-1",  True),
              ("thing >= 1.0",   "thing <= 9.0-1", True),
              ("thing >= 1.0",   "thing < 9.0-1",  True),

              ("thing > 1.0",    "thing = 9.0-1",  True),
              ("thing > 1.0",    "thing >= 9.0-1", True),
              ("thing > 1.0",    "thing > 9.0-1",  True),
              ("thing > 1.0",    "thing <= 9.0-1", True),
              ("thing > 1.0",    "thing < 9.0-1",  True)
              ]

        forM_ satisfiesCases $ \(v1, v2, b) ->
            it (v1 ++ " " ++ v2 ++ ": " ++ show b) $
                case (parseDepRequirement v1, parseDepRequirement v2) of
                    (Right verA, Right verB) -> satisfies verA verB `shouldBe` b
                    _                        -> expectationFailure "Unable to parse versions"

    describe "RPM.Version.parseEVR" $ do
        let parseEVRCases = [
                ("1.0-11.fc100",    Right EVR{epoch=Nothing, version="1.0", release="11.fc100"}),
                ("0:1.0-11.fc100",  Right EVR{epoch=Just 0,  version="1.0", release="11.fc100"}),
                ("8:1.0-11.fc100",  Right EVR{epoch=Just 8,  version="1.0", release="11.fc100"}),
                ("1.0",             Right EVR{epoch=Nothing, version="1.0", release=""}),
                ("8:1.0",           Right EVR{epoch=Just 8,  version="1.0", release=""}),

                -- missing epoch
                (":1.0-11.fc100",   Left ()),

                -- missing version
                ("0:-11.fc100",     Left ()),

                -- missing release
                ("0:1.0-",          Left ()),

                -- invalid epochs
                ("-1:1.0-100.fc11", Left ()),
                ("A:1.0-100.fc11",  Left ()),
                ("8589934592:1.0-100.fc11", Left ()),

                -- invalid versions
                ("0:1.0:0-100.fc11",  Left ()),
                ("0:1.0&0-100.fc11",  Left ()),
                ("0:1.0\x01f32e\&0-100.fc11", Left ()),

                -- invalid releases
                ("0:1.0-100.fc:11",  Left ()),
                ("0:1.0-100.fc&11",  Left ()),
                ("0:1.0-100.fc\x01f32e\&11", Left ())
                ]

        -- For the error cases, we don't actually care about the contents of the error, so just
        -- make sure parseEVR returns a Left. Also, the Either returned by parseEVR is not the
        -- same type as the ones in the test cases (ParseError vs. ()), so unwrap the Right EVR
        -- cases to compare.
        -- Making fake ParseErrors is hard, so the Either returned by parseEVR is not the same type
        -- as the either in the test data (ParseError vs. ()). Unwrap the Right values to compare EVRs,
        -- and for parse errors just check that parseEVR returns a Left.
        forM_ parseEVRCases $ \(str, result) ->
            it str $ case (result, parseEVR str) of
                (Right evr1, Right evr2) -> evr1 `shouldBe` evr2
                (Left _, Right evr)      -> expectationFailure $ "bad string parsed as: " ++ show evr
                (Right _, Left err)      -> expectationFailure $ "unable to parse valid EVR: " ++ show err
                _                        -> return ()

    describe "RPM.Version.parseDepRequirement" $ do
        let parseDepRequirementCases = [
                ("libthing",        DepRequirement "libthing" Nothing),
                ("libthing >= 1.0", DepRequirement "libthing" $ Just (RPM.GTE, EVR{epoch=Nothing, version="1.0", release=""})),
                ("libthing > 1.0",  DepRequirement "libthing" $ Just (RPM.GT,  EVR{epoch=Nothing, version="1.0", release=""})),
                ("libthing = 1.0",  DepRequirement "libthing" $ Just (RPM.EQ,  EVR{epoch=Nothing, version="1.0", release=""})),
                ("libthing < 1.0",  DepRequirement "libthing" $ Just (RPM.LT,  EVR{epoch=Nothing, version="1.0", release=""})),
                ("libthing <= 1.0", DepRequirement "libthing" $ Just (RPM.LTE, EVR{epoch=Nothing, version="1.0", release=""})),

                -- sometimes an RPM will have an invalid version in a Requires/Provides line.
                -- make sure the whole thing gets parsed as a name
                ("httpd-mmn = 20120211-x86-64", DepRequirement "httpd-mmn = 20120211-x86-64" Nothing)
                ]

        forM_ parseDepRequirementCases $ \(str, result) ->
            it str $ case parseDepRequirement str of
                Right dr -> dr `shouldBe` result
                Left err -> expectationFailure $ "failed to parse DepRequirement: " ++ show err
