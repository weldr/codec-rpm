module RPM.ParseSpec (spec) where

import Test.Hspec
import Test.Hspec.Attoparsec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


import RPM.Parse
import RPM.Tags
import RPM.Types

spec :: Spec
spec = describe "RPM.Parse" $ do
  describe "parseLead" $ do
    it "fails with wrong file signature" $ do
      let stream = BS.pack [
            0xFF, 0xFF, 0xFF, 0xFF, -- *WRONG* File signature
            8, -- Major
            9, -- Minor
            0, 1, -- Type
            0, 2,  -- ArchNum
            65, 66, 67, 68, 69, 70, -- name 66 bytes ABCDEF
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, -- OS num
            0, 4, -- Sig type
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 16 bytes padding
      parseLead `shouldFailOn` stream
      -- NOTE: http://alpmestan.com/posts/2014-06-18-testing-attoparsec-parsers-with-hspec.html
      -- Right now, hspec-attoparsec will only consider leftovers when the parser succeeds.
      -- I’m not really sure whether we should return Fail’s unconsumed input or not.
      --
      -- We can't use `leavesUnconsumed` when the parser fails
      -- so the following assertion is invalid for now!
      -- stream ~?> parseLead `leavesUnconsumed` BS.pack []

    it "fails when name < 66 chars" $ do
      let stream = BS.pack [
            0xED, 0xAB, 0xEE, 0xDB, -- File signature
            8, -- Major
            9, -- Minor
            0, 1, -- Type
            0, 1,  -- ArchNum
            65, 66, 67, 68, 69, 70, -- name 26 instead of 66 bytes
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, -- OS num
            0, 4, -- Sig type
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 16 bytes padding

      parseLead `shouldFailOn` stream

    it "fails when padding < 16 bytes" $ do
      let stream = BS.pack [
            0xED, 0xAB, 0xEE, 0xDB, -- File signature
            8, -- Major
            9, -- Minor
            0, 1, -- Type
            0, 1,  -- ArchNum
            65, 66, 67, 68, 69, 70, -- name 66 bytes
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, -- OS num
            0, 4, -- Sig type
            0, 0, 0, 0, 0, 0] -- 6 instead of 16 bytes padding

      parseLead `shouldFailOn` stream

    it "succeeds with valid data" $ do
      let stream = BS.pack [
            0xED, 0xAB, 0xEE, 0xDB, -- File signature
            8, -- Major
            9, -- Minor
            0, 1, -- Type
            0, 2,  -- ArchNum
            65, 66, 67, 68, 69, 70, -- name 66 bytes ABCDEF
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, -- OS num
            0, 4, -- Sig type
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 16 bytes padding
      let expected = Lead 8 9 1 2 "ABCDEF" 1 4

      -- parsing succeeds
      parseLead `shouldSucceedOn` stream
      -- no unconsumed input
      stream ~?> parseLead `leavesUnconsumed` BS.pack []
      -- result is as expected
      stream ~> parseLead `parseSatisfies` (==expected)

  describe "parseSectionHeader" $ do
    it "fails with invalid section header signature" $ do
      let stream = BS.pack [
            0xFF, 0xFF, 0xFF, -- *WRONG* section header signature
            8, -- sectionVersion
            0, 0, 0, 0, -- 4 reserved bytes
            255, 255, 255, 255, -- sectionCount 4 bytes
            0, 0, 255, 255] -- sectionSize 4 bytes

      parseSectionHeader `shouldFailOn` stream

    it "fails when reserved section < 4 bytes" $ do
      let stream = BS.pack [
            0x8E, 0xAD, 0xE8, -- section header signature
            8, -- sectionVersion
            0, 0, -- *2 instead of* 4 reserved bytes
            255, 255, 255, 255, -- sectionCount 4 bytes
            0, 0, 255, 255] -- sectionSize 4 bytes

      parseSectionHeader `shouldFailOn` stream

    it "succeeds with valid data" $ do
      let stream = BS.pack [
            0x8E, 0xAD, 0xE8, -- section header signature
            8, -- sectionVersion
            0, 0, 0, 0, -- 4 reserved bytes
            255, 255, 255, 255, -- sectionCount 4 bytes
            0, 0, 255, 255] -- sectionSize 4 bytes
      let expected = SectionHeader 8 4294967295 65535

      -- parsing succeeds
      parseSectionHeader `shouldSucceedOn` stream
      -- no unconsumed input
      stream ~?> parseSectionHeader `leavesUnconsumed` BS.pack []
      -- result is as expected
      stream ~> parseSectionHeader `parseSatisfies` (==expected)

  describe "parseOneTag" $ do
    it "returns Nothing when any of the input streams is empty" $ do
      parseOneTag (BS.pack []) (BS.pack [])              `shouldBe` Nothing
      parseOneTag (BS.pack [1, 2, 3, 4, 5]) (BS.pack []) `shouldBe` Nothing
      parseOneTag (BS.pack []) (BS.pack [100, 8, 0, 7])  `shouldBe` Nothing

    it "returns valid Tag when both input streams are valid" $ do
      let store = BC.pack "123-test-me"
      let bs = BS.pack [0, 0, 0, 100, -- tag
                        0, 0, 0, 8, -- ty
                        0, 0, 0, 4, -- offset
                        0, 0, 0, 7 -- count
                       ]
      parseOneTag store bs `shouldBe` Just (HeaderI18NTable ["test-me"])

  describe "parseSection" $
    it "succeeds with valid data" $ do
      let stream = BS.pack [
            0x8E, 0xAD, 0xE8, -- section header signature
            1, -- sectionVersion
            0, 0, 0, 0, -- 4 reserved bytes
            0, 0, 0, 7, -- sectionCount 4 bytes
            0, 0, 0, 0xE4, -- sectionSize 4 bytes

            -- tags defined in this section, sectionCount * 16 bytes
            -- 62 7 212 16 == HeaderSignatures (Null), ty(7) /= 0, returns Nothing
            0x00, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0xd4, 0x00, 0x00, 0x00, 0x10,
            -- 267 7 0 72 == DSAHeader (binary)
            0x00, 0x00, 0x01, 0x0b, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x48,
            -- 269 6 72 1 == SHA1Header (string)
            0x00, 0x00, 0x01, 0x0d, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x01,
            -- 1000 4 116 1 == Name (string), ty(4) /= 6, returns Nothing
            0x00, 0x00, 0x03, 0xe8, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x74, 0x00, 0x00, 0x00, 0x01,
            -- 1004 7 120 16 == Summary (i18n string), ty(7) /= 9, returns Nothing
            0x00, 0x00, 0x03, 0xec, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x10,
            -- 1005 7 136 72 == Description (i18n string), ty(7) /= 9, returns Nothing
            0x00, 0x00, 0x03, 0xed, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x88, 0x00, 0x00, 0x00, 0x48,
            -- 1007 4 208 1 == BuildHost (string), ty(4) /= 6, returns Nothing
            0x00, 0x00, 0x03, 0xef, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xd0, 0x00, 0x00, 0x00, 0x01,

            -- section payload (228 bytes in this example)
            -- DSAHeader
            0x88, 0x46, 0x04, 0x00, 0x11, 0x02, 0x00, 0x06, 0x05, 0x02, 0x53, 0x67, 0xab, 0x9d, 0x00, 0x0a,
            0x09, 0x10, 0x50, 0x8c, 0xe5, 0xe6, 0x66, 0x53, 0x4c, 0x2b, 0x6b, 0x83, 0x00, 0xa0, 0x9e, 0x1c,
            0x4e, 0x19, 0xd5, 0x78, 0x37, 0x53, 0x61, 0x8a, 0x34, 0x4b, 0x40, 0x91, 0xfb, 0xc1, 0x24, 0xbb,
            0x1c, 0x62, 0x00, 0x9f, 0x50, 0xe6, 0x5c, 0x34, 0x72, 0xa6, 0x54, 0x70, 0x45, 0xce, 0xe9, 0xec,
            0x02, 0x6b, 0x98, 0xfa, 0x45, 0x72, 0x8f, 0xca, -- SHA1Header
                                                            0x66, 0x36, 0x37, 0x35, 0x64, 0x37, 0x39, 0x62,
            0x66, 0x66, 0x33, 0x34, 0x34, 0x66, 0x36, 0x63, 0x63, 0x63, 0x32, 0x64, 0x34, 0x65, 0x37, 0x31,
            0x66, 0x66, 0x62, 0x62, 0x38, 0x61, 0x39, 0x63, 0x36, 0x38, 0x39, 0x62, 0x61, 0x64, 0x65, 0x63,
            0x00, 0x00, 0x00, 0x00, -- Name
                                    0x01, 0x2b, 0x00, 0xf6, -- Summary
                                                            0x00, 0x7c, 0xa1, 0x57, 0x31, 0x3e, 0x1f, 0x20,
            0x34, 0x4f, 0xf7, 0x1e, 0xc9, 0xbb, 0xd7, 0xdc, -- Description
                                                            0x88, 0x46, 0x04, 0x00, 0x11, 0x02, 0x00, 0x06,
            0x05, 0x02, 0x53, 0x67, 0xab, 0x9d, 0x00, 0x0a, 0x09, 0x10, 0x50, 0x8c, 0xe5, 0xe6, 0x66, 0x53,
            0x4c, 0x2b, 0x6f, 0xdf, 0x00, 0x9d, 0x13, 0x63, 0xe0, 0x2f, 0xed, 0x88, 0x8b, 0x27, 0xad, 0x46,
            0x23, 0x26, 0xb7, 0xa8, 0xda, 0xb7, 0xea, 0x64, 0x71, 0x88, 0x00, 0xa0, 0x80, 0x31, 0xf5, 0x32,
            0x25, 0x81, 0xf9, 0xce, 0xe1, 0x63, 0x15, 0x15, 0x9d, 0xe3, 0x74, 0xc1, 0x23, 0xa8, 0xba, 0xaf,
            -- BuildHost
            0x01, 0x2a, 0xeb, 0x70, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00, 0x07, 0xff, 0xff, 0xff, 0x90,
            0x00, 0x00, 0x00, 0x00] -- followed by 4 bytes of signature padding before the next section

      -- parsing succeeds
      parseSection `shouldSucceedOn` stream
      -- no unconsumed input
      stream ~?> parseSection `leavesUnconsumed` BS.pack []

      -- verify the result matches expected
      stream ~> parseSection `parseSatisfies` matchExpected
        where
          matchExpected h = do
            let expSH = SectionHeader 1 7 228
            let expT = [
                        DSAHeader (BS.pack [
                            0x88, 0x46, 0x04, 0x00, 0x11, 0x02, 0x00, 0x06, 0x05, 0x02, 0x53, 0x67, 0xab, 0x9d, 0x00, 0x0a,
                            0x09, 0x10, 0x50, 0x8c, 0xe5, 0xe6, 0x66, 0x53, 0x4c, 0x2b, 0x6b, 0x83, 0x00, 0xa0, 0x9e, 0x1c,
                            0x4e, 0x19, 0xd5, 0x78, 0x37, 0x53, 0x61, 0x8a, 0x34, 0x4b, 0x40, 0x91, 0xfb, 0xc1, 0x24, 0xbb,
                            0x1c, 0x62, 0x00, 0x9f, 0x50, 0xe6, 0x5c, 0x34, 0x72, 0xa6, 0x54, 0x70, 0x45, 0xce, 0xe9, 0xec,
                            0x02, 0x6b, 0x98, 0xfa, 0x45, 0x72, 0x8f, 0xca]),
                        SHA1Header "f675d79bff344f6ccc2d4e71ffbb8a9c689badec"
                        ]
            headerSectionHeader h == expSH && headerTags h == expT && BS.length (headerStore h) == 228
