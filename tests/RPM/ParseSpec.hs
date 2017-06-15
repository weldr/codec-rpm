module RPM.ParseSpec (spec) where

import Test.Hspec
import Test.Hspec.Attoparsec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


import RPM.Parse
import RPM.Tags
import RPM.Types(Lead(..), SectionHeader(..))


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
