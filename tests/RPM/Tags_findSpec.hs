module RPM.Tags_findSpec (spec) where

import Test.Hspec
import RPM.Tags(findTag,
                findByteStringTag,
                findStringTag,
                findStringListTag,
                findWord16Tag,
                findWord16ListTag,
                findWord32Tag,
                findWord32ListTag,
                Tag(..))
import qualified Data.ByteString.Char8 as BC


spec :: Spec
spec = describe "RPM.Tags.find*" $ do
  describe "findTag" $ do
    it "returns Nothing if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findTag "NonExistingTag" tags `shouldBe` Nothing

    it "returns Nothing if searching inside empty list" $
      findTag "ProvideName" [] `shouldBe` Nothing

    it "returns Maybe Tag if name is in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tag_2 = RequireName ["python"]
      let tag_3 = RequireVersion ["3.5"]
      let tags = [tag_1, tag_2, tag_3]

      findTag "ProvideName" tags `shouldBe` Just tag_1
      findTag "RequireName" tags `shouldBe` Just tag_2
      findTag "RequireVersion" tags `shouldBe` Just tag_3


  describe "findByteStringTag" $ do
    it "returns Nothing if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findByteStringTag "NonExistingTag" tags `shouldBe` Nothing

    it "returns Nothing if searching inside empty list" $
      findByteStringTag "ProvideName" [] `shouldBe` Nothing

    it "returns Maybe ByteString if name is in the list of tags" $ do
      let tag_1 = Summary (BC.pack "anaconda")
      let tag_2 = Description (BC.pack "The installer")
      let tags = [tag_1, tag_2]

      findByteStringTag "Summary" tags `shouldBe` Just (BC.pack "anaconda")
      findByteStringTag "Description" tags `shouldBe` Just (BC.pack "The installer")


  describe "findStringTag" $ do
    it "returns Nothing if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findStringTag "NonExistingTag" tags `shouldBe` Nothing

    it "returns Nothing if searching inside empty list" $
      findStringTag "ProvideName" [] `shouldBe` Nothing

    it "returns Maybe String if name is in the list of tags" $ do
      let tag_1 = Name "anaconda"
      let tag_2 = Version "25"
      let tags = [tag_1, tag_2]

      findStringTag "Name" tags `shouldBe` Just "anaconda"
      findStringTag "Version" tags `shouldBe` Just "25"


  describe "findStringListTag" $ do
    it "returns [] if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findStringListTag "NonExistingTag" tags `shouldBe` []

    it "returns [] if searching inside empty list" $
      findStringListTag "ProvideName" [] `shouldBe` []

    it "returns [String] if name is in the list of tags" $ do
      let tag_1 = ChangeLog ["something", "changed"]
      let tag_2 = Source ["anaconda.tar.gz"]
      let tags = [tag_1, tag_2]

      findStringListTag "ChangeLog" tags `shouldBe` ["something", "changed"]
      findStringListTag "Source" tags `shouldBe` ["anaconda.tar.gz"]


  describe "findWord16Tag" $ do
    it "returns Nothing if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findWord16Tag "NonExistingTag" tags `shouldBe` Nothing

    it "returns Nothing if searching inside empty list" $
      findWord16Tag "ProvideName" [] `shouldBe` Nothing

    -- "there are no tags holding Word16 values, so no test for those"


  describe "findWord16ListTag" $ do
    it "returns [] if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findWord16ListTag "NonExistingTag" tags `shouldBe` []

    it "returns [] if searching inside empty list" $
      findWord16ListTag "ProvideName" [] `shouldBe` []

    it "returns [Word16] if name is in the list of tags" $ do
      let tag_1 = FileModes [5]
      let tag_2 = FileRDevs [1]
      let tags = [tag_1, tag_2]

      findWord16ListTag "FileModes" tags `shouldBe` [5]
      findWord16ListTag "FileRDevs" tags `shouldBe` [1]


  describe "findWord32Tag" $ do
    it "returns Nothing if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findWord32Tag "NonExistingTag" tags `shouldBe` Nothing

    it "returns Nothing if searching inside empty list" $
      findWord32Tag "ProvideName" [] `shouldBe` Nothing

    it "returns Maybe Word32 if name is in the list of tags" $ do
      let tag_1 = InstallColor 5
      let tag_2 = PackageColor 1
      let tags = [tag_1, tag_2]

      findWord32Tag "InstallColor" tags `shouldBe` Just 5
      findWord32Tag "PackageColor" tags `shouldBe` Just 1


  describe "findWord32ListTag" $ do
    it "returns [] if name is not in the list of tags" $ do
      let tag_1 = ProvideName ["anaconda"]
      let tags = [tag_1]

      findWord32ListTag "NonExistingTag" tags `shouldBe` []

    it "returns [] if searching inside empty list" $
      findWord32ListTag "ProvideName" [] `shouldBe` []

    it "returns [Word32] if name is in the list of tags" $ do
      let tag_1 = FileUIDs [1000]
      let tag_2 = FileGIDs [1001, 1002]
      let tags = [tag_1, tag_2]

      findWord32ListTag "FileUIDs" tags `shouldBe` [1000]
      findWord32ListTag "FileGIDs" tags `shouldBe` [1001, 1002]
