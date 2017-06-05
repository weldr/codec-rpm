module RPM.Tags_TagSpec (spec) where

import Test.Hspec
import RPM.Tags(Null(..),
                Tag(..))

spec :: Spec
spec = describe "RPM.Tags data types" $ do
  describe "Null" $
    it "is equal to itself" $ do
      let null_1 = Null
      null_1 == null_1 `shouldBe` True

-- not implemented
--    it "is different than non-null" $ do
--      let null_1 = Null
--      null_1 /= 1 `shouldBe` True

  describe "Tag" $ do
    it "is equal to itself" $ do
      let tag_1 = Name "anaconda"
      tag_1 == tag_1 `shouldBe` True

    it "tags with two different values are different" $ do
      let tag_1 = Version "20"
      let tag_2 = Version "25"

      tag_1 /= tag_2 `shouldBe` True

    it "two different tags are different" $ do
      let tag_1 = Name "anaconda"
      let tag_2 = Version "25"

      tag_1 /= tag_2 `shouldBe` True
