module RPM.TypesSpec (spec) where

import Test.Hspec
import RPM.Types(SectionHeader(..))

spec :: Spec
spec = describe "RPM.Types" $
  describe "SectionHeader" $ do
    it "is equal to itself" $ do
      let header_1 = SectionHeader 0 1 1
      header_1 == header_1 `shouldBe` True

    it "is not equal to another" $ do
      let header_1 = SectionHeader 0 1 1
      let header_2 = SectionHeader 1 1 1
      header_1 /= header_2 `shouldBe` True
