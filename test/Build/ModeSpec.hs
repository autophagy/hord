module Build.ModeSpec (spec) where

import Build (BuildMode (..), determineMode)
import Test.Hspec

spec :: Spec
spec = do
  describe "determineMode" $ do
    it "decides to compile to Raw when no dhall src is present" $ do
      determineMode "blah.txt" "blah.txt" `shouldBe` Raw

    it "decides to compile to YAML when dest is a yaml file" $ do
      determineMode "blah.dhall" "blah.yaml" `shouldBe` YAML

    it "decides to compile to YAML when dest is a yml file" $ do
      determineMode "blah.dhall" "blah.yml" `shouldBe` YAML

    it "decides to compile to JSON when dest is a json file" $ do
      determineMode "blah.dhall" "blah.json" `shouldBe` JSON

    it "decides to compile to Text when dest is neither yaml or json" $ do
      determineMode "blah.dhall" "blah.txt" `shouldBe` Text
