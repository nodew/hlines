module HLines.FileSpec where

import Test.Hspec
import HLines.File
import HLines.Type

spec :: Spec
spec = do
  describe "Count lines of code" $ do
    it "PLASMA" $ do
      (lang, content) <- readContent "test-data/plasma.c"
      countLines lang content `shouldBe` Count 32032 8848 3792 44672

    it "FE" $ do
      (lang, content) <- readContent "test-data/fe25519.c"
      countLines lang content `shouldBe` Count 278 51 8 (278 + 51 + 8)

    it "EBC" $ do
      (lang, content) <- readContent "test-data/ebcdic.c"
      countLines lang content `shouldBe` Count 165 18 101 (165 + 18 + 101)

    it "DUMB" $ do
      (lang, content) <- readContent "test-data/dumb.c"
      countLines lang content `shouldBe` Count 2 0 3 5

    it "IPL" $ do
      (lang, content) <- readContent "test-data/ipl_funcs.c"
      countLines lang content `shouldBe` Count 25 6 43 (25 + 6 + 43)

    it "RUBY" $ do
      (lang, content) <- readContent "test-data/test.rb"
      countLines lang content `shouldBe` Count 2 0 2 4

    it "OCAML" $ do
      (lang, content) <- readContent "test-data/ocaml.ml"
      countLines lang content `shouldBe` Count 3 4 6 14

    it "ADA" $ do
      (lang, content) <- readContent "test-data/ada.ada"
      countLines lang content `shouldBe` Count 4 0 3 7

    it "GHERKIN" $ do
      (lang, content) <- readContent "test-data/gherkin.feature"
      countLines lang content `shouldBe` Count 8 2 2 12

    it "GROOVY" $ do
      (lang, content) <- readContent "test-data/test.groovy"
      countLines lang content `shouldBe` Count 6 1 10 17

    it "TERRAFORM" $ do
      (lang, content) <- readContent "test-data/test.tf"
      countLines lang content `shouldBe` Count 65 13 11 (65 + 13 + 11)

    it "ZIG" $ do
      (lang, content) <- readContent "test-data/zig.zig"
      countLines lang content `shouldBe` Count 5 2 2 9

    it "NIX" $ do
      (lang, content) <- readContent "test-data/test.nix"
      countLines lang content `shouldBe` Count 3 2 3 8

    it "POWERSHELL" $ do
      (lang, content) <- readContent "test-data/test.ps1"
      countLines lang content `shouldBe` Count 2 1 6 9

    it "HANDLEBARS" $ do
      (lang, content) <- readContent "test-data/test.handlebars"
      countLines lang content `shouldBe` Count 2 0 2 4

    it "NESTED_HASKELL" $ do
      (lang, content) <- readContent "test-data/nested-comments.hs"
      countLines lang content `shouldBe` Count 2 4 8 14