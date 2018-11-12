module HLines.FileSpec where

import Test.Hspec
import HLines.Counter
import System.IO
import System.FilePath
import HLines.Internal
import HLines.Type
import HLines.Language

readContent :: FilePath -> IO (Comment, Lines)
readContent fp = do
  (_, comment, lines) <- readLines fp
  return (comment, lines)

spec :: Spec
spec = do
  describe "Count lines of code" $ do
    it "PLASMA" $ do
      (comment, content) <- readContent "test-data/plasma.c"
      countLines comment content `shouldBe` Count 32032 8848 3792 44672

    it "FE" $ do
      (comment, content) <- readContent "test-data/fe25519.c"
      countLines comment content `shouldBe` Count 278 51 8 (278 + 51 + 8)

    it "EBC" $ do
      (comment, content) <- readContent "test-data/ebcdic.c"
      countLines comment content `shouldBe` Count 165 18 101 (165 + 18 + 101)

    it "DUMB" $ do
      (comment, content) <- readContent "test-data/dumb.c"
      countLines comment content `shouldBe` Count 2 0 3 5

    it "IPL" $ do
      (comment, content) <- readContent "test-data/ipl_funcs.c"
      countLines comment content `shouldBe` Count 25 6 43 (25 + 6 + 43)

    it "RUBY" $ do
      (comment, content) <- readContent "test-data/test.rb"
      countLines comment content `shouldBe` Count 2 0 2 4

    it "OCAML" $ do
      (comment, content) <- readContent "test-data/ocaml.ml"
      countLines comment content `shouldBe` Count 3 4 6 13

    it "ADA" $ do
      (comment, content) <- readContent "test-data/ada.ada"
      countLines comment content `shouldBe` Count 4 0 3 7

    it "GHERKIN" $ do
      (comment, content) <- readContent "test-data/gherkin.feature"
      countLines comment content `shouldBe` Count 8 2 2 12

    it "GROOVY" $ do
      (comment, content) <- readContent "test-data/test.groovy"
      countLines comment content `shouldBe` Count 6 1 10 17

    it "TERRAFORM" $ do
      (comment, content) <- readContent "test-data/test.tf"
      countLines comment content `shouldBe` Count 65 13 11 (65 + 13 + 11)

    it "ZIG" $ do
      (comment, content) <- readContent "test-data/zig.zig"
      countLines comment content `shouldBe` Count 5 2 2 9

    it "NIX" $ do
      (comment, content) <- readContent "test-data/test.nix"
      countLines comment content `shouldBe` Count 3 2 3 8

    it "POWERSHELL" $ do
      (comment, content) <- readContent "test-data/test.ps1"
      countLines comment content `shouldBe` Count 2 1 6 9

    it "HANDLEBARS" $ do
      (comment, content) <- readContent "test-data/test.handlebars"
      countLines comment content `shouldBe` Count 2 0 2 4

    it "NESTED_HASKELL" $ do
      (comment, content) <- readContent "test-data/nested-comments.hs"
      countLines comment content `shouldBe` Count 2 4 8 14
