module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object (empty, fromHomogeneous) as FO
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))
import TypedEnv (EnvError(..), fromEnv)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $
  describe "fromEnv" do

    it "reads the requested environment" do
      let
        env = FO.fromHomogeneous
          { "A": "a value", "B": "b value", "C": "c value" }
        expected = Right { "B": "b value", "C": "c value" }
        actual = fromEnv (Proxy :: _ ("B" :: String, "C" :: String))
          env
      actual `shouldEqual` expected

    it "indicates when a lookup has failed" do
      let
        env = FO.fromHomogeneous { "GREETING": "Hello" }
        expected = Left (EnvLookupError "MESSAGE")
        actual = fromEnv (Proxy :: _ ("MESSAGE" :: String)) env
      actual `shouldEqual` expected

    it "indicates when parsing a value has failed" do
      let
        env = FO.fromHomogeneous { "DEBUG": "50" }
        expected = Left (EnvParseError "DEBUG")
        actual = fromEnv (Proxy :: _ ("DEBUG" :: Boolean)) env
      actual `shouldEqual` expected

    it "indicates when parsing multiple values has failed" do
      let
        env = FO.fromHomogeneous { "A": "err"}
        expected = Left (EnvErrors [EnvParseError "A", EnvLookupError "B"])
        actual = fromEnv (RProxy :: RProxy (a :: Int <: "A", b :: String <: "B")) env
      actual `shouldEqual` expected

    it "parses boolean values" do
      traverse_
        ( \({ given, expected }) ->
            shouldEqual
              ( fromEnv (Proxy :: _ ("A" :: Boolean))
                  (FO.fromHomogeneous { "A": given })
              )
              (Right { "A": expected })
        )
        [ { given: "0", expected: false }
        , { given: "false", expected: false }
        , { given: "1", expected: true }
        , { given: "true", expected: true }
        ]

    it "parses integer values" do
      let
        env = FO.fromHomogeneous { "VALUE": "123" }
        expected = Right { "VALUE": 123 }
        actual = fromEnv (Proxy :: _ ("VALUE" :: Int)) env
      actual `shouldEqual` expected

    it "parses character values" do
      let
        env = FO.fromHomogeneous { "VALUE": "x" }
        expected = Right { "VALUE": 'x' }
        actual = fromEnv (Proxy :: _ ("VALUE" :: Char)) env
      actual `shouldEqual` expected

    it "parses number values" do
      let
        env = FO.fromHomogeneous { "VALUE": "123.456" }
        expected = Right { "VALUE": 123.456 }
        actual = fromEnv (Proxy :: _ ("VALUE" :: Number)) env
      actual `shouldEqual` expected

    it "parses optional values" do
      let
        env = FO.fromHomogeneous { "VALUE": "Hello" }
        expected = Right { "VALUE": Just "Hello" }
        actual = fromEnv (Proxy :: _ ("VALUE" :: Maybe String)) env
      actual `shouldEqual` expected

    it "allows optional values to be absent" do
      let
        expected = Right { "VALUE": Nothing }
        actual = fromEnv (Proxy :: _ ("VALUE" :: Maybe String))
          FO.empty
      actual `shouldEqual` expected
