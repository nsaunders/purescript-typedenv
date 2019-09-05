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
import Type.Data.Row (RProxy(..))
import TypedEnv (type (<:), EnvError(..), fromEnv)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] $
  describe "fromEnv" do

    it "reads the requested environment" do
      let
        env = FO.fromHomogeneous { "A": "a value", "B": "b value", "C": "c value" }
        expected = Right { b: "b value", c: "c value" }
        actual = fromEnv (RProxy :: RProxy (b :: String <: "B", c :: String <: "C")) env
      actual `shouldEqual` expected

    it "indicates when a lookup has failed" do
      let
        env = FO.fromHomogeneous { "GREETING": "Hello" }
        expected = Left (EnvLookupError "MESSAGE")
        actual = fromEnv (RProxy :: RProxy (message :: String <: "MESSAGE")) env
      actual `shouldEqual` expected

    it "indicates when parsing a value has failed" do
      let
        env = FO.fromHomogeneous { "DEBUG": "50" }
        expected = Left (EnvParseError "DEBUG")
        actual = fromEnv (RProxy :: RProxy (debug :: Boolean <: "DEBUG")) env
      actual `shouldEqual` expected

    it "parses boolean values" do
      traverse_
        (\({ given, expected }) ->
          shouldEqual
            (fromEnv (RProxy :: RProxy (actual :: Boolean <: "A")) (FO.fromHomogeneous { "A": given }))
            (Right { actual: expected }))
        [ { given: "0", expected: false }
        , { given: "false", expected: false }
        , { given: "1", expected: true }
        , { given: "true", expected: true }
        ]

    it "parses integer values" do
      let
        env = FO.fromHomogeneous { "VALUE": "123" }
        expected = Right { value: 123 }
        actual = fromEnv (RProxy :: RProxy (value :: Int <: "VALUE")) env
      actual `shouldEqual` expected

    it "parses character values" do
      let
        env = FO.fromHomogeneous { "VALUE": "x" }
        expected = Right { value: 'x' }
        actual = fromEnv (RProxy :: RProxy (value :: Char <: "VALUE")) env
      actual `shouldEqual` expected

    it "parses number values" do
      let
        env = FO.fromHomogeneous { "VALUE": "123.456" }
        expected = Right { value: 123.456 }
        actual = fromEnv (RProxy :: RProxy (value :: Number <: "VALUE")) env
      actual `shouldEqual` expected

    it "parses optional values" do
      let
        env = FO.fromHomogeneous { "VALUE": "Hello" }
        expected = Right { value: Just "Hello" }
        actual = fromEnv (RProxy :: RProxy (value :: Maybe String <: "VALUE")) env
      actual `shouldEqual` expected

    it "allows optional values to be absent" do
      let
        expected = Right { value: Nothing }
        actual = fromEnv (RProxy :: RProxy (value :: Maybe String <: "VALUE")) FO.empty
      actual `shouldEqual` expected
