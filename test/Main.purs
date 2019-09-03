module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object (fromHomogeneous) as FO
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Data.Row (RProxy(..))
import TypedEnv (Variable, EnvError(..), fromEnv)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] $
  describe "fromEnv" do

    it "reads the requested environment" do
      let
        env = FO.fromHomogeneous { "FOO": "foo!", "COUNT": "5", "GREETING": "Hello Sailor!", "DEBUG": "1" }
        expected = Right { count: 5, greeting: "Hello Sailor!" }
        actual = fromEnv (RProxy :: RProxy (count :: Variable "COUNT" Int, greeting :: Variable "GREETING" String)) env
      actual `shouldEqual` expected

    it "indicates when a lookup has failed" do
      let
        env = FO.fromHomogeneous { "MAX_USERS": "50" }
        expected = Left (EnvLookupError "MAX_USER_COUNT")
        actual = fromEnv (RProxy :: RProxy (maxUsers :: Variable "MAX_USER_COUNT" Int)) env
      actual `shouldEqual` expected

    it "indicates when parsing a value has failed" do
      let
        env = FO.fromHomogeneous { "DEBUG": "50" }
        expected = Left (EnvParseError "DEBUG")
        actual = fromEnv (RProxy :: RProxy (debug :: Variable "DEBUG" Boolean)) env
      actual `shouldEqual` expected
