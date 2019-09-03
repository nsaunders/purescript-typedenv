module Example.Basic where

import Prelude
import Data.Either (Either(..))
import Data.List.Lazy (replicateM)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import TypedEnv (Variable, EnvError(..))
import TypedEnv (fromEnv) as TypedEnv

type Environment =
  ( greeting :: Variable "GREETING" String
  , count :: Variable "COUNT" Int
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (RProxy :: RProxy Environment) <$> getEnv
  case env of
    Left (EnvLookupError var) ->
      log $ "ERROR: Required environment variable \"" <> var <> "\" was not set."
    Left (EnvParseError var) ->
      log $ "ERROR: Environment variable \"" <> var <> "\" was formatted incorrectly."
    Right { greeting, count } -> do
      _ <- replicateM count $ log greeting
      pure unit
