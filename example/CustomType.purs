module Example.CustomType where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Int (fromString) as Int
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import TypedEnv (type (<:), class ParseValue, EnvError(..))
import TypedEnv (fromEnv) as TypedEnv

newtype Port = Port Int

instance showPort :: Show Port where
  show (Port n) = show n

instance parseValuePort :: ParseValue Port where
  parseValue = map Port <<< find (_ <= 65535) <<< Int.fromString

type Environment =
  ( host :: String <: "HOST"
  , port :: Port   <: "PORT"
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (RProxy :: RProxy Environment) <$> getEnv
  case env of
    Left (EnvLookupError var) ->
      log $ "ERROR: Required environment variable \"" <> var <> "\" was not set."
    Left (EnvParseError var) ->
      log $ "ERROR: Environment variable \"" <> var <> "\" was formatted incorrectly."
    Right { host, port } -> do
      log $ "Connected to " <> host <> ":" <> show port
      pure unit
