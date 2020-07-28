module Example.CustomType where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Int (fromString) as Int
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import TypedEnv (type (<:), class ParseValue, envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

newtype Port = Port Int

instance showPort :: Show Port where
  show (Port n) = show n

instance parseValuePort :: ParseValue Port where
  parseValue = map Port <<< find (_ <= 65535) <<< Int.fromString

type Settings =
  ( host :: String <: "HOST"
  , port :: Port   <: "PORT"
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (RProxy :: RProxy Settings) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> envErrorMessage error
    Right { host, port } -> do
      log $ "Connected to " <> host <> ":" <> show port
      pure unit
