module Example.CustomType where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Int (fromString) as Int
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Proxy (Proxy(..))
import TypedEnv (class ParseValue, printEnvError)
import TypedEnv (fromEnv) as TypedEnv

newtype Port = Port Int

instance showPort :: Show Port where
  show (Port n) = show n

instance parseValuePort :: ParseValue Port where
  parseValue = map Port <<< find (_ <= 65535) <<< Int.fromString

type Settings =
  { "HOST" :: String
  , "PORT" :: Port
  }

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (Proxy :: Proxy Settings) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> printEnvError error
    Right { "HOST": host, "PORT": port } -> do
      log $ "Connected to " <> host <> ":" <> show port
      pure unit