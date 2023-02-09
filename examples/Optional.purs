module Example.Optional where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Proxy (Proxy(..))
import TypedEnv (envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

type Settings = ("USERNAME" :: Maybe String)

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (Proxy :: Proxy Settings) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> envErrorMessage error
    Right { "USERNAME": username } -> do
      log $ "Hello, " <> fromMaybe "Sailor" username <> "!"
      pure unit
