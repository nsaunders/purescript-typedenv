module Example.Optional where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import TypedEnv (type (<:), envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

type Settings = ( username :: Maybe String <: "USERNAME" )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (RProxy :: RProxy Settings) <$> getEnv
  case env of
    Left errors ->
      log $ "ERROR: " <> show (map envErrorMessage errors)
    Right { username } -> do
      log $ "Hello, " <> fromMaybe "Sailor" username <> "!"
      pure unit
