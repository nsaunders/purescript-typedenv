module Example.Basic where

import Prelude
import Data.Either (Either(..))
import Data.List.Lazy (replicateM)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import TypedEnv (type (<:), envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

type Environment =
  ( greeting :: String <: "GREETING"
  , count    :: Int    <: "COUNT"
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (RProxy :: RProxy Environment) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> envErrorMessage error
    Right { greeting, count } -> do
      _ <- replicateM count (log greeting)
      pure unit
