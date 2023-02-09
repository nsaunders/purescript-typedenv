module Example.Basic where

import Prelude

import Data.Either (Either(..))
import Data.List.Lazy (replicateM)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Proxy (Proxy(..))
import TypedEnv (fromEnv) as TypedEnv
import TypedEnv (printEnvError)

type Environment =
  ( "GREETING" :: String
  , "COUNT" :: Int
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (Proxy :: Proxy Environment) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> printEnvError error
    Right { "GREETING": greeting, "COUNT": count } -> do
      _ <- replicateM count (log greeting)
      pure unit
