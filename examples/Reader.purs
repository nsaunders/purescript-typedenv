module Example.Reader where

import Prelude

import Control.Monad.Reader (Reader, asks, runReader)
import Data.Either (Either(..))
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Proxy (Proxy(..))
import TypedEnv (fromEnv) as TypedEnv
import TypedEnv (printEnvError)

type Config =
  ( "USERNAME" :: Maybe String
  , "REPEAT" :: Maybe Int
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (Proxy :: Proxy Config) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> printEnvError error
    Right config@{ "REPEAT": repeat } -> do
      _ <- replicateM (1 + fromMaybe 0 repeat) $ log $ runReader greeting config
      pure unit

greeting :: forall r. Reader { "USERNAME" :: Maybe String | r } String
greeting = asks _."USERNAME" >>= \username -> pure $ "Hello, "
  <> fromMaybe "Sailor" username
  <> "!"