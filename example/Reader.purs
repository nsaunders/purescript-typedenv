module Example.Reader where

import Prelude
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Either (Either(..))
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import TypedEnv (type (<:), envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

type Config =
  ( username :: Maybe String <: "USERNAME"
  , repeat   :: Maybe Int    <: "REPEAT"
  )

main :: Effect Unit
main = do
  env <- TypedEnv.fromEnv (RProxy :: RProxy Config) <$> getEnv
  case env of
    Left error ->
      log $ "ERROR: " <> envErrorMessage error
    Right config@{ repeat } -> do
      _ <- replicateM (1 + fromMaybe 0 repeat) $ log $ runReader greeting config
      pure unit

greeting :: forall r. Reader { username :: Maybe String | r } String
greeting = asks _.username >>= \username -> pure $ "Hello, " <> fromMaybe "Sailor" username <> "!"
