module Example.App where

import Prelude

import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))
import TypedEnv (fromEnv) as TypedEnv
import TypedEnv (printEnvError)

type Config =
  ( "ALERT_EMAIL" :: String
  , "ALERT_SUBJECT" :: String
  )

newtype AppM a = AppM (ReaderT { | Config } Effect a)

runAppM :: { | Config } -> AppM ~> Effect
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM

instance monadAskAppM :: TypeEquals e { | Config } => MonadAsk e AppM where
  ask = AppM $ asks from

main :: Effect Unit
main = do
  eitherConfig <- TypedEnv.fromEnv (Proxy :: _ Config) <$> getEnv
  case eitherConfig of
    Left error ->
      log $ "ERROR: " <> printEnvError error
    Right config ->
      runAppM config sendAlert

sendAlert :: AppM Unit
sendAlert = do
  { "ALERT_EMAIL": email, "ALERT_SUBJECT": subject } <- ask
  liftEffect $ log
    ( "Sending alert with subject \"" <> subject <> "\" to \"" <> email <>
        "\"...done."
    )
