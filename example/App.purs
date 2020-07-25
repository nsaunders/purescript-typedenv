module Example.App where

import Prelude
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Node.Process (getEnv)
import Type.Data.Row (RProxy(..))
import Type.Equality (class TypeEquals, from)
import TypedEnv (Resolved, Variable, envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

type Config f =
  ( alertEmail   :: f "ALERT_EMAIL" String
  , alertSubject :: f "ALERT_SUBJECT" String
  )

type ResolvedConfig = Record (Config Resolved)

newtype AppM a = AppM (ReaderT ResolvedConfig Effect a)

runAppM :: ResolvedConfig -> AppM ~> Effect
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM

instance monadAskAppM :: TypeEquals e ResolvedConfig => MonadAsk e AppM where
  ask = AppM $ asks from

main :: Effect Unit
main = do
  eitherConfig <- TypedEnv.fromEnv (RProxy :: RProxy (Config Variable)) <$> getEnv
  case eitherConfig of
    Left errors ->
      log $ "ERROR: " <> show (map envErrorMessage errors)
    Right config ->
      runAppM config sendAlert

sendAlert :: AppM Unit
sendAlert = do
  email <- asks _.alertEmail
  subject <- asks _.alertSubject
  liftEffect $ log ("Sending alert with subject \"" <> subject <> "\" to \"" <> email <> "\"...done.")
