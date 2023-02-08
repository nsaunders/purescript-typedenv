-- | The base module for the TypedEnv library

module TypedEnv
  ( fromEnv
  , Variable
  , VariableFlipped
  , type (<:)
  , EnvError(..)
  , envErrorMessage
  , Resolved
  , class ParseValue
  , parseValue
  , class ReadValue
  , readValue
  , class ReadEnv
  , readEnv
  , class ReadEnvFields
  , readEnvFields
  ) where

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons) as String
import Data.String.Common (toLower)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object, lookup)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record (insert) as Record
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

-- | Gets a record of environment variables from a Node environment.
fromEnv
  :: forall e r proxy
   . ReadEnv e r
  => proxy e
  -> Object String
  -> Either EnvError (Record r)
fromEnv = readEnv

-- | Specifies the name and type of an environment variable.
data Variable (name :: Symbol) (ty :: Type)

-- | An alias for `Variable` with the parameters reversed
type VariableFlipped ty name = Variable name ty

-- | An alias for `VariableFlipped`
infixr 5 type VariableFlipped as <:

-- | An error that can occur while reading an environment variable
data EnvError = EnvLookupError String | EnvParseError String

derive instance eqEnvError :: Eq EnvError

derive instance genericEnvError :: Generic EnvError _

instance showEnvError :: Show EnvError where
  show = genericShow

-- | Gets the error message for a given `EnvError` value.
envErrorMessage :: EnvError -> String
envErrorMessage = case _ of
  EnvLookupError var -> "The required variable \"" <> var <>
    "\" was not specified."
  EnvParseError var -> "The variable \"" <> var <>
    "\" was formatted incorrectly."

-- | Useful for a type alias representing a resolved environment
type Resolved :: forall k. Symbol -> k -> k
type Resolved (name :: Symbol) ty = ty

-- | Parses a `String` value to the specified type.
class ParseValue ty where
  parseValue :: String -> Maybe ty

instance parseValueBoolean :: ParseValue Boolean where
  parseValue =
    toLower >>> case _ of
      "0" -> pure false
      "1" -> pure true
      "false" -> pure false
      "true" -> pure true
      _ -> Nothing

instance parseValueChar :: ParseValue Char where
  parseValue = map _.head <<< String.uncons

instance parseValueInt :: ParseValue Int where
  parseValue = Int.fromString

instance parseValueNumber :: ParseValue Number where
  parseValue = Number.fromString

instance parseValueString :: ParseValue String where
  parseValue = pure

-- | Retrieves and parses an environment variable value.
class ReadValue ty where
  readValue :: String -> Object String -> Either EnvError ty

instance readValueOptional :: ParseValue a => ReadValue (Maybe a) where
  readValue name env =
    case lookup name env of
      Nothing ->
        pure Nothing
      Just val ->
        note (EnvParseError name) $ Just <$> parseValue val
else instance readValueRequired :: ParseValue a => ReadValue a where
  readValue name env =
    (note (EnvLookupError name) $ lookup name env)
      >>= (parseValue >>> note (EnvParseError name))

-- | Transforms a row of environment variable specifications to a record.
class ReadEnv (e :: Row Type) (r :: Row Type) where
  readEnv
    :: forall proxy. proxy e -> Object String -> Either EnvError (Record r)

instance readEnvImpl ::
  ( RowToList e el
  , RowToList r rl
  , ReadEnvFields el rl r
  , ListToRow rl r
  , ListToRow el l
  ) =>
  ReadEnv e r where
  readEnv _ = readEnvFields (Proxy :: Proxy el) (Proxy :: Proxy rl)

-- | Transforms a list of environment variable specifications to a record.
class
  ReadEnvFields (el :: RowList Type) (rl :: RowList Type) (r :: Row Type)
  | el -> rl where
  readEnvFields
    :: forall proxy
     . proxy el
    -> proxy rl
    -> Object String
    -> Either EnvError (Record r)

instance readEnvFieldsCons ::
  ( IsSymbol name
  , IsSymbol varName
  , ListToRow rlt rt
  , ReadEnvFields elt rlt rt
  , Row.Lacks name rt
  , Row.Cons name ty rt r
  , ReadValue ty
  ) =>
  ReadEnvFields (Cons name (Variable varName ty) elt) (Cons name ty rlt) r where
  readEnvFields _ _ env = Record.insert nameP <$> value <*> tail
    where
    nameP = Proxy :: Proxy name
    varName = reflectSymbol (Proxy :: Proxy varName)
    value = readValue varName env
    tail = readEnvFields (Proxy :: Proxy elt) (Proxy :: Proxy rlt) env

instance readEnvFieldsNil ::
  TypeEquals {} (Record row) =>
  ReadEnvFields Nil Nil row where
  readEnvFields _ _ _ = pure $ to {}
