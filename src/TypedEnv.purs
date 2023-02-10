-- | The base module for the TypedEnv library

module TypedEnv
  ( fromEnv
  , printEnvError
  , EnvError(..)
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

import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.List (List(..), foldMap, (:))
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
  :: forall e r
   . ReadEnv e r
  => Proxy e
  -> Object String
  -> Either (List EnvError) (Record r)
fromEnv = readEnv

-- | An error that can occur while reading an environment variable
data EnvError
  = EnvLookupError String
  | EnvParseError String

derive instance eqEnvError :: Eq EnvError

derive instance genericEnvError :: Generic EnvError _

instance showEnvError :: Show EnvError where
  show err = genericShow err

-- | Gets the error message for a given `EnvError` value.
printEnvError :: List EnvError -> String
printEnvError =
  case _ of
    (x : Nil) ->
      msg x
    xxs ->
      "Multiple environment errors: " <> foldMap (\x -> "\n* " <> msg x) xxs
  where
  msg (EnvLookupError var) =
    "The required variable \"" <> var <> "\" was not specified."
  msg (EnvParseError var) =
    "The variable \"" <> var <> "\" was formatted incorrectly."

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
    :: Proxy e
    -> Object String
    -> Either (List EnvError) (Record r)

instance readEnvImpl ::
  ( RowToList e el
  , RowToList r rl
  , ReadEnvFields el rl r
  , ListToRow rl r
  , ListToRow el l
  ) =>
  ReadEnv e r where
  readEnv _ = readEnvFields (Proxy :: _ el) (Proxy :: _ rl)

-- | Transforms a list of environment variable specifications to a record.
class
  ReadEnvFields (el :: RowList Type) (rl :: RowList Type) (r :: Row Type)
  | el -> rl where
  readEnvFields
    :: Proxy el
    -> Proxy rl
    -> Object String
    -> Either (List EnvError) (Record r)

instance readEnvFieldsCons ::
  ( IsSymbol name
  , IsSymbol name
  , ListToRow rlt rt
  , ReadEnvFields elt rlt rt
  , Row.Lacks name rt
  , Row.Cons name ty rt r
  , ReadValue ty
  ) =>
  ReadEnvFields (Cons name ty elt) (Cons name ty rlt) r where
  readEnvFields _ _ env = insert value tail
    where
    nameP = Proxy :: _ name
    value = readValue (reflectSymbol nameP) env
    tail = readEnvFields (Proxy :: _ elt) (Proxy :: _ rlt) env

    insert (Left valueErr) (Left tailErrs) = Left $ valueErr : tailErrs
    insert valE tailE = Record.insert nameP <$> lmap pure valE <*> tailE

instance readEnvFieldsNil ::
  TypeEquals {} (Record row) =>
  ReadEnvFields Nil Nil row where
  readEnvFields _ _ _ = pure $ to {}
