module TypedEnv
  ( fromEnv
  , Variable
  , EnvError(..)
  , class ParseValue
  , parseValue
  , class ReadEnv
  , readEnv
  , class ReadEnvFields
  , readEnvFields
  ) where

import Prelude
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(Nothing))
import Data.Number (fromString) as Number
import Data.String.CodeUnits (uncons) as String
import Data.String.Common (toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object (Object, lookup)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)
import Record (insert) as Record
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, to)
import Type.RowList (class ListToRow)

fromEnv :: forall e r proxy. ReadEnv e r => proxy e -> Object String -> Either EnvError (Record r)
fromEnv = readEnv

data Variable (name :: Symbol) (ty :: Type)

data EnvError = EnvLookupError String | EnvParseError String

derive instance eqEnvError :: Eq EnvError

derive instance genericEnvError :: Generic EnvError _

instance showEnvError :: Show EnvError where
  show = genericShow

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

class ReadEnv (e :: # Type) (r :: # Type) where
  readEnv :: forall proxy. proxy e -> Object String -> Either EnvError (Record r)

instance readEnvImpl ::
  ( RowToList e el
  , RowToList r rl
  , ReadEnvFields el rl r
  , ListToRow rl r
  , ListToRow el l
  ) => ReadEnv e r where
    readEnv _ = readEnvFields (RLProxy :: RLProxy el) (RLProxy :: RLProxy rl)

class ReadEnvFields (el :: RowList) (rl :: RowList) (r :: # Type) | el -> rl where
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
  , ParseValue ty
  ) => ReadEnvFields (Cons name (Variable varName ty) elt) (Cons name ty rlt) r where
    readEnvFields _ _ env = Record.insert nameP <$> value <*> tail
      where
        nameP = SProxy :: SProxy name
        varName = reflectSymbol (SProxy :: SProxy varName)
        value = note (EnvLookupError varName) (lookup varName env) >>= parseValue >>> note (EnvParseError varName)
        tail = readEnvFields (RLProxy :: RLProxy elt) (RLProxy :: RLProxy rlt) env

instance readEnvFieldsNil :: TypeEquals {} (Record row) => ReadEnvFields Nil Nil row where
  readEnvFields _ _ _ = pure $ to {}
