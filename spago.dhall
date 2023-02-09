{ name = "typedenv"
, license = "MIT"
, repository = "https://github.com/nsaunders/purescript-typedenv"
, dependencies =
  [ "bifunctors"
  , "either"
  , "foreign-object"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "prelude"
  , "record"
  , "strings"
  , "type-equality"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
