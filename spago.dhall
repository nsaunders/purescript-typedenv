{ name = "typedenv"
, license = "MIT"
, repository = "https://github.com/nsaunders/purescript-typedenv"
, dependencies =
  [ "either"
  , "foreign-object"
  , "integers"
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
