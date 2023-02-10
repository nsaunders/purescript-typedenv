{ name = "typedenv"
, license = "MIT"
, repository = "https://github.com/nsaunders/purescript-typedenv"
, dependencies =
  [ "either"
  , "foreign-object"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "prelude"
  , "record"
  , "strings"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
