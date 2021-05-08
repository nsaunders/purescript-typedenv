{ name = "typedenv"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "maybe"
  , "node-process"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "record"
  , "spec"
  , "strings"
  , "transformers"
  , "type-equality"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
