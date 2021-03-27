{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "foreign-object"
    , "integers"
    , "node-process"
    , "numbers"
    , "psci-support"
    , "record"
    , "spec"
    , "strings"
    , "transformers"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
