{ name =
    "typedenv"
, dependencies =
    [ "console"
    , "effect"
    , "foreign-object"
    , "generics-rep"
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
