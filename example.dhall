let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/**/*.purs" ],
  dependencies = conf.dependencies # 
    [ "console"
    , "effect"
    , "foldable-traversable"
    , "lists"
    , "node-process"
    , "transformers"
    , "type-equality"
    ]
}
