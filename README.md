# purescript-typedenv [![Build](https://github.com/nsaunders/purescript-typedenv/workflows/CI/badge.svg)](https://github.com/nsaunders/purescript-typedenv/actions/workflows/ci.yml) [![Latest release](http://img.shields.io/github/release/nsaunders/purescript-typedenv.svg)](https://github.com/nsaunders/purescript-typedenv/releases) [![PureScript registry](https://img.shields.io/badge/dynamic/json?color=informational&label=registry&query=%24.typedenv.version&url=https%3A%2F%2Fraw.githubusercontent.com%2Fpurescript%2Fpackage-sets%2Fmaster%2Fpackages.json)](https://github.com/purescript/registry) [![purescript-typedenv on Pursuit](https://pursuit.purescript.org/packages/purescript-typedenv/badge)](https://pursuit.purescript.org/packages/purescript-typedenv)
## Parse environment variables according to a type-level specification.

<img src="https://raw.githubusercontent.com/nsaunders/purescript-typedenv/master/img/tile.png" alt="purescript-typedenv" align="right" />

The [`purescript-node-process` environment API](https://pursuit.purescript.org/packages/purescript-node-process/7.0.0/docs/Node.Process#v:getEnv)
provides environment variables in the form of an
[`Object String`](https://pursuit.purescript.org/packages/purescript-foreign-object/2.0.2/docs/Foreign.Object#t:Object)
(a string map), but it is left up to us to validate and to parse the values into some configuration model that can be used
throughout the rest of the program.

Perhaps one of the more popular solutions would be something like this applicative-style lookup/validation/parsing
into a record:

```purescript
type Config =
  { greeting :: String
  , count    :: Int
  }

readConfig :: Object String -> Either String Config
readConfig env = { greeting: _, count: _ }
  <$> value "GREETING"
  <*> (value "COUNT" >>= Int.fromString >>> note "Invalid COUNT")
  where
    value name =
      note ("Missing variable " <> name) $ lookup name env
```

However, this is a bit unsatisfying. For one thing, the explicit lookups, parsing logic, and error handling are somewhat
verbose and might start to look like a lot of boilerplate as the `Config` model is extended with additional fields. Second,
multiple non-consecutive lines of code would need to be touched in order to add a new setting. Third, it may not be
immediately clear at a glance what environment variables are required, their types, or their relationships to the `Config`
model (i.e. which variable corresponds to each field).

This library attempts to address these issues by extending a configuration model like the above with the small amount of
additional information required to read it from the environment⁠—that is, the name of the environment variable corresponding
to each field. For example:

```purescript
type Config =
  ( "GREETING" :: String
  , "COUNT"    :: Int
  )
```

Its `fromEnv` function can now read the configuration from the environment with relative ease:

```purescript
readConfig env =
  bimap
    envErrorMessage
    (\r -> { greeting: r."GREETING", count: r."COUNT" })
    $ TypedEnv.fromEnv (Proxy :: _ Config) env
```

For more, see the [examples](#examples) section below.

### Installation

via [spago](https://github.com/spacchetti/spago):
```bash
spago install typedenv
```

### Examples

To run one of the [examples](example), clone the repository and run the following command, replacing `<example-name>` with the name of the example.

```bash
spago run -p example/<example-name>.purs -m Example.<example-name>
```

### Similar ideas
* [TypedEnv](https://github.com/freight-hub/TypedEnv)
* [dotenv-hs](https://github.com/stackbuilders/dotenv-hs) can validate variables against a `.schema.yml` file.
