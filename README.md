_Note: README is a WIP; things are broken!_

# purescript-typedenv [![build status](https://img.shields.io/travis/nsaunders/purescript-typedenv.svg)](https://travis-ci.org/nsaunders/purescript-typedenv) [![purescript-typedenv on Pursuit](https://pursuit.purescript.org/packages/purescript-typedenv/badge)](https://pursuit.purescript.org/packages/purescript-typedenv) [![typedenv in package-sets](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-0lf69kxs4fbd.runkit.sh/typedenv)](https://github.com/purescript/package-sets)
## Parse environment variables using a type-level schema encoding.

<img src="https://raw.githubusercontent.com/nsaunders/purescript-typedenv/master/img/tile.png" alt="purescript-typedenv" align="right" />

The [`purescript-node-process` environment API](https://pursuit.purescript.org/packages/purescript-node-process/7.0.0/docs/Node.Process#v:getEnv)
provides environment variables in the form of an
[`Object String`](https://pursuit.purescript.org/packages/purescript-foreign-object/2.0.2/docs/Foreign.Object#t:Object)
(a string map), but it is left up to us to validate and to parse the values into some configuration model that can be used
throughout the rest of the program.

Perhaps one of the more elegant solutions would be something like this applicative-style lookup/validation/parsing into a
record:

```purescript
type Config =
  { greeting :: String
  , count :: Int
  }

readConfig :: Object String -> Either String Config
readConfig env = { greeting: _, count: _ }
  <$> value "GREETING"
  <*> (value "COUNT" >>= Int.fromString >>> note "Invalid COUNT")

  where
    value name =
      note ("Missing variable " <> name) $ lookup name env
```

However, this is still a bit unsatisfying. For one thing, the explicit lookups, parsing logic, and error handling are a bit tedious and might start to look like boilerplate as the `Config` model is extended with additional fields.
