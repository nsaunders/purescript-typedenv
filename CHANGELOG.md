# Changelog

Notable changes are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [2.0.1] - 2023-02-13

Other improvements:
- Minor fix to field names in README example (#11 by @nsaunders)

## [2.0.0] - 2023-02-10

Breaking changes:
- Updated to v0.15 of the compiler, dropping support for previous versions. (#5 by @nsaunders)
- New API drops the `<:` operator. Instead, `fromEnv` looks up the environment variable name corresponding to each row label. (#6 by @nsaunders)
- Support for accumulating multiple errors (#3 by @johncowie)
- Accumulate errors in a `List EnvError`, eliminating the need for an `EnvErrors` constructor. (#8 by @nsaunders)
- Update `fromEnv` to accept a record type instead of a row type (#10 by @nsaunders)

Other improvements:
- Better performance via Record Builder (#9 by @nsaunders)
- Updated code examples in the README (#7 and [a9eaa73](https://github.com/nsaunders/purescript-typedenv/commit/a9eaa7369e61a3e2b0606a3aac21787ff64b4e52) by @nsaunders)
- Clarify source module for `note` function (#1 by @srghma)

## [1.0.0] - 2021-03-27

Breaking changes:
- Updated to v0.14 of the compiler, dropping support for previous versions. ([452b2b8](https://github.com/nsaunders/purescript-typedenv/commit/452b2b81d58e7b5e8daee288606663a7a554268e) by @nsaunders)

Other improvements:
- Various documentation updates.

## [0.0.1] - 2019-09-07

Initial release
