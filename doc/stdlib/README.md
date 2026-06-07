# Standard Library

The standard library now has its own documentation section.

Read this page first, then jump to the specific module you need.

## Modules

- [PRELUDE](PRELUDE.md) — the implicit standard module: core data types,
  convenience macros, and built-in typeclasses
- [APPLICATIVE](APPLICATIVE.md) — applicative typeclasses, `pure`, and `ap`
- [MONAD](MONAD.md) — monadic `bind` and `do-let`
- [TRAVERSABLE](TRAVERSABLE.md) — effectful structure traversal with `traverse`
- [CLI](CLI.md) — the top-level command-line DSL
- [FILEIO](FILEIO.md) — file, directory, and handle-oriented I/O

## Notes

`PRELUDE` is implicitly available in every program.

You do not need an explicit `(import PRELUDE)`.

Do not write one anyway. `PRELUDE` is already available, and an explicit
`(import PRELUDE)` currently fails with duplicate macro definitions.

If you are new to the language, start with:

1. [PRELUDE](PRELUDE.md)
2. [APPLICATIVE](APPLICATIVE.md), [MONAD](MONAD.md), and
   [TRAVERSABLE](TRAVERSABLE.md) if you need typed effect composition
3. [CLI](CLI.md) if you need command-line parsing
4. [FILEIO](FILEIO.md) if you need filesystem access
