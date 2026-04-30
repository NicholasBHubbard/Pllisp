# Standard Library

The standard library now has its own documentation section.

Read this page first, then jump to the specific module you need.

## Modules

- [PRELUDE](PRELUDE.md) — the implicit standard module: core data types,
  convenience macros, and built-in typeclasses
- [CLI](CLI.md) — the top-level command-line DSL
- [FILEIO](FILEIO.md) — file, directory, and handle-oriented I/O

## Notes

`PRELUDE` is implicitly available in every program.

You do not need an explicit `(import PRELUDE)`.

Do not write one anyway. `PRELUDE` is already available, and an explicit
`(import PRELUDE)` currently fails with duplicate macro definitions.

If you are new to the language, start with:

1. [PRELUDE](PRELUDE.md)
2. [CLI](CLI.md) if you need command-line parsing
3. [FILEIO](FILEIO.md) if you need filesystem access
