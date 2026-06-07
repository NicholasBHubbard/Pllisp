# Pllisp

A statically typed Lisp programming language.

This project is currently in development.

## Key Features

- Lisp language
- Static typing
- Type inference
- Algebraic data types
- Higher-kinded types
- Record types
- Pattern matching
- Typeclasses
- Tail call optimization
- Hygienic macro system
- Mutable references
- Module system
- Built-in regular expressions
- C FFI
- Compiles to LLVM
- Standard library

## Future Work

- [ ] Documentation system
- [ ] Strong unicode support
- [ ] Heredoc strings
- [ ] Add an optimization phase to compilation
- [ ] Sophisticated emacs support similar to slime
- [ ] Add `unwind-protect`
- [ ] Comprehensive test library
- [ ] Rewrite entire compiler in Pllisp

## Getting Started

### Requirements

- GHC and `cabal`
- `clang`
- native libraries for `pcre2`, `libgc`, and `libffi`

### Build

```sh
cabal build
```

### Build a Program

```sh
cabal run pllisp -- example-programs/valid/hello.pllisp
./example-programs/valid/hello
```

This writes the executable next to the input file, using the same basename.

### Start the REPL

```sh
cabal run pllisp -- repl
```

You can preload a file into the session:

```sh
cabal run pllisp -- repl example-programs/valid/typeclasses.pllisp
```

The entry file can use any filename you pass on the command line. Imported
modules are different: they are resolved as exact `MODULE.pll` filenames.

### Try the Example Programs

The repository already includes runnable examples:

- `example-programs/valid/` — small working programs
- `example-programs/invalid/` — programs that should fail with an error
- `example-programs/modules/` — multi-file module examples

Good starting points:

- `example-programs/valid/hello.pllisp`
- `example-programs/valid/prelude-macros.pllisp`
- `example-programs/valid/typeclasses.pllisp`
- `example-programs/valid/records.pllisp`
- `example-programs/modules/valid/qualified-access/main.pllisp`

Imported support files must use the module filename rule. For example, a main
file can be named `main.pllisp`, but an imported `MATH` module must live in
`MATH.pll`.

### Run Tests

```sh
cabal test pllisp-test
```

## Manual

Start with [Overview](doc/overview.md), then use the docs under [`doc/`](doc):

- [Naming Conventions](doc/conventions.md) — case rules, naming style, and
  how case-insensitivity affects source code
- [Expressions](doc/expressions.md) — literals, bindings, functions,
  conditionals, references, I/O, and regular expressions
- [Types](doc/types.md) — annotations, inference, ADTs, records, patterns,
  and uninterned symbols
- [Typeclasses](doc/typeclasses.md) — classes, instances, superclasses,
  parametric instances, and higher-kinded classes
- [Macros](doc/macros.md) — `mac`, quasiquote, `eval-when`, helper bindings,
  `%SYNTAX`, typed compile-time helpers, module macro libraries, and
  automatic hygiene
- [Modules](doc/modules.md) — file layout, runtime imports, macro imports,
  aliases, compile-time exports, and PRELUDE behavior
- [REPL](doc/repl.md) — interactive sessions, REPL commands, file loading,
  multiline input, and type or macro inspection
- [FFI](doc/ffi.md) — calling C functions, structs, enums, arrays,
  variadics, and callbacks
- [Standard Library Overview](doc/stdlib/README.md) — the stdlib docs index
- [PRELUDE](doc/stdlib/PRELUDE.md) — the implicit standard module:
  built-in types, convenience macros, and core typeclasses
- [APPLICATIVE](doc/stdlib/APPLICATIVE.md) — applicative typeclasses,
  `pure`, and `ap`
- [MONAD](doc/stdlib/MONAD.md) — monadic `bind` and `do-let`
- [TRAVERSABLE](doc/stdlib/TRAVERSABLE.md) — effectful structure traversal
  with `traverse`
- [CLI](doc/stdlib/CLI.md) — the top-level command-line DSL
- [FILEIO](doc/stdlib/FILEIO.md) — filesystem and handle-oriented I/O
- [Reference](doc/reference.md) — quick syntax and builtin cheat sheet

If you are new to the language, the fastest path is:

1. Read [Overview](doc/overview.md).
2. Read [Expressions](doc/expressions.md) and [Types](doc/types.md).
3. Skim [Standard Library Overview](doc/stdlib/README.md).
4. Come back to [Typeclasses](doc/typeclasses.md), [Macros](doc/macros.md),
   [Modules](doc/modules.md), and [FFI](doc/ffi.md) as needed.
