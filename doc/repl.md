# REPL

Pllisp ships with an interactive REPL:

```sh
pllisp repl
```

If you are running from a checkout instead of an installed binary, use:

```sh
cabal run pllisp -- repl
```

You can also preload a file before the session starts:

```sh
pllisp repl path/to/file.pllisp
```

That compiles the file, runs its top-level forms, and then leaves you in the
same session with its bindings, macros, types, and imports available.

## Entering Code

Normal input is parsed exactly like ordinary pllisp source. Top-level bindings,
types, classes, instances, macros, and mutable references persist across
entries.

```lisp
(fun square ((x %INT)) (mul x x))
(print (int-to-str (square 9)))
```

Multi-line input works. The REPL waits until it has a complete form before it
tries to compile and run anything.

The REPL does not automatically print arbitrary values. Use `print` when you
want runtime output, and use `:type` when you want to inspect a type.

## Commands

`:` commands are handled by the REPL itself:

| Command | Meaning |
|---------|---------|
| `:help` | show the command summary |
| `:quit` | exit the session |
| `:load FILE` | compile and run a file in the current session |
| `:reload` | reload the most recent file loaded with `pllisp repl FILE` or `:load FILE` |
| `:reset` | start over from a fresh session with only the implicit `PRELUDE` |
| `:type EXPR` | infer and print the type of an expression |
| `:macroexpand FORM` | show the expanded form without running it |

Examples:

```lisp
:type (Just 1)
:macroexpand (when true (print "ok"))
:load example-programs/valid/typeclasses.pllisp
```

## Imports and Files

The REPL uses the normal compiler pipeline. `(import MODULE)` works the same
way it does in regular source files, including aliases and unqualified imports.
Module files still follow the normal path rule: `MATH` loads `MATH.pll`, and
hierarchical module paths like `FOO.BAR.BAZ` load `FOO/BAR/BAZ.pll`.

Use `:load FILE` when you want to bring in a whole script with ordinary
top-level forms. Use `(import MODULE)` when you want module-style dependencies
inside the code you are typing.

## Errors

Parse, macro-expansion, resolve, and type errors are reported immediately. A
failed entry does not partially update the session state.
