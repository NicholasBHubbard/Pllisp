# Modules

## Declaring a Module

The compiler scans the whole file for a `module` declaration. It does not
enforce that `module` is the first form, but keeping it at the top is the
least confusing style.

For the file being compiled directly, the declared module name must match the
file basename case-insensitively:

```
# File: MATH.pll
(module MATH)

(let ((square (lam ((x %INT)) (mul x x))))
  unit)
```

## Imports

The compiler scans the whole file for `import` declarations. It does not
enforce that they appear immediately after `module`, but keeping them grouped
near the top is recommended.

```
(import MATH)              # qualified access only: MATH.square
(import MATH (square))     # also bring square into scope
(import MATH M)            # alias: M.square
(import MATH M (square))   # alias + unqualified
```

Imported modules are resolved as exact `MODULE.pll` filenames in the current
directory first, then in the stdlib directory. On a case-sensitive filesystem,
`(import MATH)` looks for `MATH.pll`, not `math.pll`.

The main entry file may use whatever filename you pass to the CLI. Imported
support files must be named `MODULE.pll`.

If you use an alias, that alias replaces the original qualifier:

```
(import MATH M)
(print (int-to-str (M.square 5)))   # ok
```

`MATH.square` is not in scope in that program.

Unqualified import lists are validated. If you request a name the module does
not export, compilation fails.

## Exports

There is no explicit export list.

Currently exported automatically:

- top-level `let` bindings, except `_`
- data constructors from `type` declarations
- typeclass methods

Not currently exported across modules:

- top-level `ffi`, `ffi-var`, `ffi-struct`, `ffi-enum`, and `ffi-callback`
  declarations
- declaration names like the type name itself or the class name itself

## Qualified Access

Imported names are accessed through their module name or alias:

```
(import MATH M)
(print (int-to-str (M.square 5)))
```

If the same name is unqualified from multiple modules, the compiler reports
a collision error.

## The PRELUDE

The `PRELUDE` module is implicitly imported into every program. It provides
the standard types (`List`, `Maybe`, `Either`, `Pair`, `Unit`), control flow
macros (`fun`, `progn`, `if_`, `when`, `unless`, `cond`, etc.), and
typeclasses (`TRUTHY`, `STRINGY`).

Do not write an explicit `(import PRELUDE)`. The compiler already injects it,
and an explicit import currently fails with duplicate PRELUDE macro
definitions.

See [Standard Library](stdlib.md) for full details.
