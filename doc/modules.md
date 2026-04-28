# Modules

## Declaring a Module

The `module` declaration must be the first form in the file, and the name
must match the filename (case-insensitive):

```
# File: MATH.pll
(module MATH)

(let ((square (lam ((x %INT)) (mul x x))))
  unit)
```

## Imports

```
(import MATH)              # qualified access only: MATH.square
(import MATH (square))     # also bring square into scope
(import MATH M)            # alias: M.square
(import MATH M (square))   # alias + unqualified
```

Import declarations must appear after the `module` declaration and before any
other code.

## Exports

Everything defined at the top level is exported automatically: `let` bindings
(except `_`), type constructors, and typeclass methods. There is no explicit
export list.

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

See [Standard Library](stdlib.md) for full details.
