# Modules

Pllisp supports multi-file programs through explicit imports.

## Basic Shape

A typical module setup looks like this:

```text
main.pllisp
MATH.pll
```

`main.pllisp`:

```
(import MATH)
(print (int-to-str (MATH.square 5)))
```

`MATH.pll`:

```
(module MATH)

(fun square ((x %INT))
  (mul x x))
```

Important distinction:

- the entry file can have any filename you pass to the CLI
- imported support files must be named exactly `MODULE.pll`

On a case-sensitive filesystem, `(import MATH)` looks for `MATH.pll`, not
`math.pll`.

## Declaring a Module

Use:

```
(module MATH)
```

A file may place its `module` declaration anywhere, but keeping it at the top
is the least confusing style.

For the main entry file, if you do write a `module` declaration, its name must
match the file basename case-insensitively.

Examples:

- `Foo.pll` may declare `(module FOO)`
- `src/Foo.pll` may declare `(module FOO)`
- `Bar.pll` may not declare `(module FOO)`

## Imports

Imports may appear anywhere in a file, but keeping them grouped near the top
is recommended.

Supported forms:

```
(import MATH)              # qualified access only: MATH.square
(import MATH (square))     # also bring square into scope
(import MATH M)            # alias: M.square
(import MATH M (square))   # alias + unqualified import
```

### Qualified Access

```
(import MATH)
(print (int-to-str (MATH.square 5)))
```

### Aliased Access

```
(import MATH M)
(print (int-to-str (M.square 5)))
```

If you use an alias, that alias replaces the original qualifier in that
program. `MATH.square` is not in scope there.

### Unqualified Imports

```
(import HELPER (double triple))
(print (int-to-str (double 5)))
(print (int-to-str (triple 5)))
```

Unqualified import lists are validated. If you request a name the module does
not export, that is an error even if you never use the bad name.

## What Gets Exported

There is no explicit export list.

Currently exported automatically:

- top-level `let` bindings, except `_`
- data constructors from `type` declarations
- typeclass methods

Not currently exported across modules:

- top-level `ffi`, `ffi-var`, `ffi-struct`, `ffi-enum`, and `ffi-callback`
  declarations
- declaration names like the type name itself or the class name itself

This means a module can export `Circle` and `Rect`, but not the type name
`Shape` itself as an importable declaration name.

## Importing Constructors and Typeclass Methods

Constructors can be imported directly:

```
(import SHAPES (Circle Rect area))
(print (int-to-str (area (Circle 5))))
```

Typeclass methods can also be imported directly:

```
(import DISPLAY (display))
(print (display 42))
```

## Collisions

If the same unqualified name is imported from multiple modules, you get a
collision error:

```
(import FOO (helper))
(import BAR (helper))
```

In that situation, keep the imports qualified or use aliases.

## Search Rules

When resolving an import, pllisp looks:

1. in the current directory of the importing file
2. in the stdlib directory

That makes sibling-module layouts work naturally:

```text
app/main.pllisp
app/MATH.pll
app/UTIL.pll
```

## `module` and `import` Placement

The frontend scans the whole file for `module` and `import` forms. It does not
require a strict header block, but writing one is still the sane style:

```
(module MATH)
(import UTIL (double))

(fun square ((x %INT))
  (mul x x))
```

## The PRELUDE

The `PRELUDE` module is implicitly available in every program. It provides:

- standard types like `List`, `Maybe`, `Either`, `Pair`, and `Unit`
- convenience macros like `fun`, `progn`, `if_`, `when`, `unless`, and `cond`
- core typeclasses like `TRUTHY`, `EQ`, `ORD`, and `STRINGY`

Do not write an explicit `(import PRELUDE)`. `PRELUDE` is already available,
and an explicit import currently fails with duplicate PRELUDE macro
definitions.

Also avoid redefining PRELUDE names at top level. Colliding with exported
PRELUDE symbols or PRELUDE macro names is an error.

## Practical Advice

- keep imported support files named exactly like their module name
- put `module` and `import` forms near the top even though the language does
  not force it
- prefer qualified imports for larger modules
- use unqualified imports sparingly and intentionally
- do not rely on FFI declarations exporting across modules

See [Standard Library](stdlib.md) for what `PRELUDE` already brings into scope.
