# Modules

Pllisp supports multi-file programs through explicit imports.

Modules carry both:

- runtime definitions such as top-level `let` bindings, constructors, and
  typeclass methods
- compile-time definitions such as macros and macro helper bindings

## Basic Shape

A typical layout looks like this:

```text
main.pllisp
MATH.pll
```

`main.pllisp`:

```lisp
(import MATH)
(print (int-to-str (MATH.square 5)))
```

`MATH.pll`:

```lisp
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

```lisp
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

```lisp
(import MATH)              # qualified access only: MATH.square
(import MATH (square))     # also bring square into scope
(import MATH M)            # alias: M.square
(import MATH M (square))   # alias + unqualified import
```

### Qualified Access

```lisp
(import MATH)
(print (int-to-str (MATH.square 5)))
```

### Aliased Access

```lisp
(import MATH M)
(print (int-to-str (M.square 5)))
```

If you use an alias, that alias replaces the original qualifier in that
program. `MATH.square` is not in scope there.

### Unqualified Imports

```lisp
(import HELPER (double triple))
(print (int-to-str (double 5)))
(print (int-to-str (triple 5)))
```

Unqualified import lists are validated. If you request a name the module does
not export, that is an error even if you never use the bad name.

## Runtime Exports

There is no explicit runtime export list.

Currently exported automatically at runtime:

- top-level `let` bindings, except `_`
- data constructors from `type` declarations
- typeclass methods

Not currently exported across modules:

- top-level `ffi`, `ffi-var`, `ffi-struct`, `ffi-enum`, and `ffi-callback`
  declarations
- declaration names like the type name itself or the class name itself

This means a module can export `Circle` and `Rect`, but not the type name
`Shape` itself as an importable declaration name.

## Macros and Compile-Time Imports

Modules also export compile-time definitions.

Currently exported automatically at compile time:

- top-level `mac` definitions
- top-level `let` bindings inside `eval-when (:compile-toplevel ...)`

That means a module can be a real macro library, not just a bag of runtime
functions.

### Simple Macro Module

`MACROS.pll`:

```lisp
(module MACROS)

(mac double (x)
  `(add ,x ,x))
```

`main.pllisp`:

```lisp
(import MACROS)

(print (int-to-str (double 21)))
```

### Macro Module With Helper Library

`BUILDERS.pll`:

```lisp
(module BUILDERS)

(eval-when (:compile-toplevel)
  (let ((emit-double-print
          (lam (expr)
            `(print (int-to-str (add ,expr ,expr))))))
    emit-double-print))
```

`MACROS.pll`:

```lisp
(module MACROS)
(import BUILDERS)

(mac show-double (expr)
  (emit-double-print expr))
```

`main.pllisp`:

```lisp
(import MACROS)

(show-double 21)
```

That pattern is supported directly. Imported modules bring their compile-time
state with them, including helper bindings used by later macros.

### Transitive Macro Imports Work

If module `A` exports a macro, and module `B` imports `A` and defines another
macro using it, importing `B` is enough. You do not need to re-import `A` in
the final program.

## How Macro Imports Behave

Macro imports are not namespaced the same way runtime values are.

Important rules:

- imported macros are called by bare name, not by qualified name
- module aliases affect runtime qualified names, not macro call syntax
- unqualified import lists control runtime names, not whether macros are loaded

So this works:

```lisp
(import MOD M)
(double 21)
```

and this is not the model to expect:

```lisp
(import MOD M)
(M.double 21)   # do not expect macro calls to work this way
```

Similarly, this still loads the module’s macros:

```lisp
(import MOD (square))
```

even though only `square` is being brought into runtime scope unqualified.

Bluntly: runtime import controls and compile-time macro loading are related,
but they are not the same interface.

## Compile-Time Name Collisions

Imported compile-time names share one compile-time namespace.

That means:

- importing two modules that both export the same macro name is an error
- importing two modules that both export the same compile-time helper binding
  name is also an error

Example collision:

```lisp
(import A)
(import B)
```

if both `A` and `B` export a macro named `double`.

## Importing Constructors and Typeclass Methods

Constructors can be imported directly:

```lisp
(import SHAPES (Circle Rect area))
(print (int-to-str (area (Circle 5))))
```

Typeclass methods can also be imported directly:

```lisp
(import DISPLAY (display))
(print (display 42))
```

## Runtime Name Collisions

If the same unqualified runtime name is imported from multiple modules, you
get a collision error:

```lisp
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

```lisp
(module MATH)
(import UTIL (double))

(fun square ((x %INT))
  (mul x x))
```

Because imports are pre-scanned, imported macros are available throughout the
file even if the textual `import` appears later. Local macro definitions are
still order-sensitive within the file.

## The PRELUDE

The `PRELUDE` module is implicitly available in every program. It provides:

- standard types like `List`, `Maybe`, `Either`, `Pair`, and `Unit`
- convenience macros like `fun`, `progn`, `if_`, `when`, `unless`, and `cond`
- core typeclasses like `TRUTHY`, `EQ`, `ORD`, and `STRINGY`
- compile-time helper functions used by macros, such as `append`, `reverse`,
  `map`, `filter`, and `foldl`

Do not write an explicit `(import PRELUDE)`. `PRELUDE` is already available,
and an explicit import currently fails with duplicate PRELUDE macro
definitions.

Also avoid redefining PRELUDE names at top level. Colliding with exported
PRELUDE symbols or PRELUDE macro names is an error.

## Practical Advice

- keep imported support files named exactly like their module name
- put `module` and `import` forms near the top even though the language does
  not force it
- use ordinary modules for runtime code, macro modules for syntax extensions,
  and `eval-when (:compile-toplevel)` when a macro library needs helper code
- prefer qualified imports for larger runtime modules
- expect macro names to live in one shared compile-time namespace
- do not rely on FFI declarations exporting across modules

For the details of writing macros themselves, see [Macros](macros.md).
