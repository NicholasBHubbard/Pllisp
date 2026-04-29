# Naming Conventions

All identifiers in pllisp are case-insensitive. These conventions govern how
names are written in source code for readability and consistency.

## Case-Insensitivity in Practice

These all refer to the same name:

```
foo
Foo
FOO
```

That affects both declarations and uses:

```
(let ((count 10))
  (print (int-to-str COUNT)))
```

For that reason, consistent casing matters for readability even though the
language does not require it.

## Recommended Naming Tiers

| Kind | Convention | Examples |
|------|-----------|----------|
| Variables / functions | lowercase kebab-case | `x`, `safe-div`, `find-user` |
| Macros | lowercase kebab-case | `fun`, `when-let`, `progn` |
| Typeclass methods | lowercase kebab-case | `truthy`, `str`, `show` |
| Builtins | lowercase kebab-case | `add`, `eqi`, `int-to-str` |
| Constructors | TitleCase | `Just`, `Nothing`, `Cons`, `Empty` |
| Type names | TitleCase | `List`, `Maybe`, `Person` |
| Typeclass names | UPPER CASE | `TRUTHY`, `SHOW`, `FUNCTOR` |
| Module names | UPPER CASE | `PRELUDE`, `MATH` |
| Primitive types | UPPER CASE | `INT`, `FLT`, `STR`, `BOOL` |
| Uninterned symbols | lowercase | `:ok`, `:error`, `:pending` |

## Why This Style Works

The three tiers make it possible to tell at a glance what kind of thing a
name refers to:

- **lowercase** — values: variables, functions, macros, methods, builtins
- **TitleCase** — type-level constructors: ADT constructors, type names
- **UPPER CASE** — declarations: modules, typeclasses, primitive types

## File and Module Names

For imported modules, follow the module name directly:

```
# File: MATH.pll
(module MATH)
```

In practice:

- imported module files should be named like `MATH.pll`, `DISPLAY.pll`,
  `SHAPES.pll`
- module declarations should use the same module name
- entry files are looser and can still be named things like `main.pllisp`

See [Modules](modules.md) for the full file-resolution rules.

## Field Names and Keyword Names

Record fields and keyword parameters follow the same lowercase style as normal
value names:

```
(type Person ()
  (Person (name %STR) (age %INT)))

(lam (&key (width 80) (height 24))
  (mul width height))
```

## Reserved Words

Some names are reserved and cannot be used in binding or pattern positions.
See [Reference](reference.md#reserved-words) for the exact list.
