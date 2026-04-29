# Naming Conventions

All identifiers in pllisp are case-insensitive. These conventions govern how
names are written in source code for readability and consistency.

## Three Tiers

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

## Rationale

The three tiers make it possible to tell at a glance what kind of thing a
name refers to:

- **lowercase** — values: variables, functions, macros, methods, builtins
- **TitleCase** — type-level constructors: ADT constructors, type names
- **UPPER CASE** — declarations: modules, typeclasses, primitive types
