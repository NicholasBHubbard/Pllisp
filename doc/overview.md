# Pllisp

Pllisp is a statically typed Lisp that compiles to LLVM. It combines Lisp's
s-expression syntax and procedural macro system with Hindley-Milner type
inference, algebraic data types, pattern matching, and Haskell-style
typeclasses.

## Quick Taste

A pllisp program is a sequence of top-level expressions. There is no `main`
function — top-level expressions execute in order.

```
(print "hello, world")
```

A more involved example — define a list type, sum its elements, print the
result:

```
(type List (a)
  (Empty)
  (Cons a %(List a)))

(let ((sum (lam ((xs %(List INT)))
      (case xs
        ((Empty) 0)
        ((Cons h t) (add h (sum t)))))))
  (print (int-to-str (sum (Cons 1 (Cons 2 (Cons 3 Empty)))))))
```

## Key Features

- **Static typing with inference** — Hindley-Milner type inference means you
  rarely write type annotations, but the compiler catches type errors at
  compile time. See [Types](types.md).

- **Algebraic data types and pattern matching** — Define sum and product types,
  destructure them with `case`, and the compiler checks exhaustiveness.
  See [Types](types.md#algebraic-data-types).

- **Typeclasses** — Haskell-style ad-hoc polymorphism with superclasses,
  parametric instances, and higher-kinded types. Compiled to dictionary
  passing. See [Typeclasses](typeclasses.md).

- **Procedural macros** — Common Lisp-style macros that transform syntax before
  type checking, with quasiquoting and a full macro-time interpreter.
  See [Macros](macros.md).

- **Module system** — Separate compilation with imports, qualified access, and
  automatic exports. See [Modules](modules.md).

- **C FFI** — Call C functions, define struct layouts, enums, and callbacks
  directly. See [FFI](ffi.md).

- **Standard library** — The PRELUDE provides common types (`List`, `Maybe`,
  `Either`), control flow macros (`progn`, `when`, `cond`), and typeclasses
  (`TRUTHY`, `STRINGY`). See [Standard Library](stdlib.md).

- **Built-in regex** — First-class regular expressions with literal syntax and
  PCRE2 support. See [Expressions](expressions.md#regular-expressions).

- **Tail call optimization** — Self-recursive tail calls compile to loops
  automatically. See [Expressions](expressions.md#tail-call-optimization).

## Conventions

All identifiers are case-insensitive. `foo`, `Foo`, and `FOO` refer to the
same name. The compiler uppercases everything internally. By convention:
lowercase for values/functions, TitleCase for constructors/types, UPPER CASE
for modules/typeclasses/primitives. See [Naming Conventions](conventions.md).

Comments start with `#`:

```
(print "hi")  # this is a comment
```

## Documentation

- [Naming Conventions](conventions.md) — Capitalization and naming rules
- [Expressions](expressions.md) — Literals, bindings, functions, conditionals,
  references, regex
- [Types](types.md) — Type annotations, inference, ADTs, records, pattern
  matching
- [Typeclasses](typeclasses.md) — Classes, instances, superclasses, HKTs
- [Macros](macros.md) — Macro system, quasiquoting, macro-time builtins
- [Modules](modules.md) — Module system, imports, exports
- [FFI](ffi.md) — Foreign function interface
- [Standard Library](stdlib.md) — PRELUDE types, macros, typeclasses
- [Reference](reference.md) — Reserved words, syntax summary, all builtins
