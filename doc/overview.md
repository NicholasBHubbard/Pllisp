# Pllisp

Pllisp is a statically typed Lisp with s-expression syntax, procedural macros,
Hindley-Milner type inference, algebraic data types, pattern matching, and
Haskell-style typeclasses.

## What a Program Looks Like

A pllisp program is a sequence of top-level expressions. There is no `main`
function — top-level expressions execute in order.

```
(print "hello, world")
```

Comments start with `#` and run to the end of the line:

```
(print "hi")  # this is a comment
```

Here is a slightly larger program using the PRELUDE `fun` macro and the
built-in `List` type:

```
(fun sum-list ((xs %(List INT)))
  (case xs
    ((Empty) 0)
    ((Cons h t) (add h (sum-list t)))))

(print (int-to-str (sum-list (Cons 1 (Cons 2 (Cons 3 Empty))))))
```

That prints `6`.

## Core Ideas

- **Everything is an expression.** `if`, `case`, `let`, and function calls all
  produce values. There are no statements.

- **Top-level code runs in order.** You can define helpers, types, classes,
  macros, and then run expressions after them.

- **Type annotations are optional most of the time.** You can write them when
  they clarify intent or resolve ambiguity.

- **The PRELUDE is implicit.** Common types like `List` and `Maybe`, plus
  convenience macros like `fun`, `progn`, `when`, and `cond`, are already
  available.

- **Names are case-insensitive.** `sum-list`, `Sum-List`, and `SUM-LIST`
  refer to the same symbol.

## Language Tour

### Expressions and Functions

Functions are values. You build them with `lam` or, more commonly, with the
PRELUDE `fun` macro:

```
(fun double ((x %INT)) (mul x 2))
(print (int-to-str (double 21)))
```

See [Expressions](expressions.md).

### Algebraic Data Types and Pattern Matching

```
(type MaybeInt ()
  (NoInt)
  (HasInt %INT))

(fun describe ((m %MaybeInt))
  (case m
    ((NoInt) "empty")
    ((HasInt n) (concat "value=" (int-to-str n)))))
```

See [Types](types.md).

### Records

Record fields are named at the type declaration site and accessed with
`(.field expr)`:

```
(type Person ()
  (Person (name %STR) (age %INT)))

(let ((p (Person "Alice" 30)))
  (print (.name p)))
```

See [Types](types.md#records).

### Typeclasses

Typeclasses let you write overloaded operations:

```
(cls SHOW () (a)
  (show %a %STR))

(inst SHOW %INT
  (show (lam ((x %INT)) (int-to-str x))))

(print (show 42))
```

See [Typeclasses](typeclasses.md).

### Macros

Macros transform syntax and are especially useful for surface-level language
extensions:

```
(mac unless (cond body)
  `(if_ ,cond unit ,body))
```

See [Macros](macros.md).

### Modules

Modules are imported with `import`, optionally with an alias or unqualified
names:

```
(import MATH)
(print (int-to-str (MATH.square 5)))
```

See [Modules](modules.md).

### C FFI

Pllisp can call C functions and describe C structs directly:

```
(ffi c-sqrt (:link-name "sqrt") (%FLT) %FLT)
(print (flt-to-str (c-sqrt 9.0)))
```

See [FFI](ffi.md).

### Built-In Regex

Regular expressions are first-class values:

```
(let ((digits /[0-9]+/))
  (print (rx-find digits "abc123def")))
```

See [Expressions](expressions.md#regular-expressions).

## Where to Go Next

If you want to write ordinary programs first:

1. Read [Expressions](expressions.md).
2. Read [Types](types.md).
3. Read [Standard Library Overview](stdlib/README.md).

If you are working on larger or more advanced programs:

- [Naming Conventions](conventions.md)
- [Typeclasses](typeclasses.md)
- [Macros](macros.md)
- [Modules](modules.md)
- [FFI](ffi.md)
- [Reference](reference.md)
