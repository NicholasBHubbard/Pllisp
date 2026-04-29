# Types

Pllisp uses Hindley-Milner type inference. Most code needs no type annotations
at all — types are inferred from usage. Annotations are available when you
want them, using the `%` prefix.

## Big Picture

Pllisp has:

- primitive types like `%INT`, `%FLT`, `%STR`, and `%BOOL`
- function types like `%(-> INT INT BOOL)`
- parameterized types like `%(List INT)` and `%(Maybe STR)`
- user-defined algebraic data types
- record-style constructor fields

## Type Annotations

The `%` character introduces a type expression anywhere one is expected:

```
%INT                     # primitive type
%(List INT)              # parameterized type
%(-> INT INT BOOL)       # function type: two INTs to BOOL
%(Maybe (List STR))      # nested parameterized type
```

Annotations appear in lambda parameters, let bindings, and lambda return types:

```
(lam ((x %INT) (y %INT)) (add x y))
(let ((name %STR "Alice")) name)
(lam ((x %INT)) %STR (int-to-str x))
```

## Primitive Types

| Type | Description | Size |
|------|-------------|------|
| `%INT` | Signed integer | 64-bit |
| `%FLT` | Floating point | 64-bit |
| `%STR` | String | pointer |
| `%BOOL` | Boolean | 1-bit |
| `%UNIT` | Unit (no meaningful value) | -- |
| `%RX` | Regular expression | pointer |
| `%USYM` | Uninterned symbol | pointer |

The `%` prefix is only part of type syntax. Values themselves are written
without `%`:

```
42          # value of type %INT
"hello"     # value of type %STR
true        # value of type %BOOL
```

## Type Inference

Types are inferred through unification. You rarely need annotations:

```
# Inferred: double : INT -> INT
(let ((double (lam (x) (mul x 2))))
  (double 21))
```

Annotations are most useful for:

- numeric ambiguity
- clearer function signatures
- documenting polymorphic code

```
# Without annotation, the type is ambiguous
(let ((zero %INT 0)) zero)
```

## Algebraic Data Types

Types are defined with `type`. Each declaration gives a type name, zero or more
type parameters, and one or more constructors.

```
(type Maybe (a)
  (Nothing)
  (Just a))

(type List (a)
  (Empty)
  (Cons a %(List a)))

(type Either (a b)
  (Left a)
  (Right b))
```

Constructors are functions:

- `Nothing : Maybe a`
- `Just : a -> Maybe a`
- `Cons : a -> List a -> List a`

### Nullary and Multi-Field Constructors

Constructors can take zero, one, or many arguments:

```
(type Color ()
  (Red)
  (Green)
  (Blue))

(type Shape ()
  (Circle %INT)
  (Rect %INT %INT))
```

Using them:

```
Red
(Circle 5)
(Rect 3 4)
```

### Parameterized Types

Type parameters are written in the second list:

```
(type Box (a)
  (Box a))

(type PairBox (a b)
  (PairBox a b))
```

Then use them in annotations:

```
(let ((b %(Box INT) (Box 42)))
  b)
```

### Records

Constructor fields can be named. Named fields enable `.field` access:

```
(type Person ()
  (Person (name %STR) (age %INT)))

(let ((p (Person "Alice" 30)))
  (progn
    (print (.name p))
    (print (int-to-str (.age p)))))
```

Field access is prefix syntax:

```
(.name p)
(.age p)
(.val (.val boxed))
```

Records still construct positionally:

```
(Person "Alice" 30)
```

They also still pattern-match positionally:

```
(case p
  ((Person n a) (print (concat n (concat ":" (int-to-str a))))))
```

Current limits:

- no record-update syntax
- fields belong to the constructor/type that defined them
- field names are not globally shared labels

So this is valid:

```
(type Box (a)
  (Box (val a)))

(.val (Box 42))
```

But accessing a field on the wrong type is an error.

## Pattern Matching

`case` destructures values by pattern:

```
(case maybe-value
  ((Just x) (print (int-to-str x)))
  ((Nothing) (print "nothing")))
```

Patterns can be:

- **Wildcard**: `_`
- **Variable**: `x`
- **Literal**: `42`, `3.14`, `"hello"`, `true`, `false`, `:foo`
- **Constructor**: `(Just x)`, `(Cons h t)`, `(Pair a b)`

Patterns can nest:

```
(case list-of-pairs
  ((Cons (Pair k v) rest) (print k))
  ((Empty) (print "empty")))
```

Pattern variables introduce new names:

```
(case 42
  (x (print (int-to-str x))))
```

### Exhaustiveness Checking

`case` expressions must cover all constructors. This is an error:

```
# Error: non-exhaustive patterns, missing: Nothing
(case maybe-val
  ((Just x) x))
```

Add a wildcard or cover all cases:

```
(case maybe-val
  ((Just x) x)
  (_ 0))
```

Literal types like `INT`, `STR`, and `USYM` are open-ended, so exhaustive
literal coverage is not required. In those cases, `_` is the usual catch-all.

## Uninterned Symbols

Uninterned symbols are lightweight tags written with a `:` prefix: `:foo`,
`:bar`, `:ok`, `:error`.

They are:

- self-evaluating
- compared by name
- useful when a full ADT would be overkill

Example:

```
(fun handle-status ((s %USYM))
  (case s
    (:ok "success")
    (:error "failure")
    (_ "unknown")))
```

They have type `%USYM`. Convert to and from strings with `usym-to-str` and
`str-to-usym`.

## What Belongs Elsewhere

Two common sources of confusion:

- FFI C types like `%I32`, `%PTR`, and `%VOID` are covered in [FFI](ffi.md),
  not in ordinary user type syntax
- typeclasses and class-based polymorphism are covered in
  [Typeclasses](typeclasses.md)
