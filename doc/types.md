# Types

Pllisp uses Hindley-Milner type inference. Most code needs no type annotations
at all — the compiler infers types from usage. Annotations are available when
you want them, using the `%` prefix.

## Type Annotations

The `%` character introduces a type expression anywhere one is expected:

```
%INT                     # primitive type
%(List INT)              # parameterized type
%(-> INT INT BOOL)       # function type: two INTs to BOOL
%(Maybe (List STR))      # nested parameterized type
```

Annotations appear in lambda parameters, let bindings, and constructor
arguments:

```
(lam ((x %INT) (y %INT)) (add x y))
(let ((name %STR "Alice")) name)
(type Box (a) (Box a))
```

## Primitive Types

| Type | Description | Size |
|------|-------------|------|
| `%INT` | Signed integer | 64-bit |
| `%FLT` | Floating point | 64-bit |
| `%STR` | String | pointer |
| `%BOOL` | Boolean | 1-bit |
| `%UNIT` | Unit (no meaningful value) | -- |
| `%RX` | Compiled regular expression | pointer |
| `%USYM` | Uninterned symbol | pointer |

## Type Inference

The compiler infers types through unification. You rarely need annotations:

```
# The compiler infers: double : INT -> INT
(let ((double (lam (x) (mul x 2))))
  (double 21))
```

Annotations are needed when there is genuine ambiguity, or for documentation:

```
# Without annotation, the compiler can't know which numeric type
(let ((zero %INT 0)) zero)
```

## Algebraic Data Types

Types are defined with the `type` form. Each type has a name, type parameters,
and one or more constructors.

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

Constructors are functions — `Just` has type `a -> Maybe a`, `Cons` has type
`a -> List a -> List a`.

### Records

Constructor fields can be named. Named fields enable `.field` accessor syntax:

```
(type Person ()
  (Person (name %STR) (age %INT)))

(let ((p (Person "Alice" 30)))
  (progn
    (print (.name p))
    (print (int-to-str (.age p)))))
```

## Pattern Matching

`case` destructures values by pattern:

```
(case maybe-value
  ((Just x) (print (int-to-str x)))
  ((Nothing) (print "nothing")))
```

Patterns can be:

- **Wildcard**: `_` matches anything without binding
- **Variable**: `x` matches anything and binds the value
- **Literal**: `42`, `3.14`, `"hello"`, `true`, `false`, `:foo`
- **Constructor**: `(Just x)`, `(Cons h t)`, `(Pair a b)`

Patterns nest:

```
(case list-of-pairs
  ((Cons (Pair k v) rest) (print k))
  ((Empty) (print "empty")))
```

### Exhaustiveness Checking

The compiler verifies that `case` expressions cover all constructors. This
fails to compile:

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

Literal types (`INT`, `STR`, `USYM`, etc.) are open-ended — the compiler does
not require exhaustive literal coverage, but a wildcard catches everything
else.

## Uninterned Symbols

Uninterned symbols are lightweight tags written with a `:` prefix: `:foo`,
`:bar`, `:ok`, `:error`. They are self-evaluating, compared by name, and
useful where you want tagged values without defining a full ADT:

```
(case status
  (:ok (print "success"))
  (:error (print "failure"))
  (_ (print "unknown")))
```

They have type `%USYM`. Convert to and from strings with `usym-to-str` and
`str-to-usym`.
