# Standard Library

The `PRELUDE` module is implicitly imported into every program. You do not need
an explicit `(import PRELUDE)`.

## Types

```
(type List (a)
  (Empty)
  (Cons a %(List a)))

(type Maybe (a)
  (Nothing)
  (Just a))

(type Either (a b)
  (Left a)
  (Right b))

(type Pair (a b)
  (Pair a b))

(type Unit ()
  (Unit))
```

`List` is the standard linked list. `&rest` parameters in lambdas collect
arguments into a `List`.

`Maybe` represents optional values. `Either` represents values with two
possibilities (conventionally `Left` for errors, `Right` for success).

`Pair` is a simple two-element tuple.

`Unit` is a named wrapper around `unit` — useful when you need a constructor.

## Macros

### fun

Binds a named function. Shorthand for `let` + `lam`:

```
(fun square ((x %INT)) (mul x x))
# Expands to: (let ((square (lam ((x %INT)) (mul x x)))) square)
```

### progn

Sequences expressions, returning the last. Intermediate results are discarded:

```
(progn
  (print "a")
  (print "b")
  (print "c"))
# Expands to: (let ((_ (print "a"))) (let ((_ (print "b"))) (print "c")))
```

### if_

Like `if`, but the condition is converted to `%BOOL` via the `TRUTHY`
typeclass:

```
(if_ 42 "truthy" "falsy")     # "truthy" — non-zero INT
(if_ Nothing "yes" "no")      # "no" — Nothing is falsy
```

### when / unless

One-armed conditionals using `TRUTHY`. Return `unit` for the missing branch:

```
(when (gt x 0) (print "positive"))   # gt from ORD typeclass
(unless (eqs s "") (print s))
```

### cond

Multi-way conditional:

```
(cond
  ((eqi x 1) (print "one"))
  ((eqi x 2) (print "two"))
  (true      (print "other")))
```

### if-let / when-let / unless-let

Bind a value, then test its truthiness:

```
(if-let (x (find-user id))
  (print (.name x))
  (print "not found"))

(when-let (x (find-user id))
  (print (.name x)))

(unless-let (x (find-user id))
  (print "not found"))
```

## Typeclasses

### TRUTHY

Converts a value to `%BOOL`. Used by `if_`, `when`, `unless`, and the `*-let`
macros.

```
(cls TRUTHY () (a) (truthy %a %BOOL))
```

| Instance | Truthy when |
|----------|-------------|
| `%BOOL` | is `true` |
| `%INT` | is non-zero |
| `%FLT` | is non-zero |
| `%STR` | is non-empty |
| `%(Maybe a)` | is `Just` |
| `%(List a)` | is `Cons` |
| `%(Either a b)` | is `Right` |

### EQ

Equality comparison. Provides `eq` as a typeclass method, replacing direct
use of the primitive builtins (`eqi`, `eqf`, `eqs`).

```
(cls EQ () (a) (eq %a %a %BOOL))
```

| Instance | Delegates to |
|----------|--------------|
| `%INT` | `eqi` |
| `%FLT` | `eqf` |
| `%STR` | `eqs` |
| `%BOOL` | pure (XNOR) |

```
(eq 1 1)            # true — uses EQ %INT
(eq "hello" "hello") # true — uses EQ %STR
```

### ORD

Ordering comparison. Requires `EQ`. Provides `lt`, `gt`, `le`, `ge` as
typeclass methods, replacing direct use of the primitive builtins (`lti`,
`gti`, `lei`, `gei`).

```
(cls ORD (EQ) (a) (lt %a %a %BOOL) (gt %a %a %BOOL) (le %a %a %BOOL) (ge %a %a %BOOL))
```

| Instance | Delegates to |
|----------|--------------|
| `%INT` | `lti`, `gti`, `lei`, `gei` |
| `%FLT` | `ltf`, `gtf`, `lef`, `gef` |
| `%STR` | `lts`, `gts`, `les`, `ges` |

```
(gt 3 2)          # true — uses ORD %INT
(le 1.5 2.0)      # true — uses ORD %FLT
```

### STRINGY

Converts a value to `%STR`.

```
(cls STRINGY () (a) (str %a %STR))
```

| Instance | Result |
|----------|--------|
| `%BOOL` | `"true"` or `"false"` |
| `%INT` | decimal string |
| `%FLT` | decimal string |
| `%STR` | identity |
| `%USYM` | symbol name as string |

See also: [Typeclasses](typeclasses.md) for how to define your own.
