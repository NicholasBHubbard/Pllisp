# Expressions

Everything in pllisp is an expression. There are no statements.

## Literals

```
42              # INT   — 64-bit signed integer
3.14            # FLT   — 64-bit double
"hello"         # STR   — string
true            # BOOL  — boolean
false           # BOOL
unit            # UNIT  — the unit value
/pattern/i      # RX    — regular expression with flags
:foo            # USYM  — uninterned symbol
```

See [Types](types.md#primitive-types) for the full type table.

## Let Bindings

`let` introduces local bindings. Bindings are sequential — each one can
reference those above it.

```
(let ((x 10)
      (y (add x 5)))
  (mul x y))
```

A binding to `_` discards the result, useful for sequencing side effects:

```
(let ((_ (print "first")))
  (print "second"))
```

The [PRELUDE](stdlib.md) provides a `progn` macro for this pattern:

```
(progn
  (print "first")
  (print "second")
  (print "third"))
```

Bindings can include type annotations:

```
(let ((x %INT 42)) x)
```

## Functions

Functions are created with `lam` and are first-class values. Since there is
no top-level `def` form, named functions are just `let`-bound lambdas:

```
(let ((double (lam ((x %INT)) (mul x 2))))
  (print (int-to-str (double 21))))
```

The [PRELUDE](stdlib.md) `fun` macro makes this less verbose:

```
(fun double ((x %INT)) (mul x 2))
(print (int-to-str (double 21)))
```

### Parameter Lists

Bare parameters are inferred. Annotated parameters use the `(name %Type)` form:

```
(lam (x y) (add x y))               # types inferred
(lam ((x %INT) (y %INT)) (add x y)) # explicitly typed
```

A return type annotation can follow the parameter list:

```
(lam ((x %INT)) %STR (int-to-str x))
```

### Optional, Rest, and Keyword Parameters

Optional parameters with defaults:

```
(lam (x %opt (y 0) (z 1)) (add x (add y z)))
```

A rest parameter collects remaining positional arguments into a `List`:

```
(lam (first &rest others) others)
```

Keyword parameters with defaults:

```
(lam (&key (width 80) (height 24)) (mul width height))
```

Called with `&key` at the call site:

```
(f &key width 120 &key height 40)
```

These three forms are mutually exclusive — a lambda uses at most one of
`%opt`, `&rest`, or `&key`.

### Recursion

Lambdas bound with `let` can call themselves recursively:

```
(let ((fact (lam ((n %INT))
      (if (eqi n 0)
        1
        (mul n (fact (sub n 1)))))))
  (print (int-to-str (fact 10))))
```

### Tail Call Optimization

Self-recursive tail calls are handled efficiently:

```
(let ((sum (lam ((n %INT) (acc %INT))
      (if (eqi n 0) acc (sum (sub n 1) (add acc n))))))
  (print (int-to-str (sum 1000000 0))))
```

## Conditionals

`if` requires a `%BOOL` condition and two branches of the same type:

```
(if (gt x 0) "positive" "non-positive")   # gt from ORD typeclass
```

The [PRELUDE](stdlib.md) extends this with macros that use the `TRUTHY`
typeclass for implicit boolean conversion:

```
(if_ maybe-value "got something" "nothing")
(when (gt x 0) (print "positive"))   # gt from ORD typeclass
(unless (eqs s "") (print s))
(cond
  ((eqi x 1) "one")
  ((eqi x 2) "two")
  (true      "other"))
```

## Mutable References

Mutable state is explicit through references:

```
(let ((counter (ref 0)))
  (set! counter (add (deref counter) 1))
  (print (int-to-str (deref counter))))
```

`ref`, `deref`, and `set!` are polymorphic — they work with any type.

## Regular Expressions

Regex literals use `/pattern/flags` syntax and have type `%RX`.

```
(let ((pat /^hello/i))
  (if (rx-match pat "Hello, world")
    (print "matched")
    (print "no match")))
```

Supported flags: `i` (case-insensitive), `m` (multiline), `s` (dotall),
`x` (extended).

The regex builtins cover matching, searching, replacement, splitting, and
capture groups:

```
# Find first match
(print (rx-find /[0-9]+/ "abc 42 def"))   # "42"

# Replace first / all occurrences
(print (rx-sub  /world/ "hello world" "pllisp"))  # "hello pllisp"
(print (rx-gsub /o/     "foo" "0"))                # "f00"

# Split by pattern
(let ((parts (rx-split /,\s*/ "a, b, c")))
  parts)  # List of "a", "b", "c"

# Extract capture groups
(let ((caps (rx-captures /(\w+)@(\w+)/ "user@host")))
  caps)  # List of "user", "host"
```

You can also create a pattern from a string at runtime with `rx-compile`:

```
(let ((pat (rx-compile (argv 1))))
  (rx-match pat (read-line unit)))
```

See [Reference](reference.md#regex) for the full list of regex builtins.
