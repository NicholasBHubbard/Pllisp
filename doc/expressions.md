# Expressions

Everything in pllisp is an expression. There are no statements.

## Top-Level Evaluation

A source file is a sequence of top-level expressions:

```
(print "first")
(print "second")
```

That prints:

```
first
second
```

Top-level declarations like `type`, `cls`, `inst`, `mac`, and `import` sit in
that same sequence.

Comments start with `#` and run to the end of the line:

```
(print "hi")  # this is a comment
```

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

String literals use normal double-quote syntax:

```
"hello"
"line 1\nline 2"
```

Uninterned symbols are useful as lightweight tags:

```
:ok
:error
:pending
```

## Let Bindings

`let` introduces local bindings. Bindings are sequential, so later bindings can
refer to earlier ones.

```
(let ((x 10)
      (y (add x 5)))
  (mul x y))
```

Bindings can include type annotations:

```
(let ((x %INT 42)) x)
```

Later scopes can shadow earlier names:

```
(let ((x 1))
  (let ((x 2))
    (print (int-to-str x))))
```

The special name `_` means “ignore this value”:

```
(let ((_ (print "side effect only")))
  42)
```

The [PRELUDE](stdlib/PRELUDE.md) `progn` macro is a convenient way to sequence several
expressions when you only care about the last result:

```
(progn
  (print "first")
  (print "second")
  (print "third"))
```

## Functions

Functions are created with `lam` and are first-class values.

```
(let ((double (lam ((x %INT)) (mul x 2))))
  (print (int-to-str (double 21))))
```

The PRELUDE `fun` macro is the normal shorthand for named functions:

```
(fun double ((x %INT)) (mul x 2))
(print (int-to-str (double 21)))
```

Function calls use ordinary prefix application:

```
(double 21)
(add 1 2)
(concat "hello" " world")
```

### Parameter Lists

Bare parameters are inferred. Annotated parameters use `(name %Type)`:

```
(lam (x y) (add x y))
(lam ((x %INT) (y %INT)) (add x y))
```

A return type annotation can follow the parameter list:

```
(lam ((x %INT)) %STR (int-to-str x))
```

Functions close over surrounding bindings:

```
(let ((offset 10))
  (let ((add-offset (lam (x) (add x offset))))
    (print (int-to-str (add-offset 32)))))
```

### Partial Application

Calling a function with fewer arguments than it needs returns another
function:

```
(let ((inc (add 1)))
  (print (int-to-str (inc 41))))
```

This also works with user-defined functions:

```
(let ((add3 (lam ((a %INT) (b %INT) (c %INT))
              (add a (add b c)))))
  (let ((add1and2 (add3 1 2)))
    (print (int-to-str (add1and2 39)))))
```

There is no builtin `apply` function. In many examples, `apply` is just a
user-defined helper name.

### Optional, Rest, and Keyword Parameters

Pllisp supports three extended parameter-list styles.

Optional parameters with defaults:

```
(lam (x %opt (y 0) (z 1)) (add x (add y z)))
```

Rest parameters collect the remaining positional arguments into a `List`:

```
(lam (first &rest others) others)
```

Keyword parameters use defaulted named arguments:

```
(lam (&key (width 80) (height 24))
  (mul width height))
```

Call keyword functions with explicit `&key` markers:

```
(render &key width 120 &key height 40)
```

More concrete examples:

```
# Optional parameter
(let ((greet (lam (%opt (name "world"))
               (concat "hello " name))))
  (print (greet))
  (print (greet "alice")))
```

```
# Rest parameter
(let ((go (lam ((xs %(List INT)))
            (case xs
              ((Empty) 0)
              ((Cons h t) (add h (go t)))))))
  (let ((sum-all (lam (&rest xs) (go xs))))
    (print (int-to-str (sum-all 1 2 3 4)))))
```

```
# Keyword parameters
(let ((area (lam (&key (width 1) (height 1))
              (mul width height))))
  (print (int-to-str (area &key width 3 &key height 4)))
  (print (int-to-str (area &key height 5))))
```

Rules:

- required parameters always come first
- use at most one of `%opt`, `&rest`, or `&key` in a single lambda list
- `&rest` collects extra positional arguments into a `List`
- keyword arguments are explicitly marked at the call site

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
      (if (eqi n 0)
        acc
        (sum (sub n 1) (add acc n))))))
  (print (int-to-str (sum 1000000 0))))
```

## Conditionals

Plain `if` requires a `%BOOL` condition and branches of the same type:

```
(if (gt x 0) "positive" "non-positive")
```

The PRELUDE adds truthiness-based control flow:

```
(if_ maybe-value "got something" "nothing")
(when (gt x 0) (print "positive"))
(unless (eqs s "") (print s))
(cond
  ((eqi x 1) "one")
  ((eqi x 2) "two")
  (true      "other"))
```

Use plain `if` when you already have a boolean. Use the PRELUDE macros when
you want `TRUTHY` behavior.

## Case Expressions

`case` is also an expression:

```
(case (Just 42)
  ((Just n) (int-to-str n))
  ((Nothing) "empty"))
```

It is the main tool for destructuring algebraic data types and lists. See
[Types](types.md#pattern-matching) for the full pattern syntax.

## Mutable References

Mutable state is explicit through references:

```
(let ((counter (ref 0)))
  (set! counter (add (deref counter) 1))
  (print (int-to-str (deref counter))))
```

`ref`, `deref`, and `set!` are polymorphic and work with any type.

## Strings and Text Builtins

Common string operations:

```
(print (concat "Hello" ", world"))
(print (int-to-str (strlen "pllisp")))
(print (substr "Hello, world" 7 5))
(print (if (str-contains "abc123" "123") "yes" "no"))
```

Notes:

- `concat` joins two strings
- `strlen` returns an `INT`
- `substr` takes `string`, `start`, `length`
- `str-contains` checks substring membership

## I/O and Command-Line Access

Basic line-oriented input:

```
(let ((line (read-line unit)))
  (print line))
```

You can check end-of-file with `is-eof`:

```
(let ((_ (read-line unit)))
  (print (if (is-eof unit) "done" "more")))
```

Command-line access:

```
(print (int-to-str (argc unit)))
(print (argv 1))
```

Important detail:

- `argc unit` counts the program name too
- `argv 1` is the first user-supplied argument

## Regular Expressions

Regex literals use `/pattern/flags` syntax and have type `%RX`.

```
(let ((pat /^hello/i))
  (if (rx-match pat "Hello, world")
    (print "matched")
    (print "no match")))
```

Supported flags:

- `i` — case-insensitive
- `m` — multiline
- `s` — dotall
- `x` — extended

Regex builtins cover matching, searching, replacement, splitting, and capture
groups:

```
# Find first match
(print (rx-find /[0-9]+/ "abc 42 def"))

# Replace first / all occurrences
(print (rx-sub  /world/ "pllisp" "hello world"))
(print (rx-gsub /o/     "0"      "foo"))

# Split by pattern
(let ((parts (rx-split /,\s*/ "a, b, c")))
  parts)

# Extract capture groups
(let ((caps (rx-captures /(\w+)@(\w+)/ "user@host")))
  caps)
```

You can also build a regex from a string at runtime with `rx-compile`:

```
(let ((pat (rx-compile (argv 1))))
  (rx-match pat (read-line unit)))
```

Useful behavior:

- `rx-find` returns the first match, or `""` if there is no match
- `rx-captures` returns a `List STR`; on no match, it returns `Empty`
- `rx-sub` replaces the first match
- `rx-gsub` replaces every match
- replacement strings can use backreferences

See [Reference](reference.md#regex) for the full list of regex builtins.
