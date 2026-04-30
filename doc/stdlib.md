# Standard Library

The `PRELUDE` module is implicitly available in every program.

You do not need an explicit `(import PRELUDE)`.

Do not write one anyway. `PRELUDE` is already available, and an explicit
`(import PRELUDE)` currently fails with duplicate macro definitions.

## What PRELUDE Gives You

The PRELUDE provides:

- common algebraic data types
- convenience macros for writing ordinary code
- core typeclasses used throughout examples and user programs

If you want the “language most people actually write,” this page matters.

## Types

The PRELUDE defines:

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

### `List`

`List` is the standard linked list type.

```
Empty
(Cons 1 (Cons 2 (Cons 3 Empty)))
```

Pattern matching:

```
(case xs
  ((Empty) 0)
  ((Cons h t) (add h (sum-list t))))
```

`&rest` parameters collect their extra arguments into a `List`.

### `Maybe`

`Maybe` represents optional values:

```
Nothing
(Just 42)
```

Typical use:

```
(fun safe-div ((a %INT) (b %INT))
  (if (eqi b 0) Nothing (Just (div a b))))
```

### `Either`

`Either` represents two possibilities, conventionally error/success:

```
(Left "bad input")
(Right 42)
```

### `Pair`

`Pair` is a simple 2-tuple:

```
(Pair "x" 10)
```

### `Unit`

`Unit` is a named wrapper around the `unit` value. Use it when you need a
constructor form instead of the bare builtin value.

## Macros

The PRELUDE macros are the main reason ordinary pllisp programs read cleanly.

### `fun`

Binds a named function.

```
(fun square ((x %INT))
  (mul x x))
```

Equivalent shape:

```
(let ((square (lam ((x %INT)) (progn (mul x x)))))
  square)
```

`fun` accepts one or more body forms. Multiple forms are wrapped in `progn`
automatically:

```
(fun greet ((name %STR))
  (print "greeting")
  (print (concat "hello " name)))
```

Explicit return annotations still work with multi-form bodies:

```
(fun next ((x %INT)) %INT
  (print "bump")
  (add x 1))
```

### `val`

Creates a top-level immutable binding:

```
(val answer 42)
(print (int-to-str answer))
```

This is declaration-style sugar for an exported top-level binding. Use `val`
when you want a named module constant without writing a full `let`.

### `var`

Creates a top-level mutable reference binding:

```
(var counter 0)
(set! counter (add (deref counter) 1))
(print (int-to-str (deref counter)))
```

`var` binds a `REF` value. Reads still use `deref`, and writes still use
`set!`.

### `progn`

Sequences expressions and returns the last one:

```
(progn
  (print "a")
  (print "b")
  (print "c"))
```

It is the normal way to write “do these things in order” code.

### `foreach`

Iterates a `List` for side effects:

```
(foreach (x xs)
  (print "value:")
  (print (int-to-str x)))
```

`foreach` evaluates the list once, binds the loop variable for each `Cons`
element, and returns `unit` when it reaches `Empty`.

Use it for side-effecting iteration. If you want to build a result, prefer
`map`, `filter`, or `foldl`.

### `if_`

Like `if`, but first converts the condition through the `TRUTHY` typeclass:

```
(if_ 42 "truthy" "falsy")
(if_ Nothing "yes" "no")
```

Use plain `if` when you already have a `%BOOL`. Use `if_` when you want
truthiness semantics.

### `when` and `unless`

One-armed conditionals using `TRUTHY`:

```
(when (gt x 0)
  (print "positive"))

(unless (eqs s "")
  (print s))
```

### `cond`

Multi-way conditional:

```
(cond
  ((eqi x 1) (print "one"))
  ((eqi x 2) (print "two"))
  (true      (print "other")))
```

### `if-let`, `when-let`, `unless-let`

Bind a value, then test its truthiness:

```
(if-let (x maybe-user)
  (print (.name x))
  (print "not found"))
```

```
(when-let (x maybe-user)
  (print (.name x)))
```

```
(unless-let (x maybe-user)
  (print "not found"))
```

These are especially useful with `Maybe`, lists, and strings.

## Typeclasses

### `TRUTHY`

Converts a value to `%BOOL`. It powers `if_`, `when`, `unless`, and the
`*-let` macros.

```
(cls TRUTHY () (a)
  (truthy %a %BOOL))
```

Built-in PRELUDE instances:

| Instance | Truthy when |
|----------|-------------|
| `%BOOL` | is `true` |
| `%INT` | is non-zero |
| `%FLT` | is non-zero |
| `%STR` | is non-empty |
| `%(Maybe a)` | is `Just` |
| `%(List a)` | is `Cons` |
| `%(Either a b)` | is `Right` |

Examples:

```
(if_ "hello" "yes" "no")
(if_ "" "yes" "no")
```

### `EQ`

Generic equality comparison.

```
(cls EQ () (a)
  (eq %a %a %BOOL))
```

Built-in PRELUDE instances:

| Instance | Behavior |
|----------|----------|
| `%INT` | uses integer equality |
| `%FLT` | uses float equality |
| `%STR` | uses string equality |
| `%BOOL` | compares booleans |

Examples:

```
(eq 1 1)
(eq "hello" "hello")
(eq false false)
```

Use `eq` when you want the typeclass-based interface. Use `eqi`, `eqf`, or
`eqs` when you explicitly want the raw primitive builtin.

### `ORD`

Generic ordering comparison.

```
(cls ORD (EQ) (a)
  (lt %a %a %BOOL)
  (gt %a %a %BOOL)
  (le %a %a %BOOL)
  (ge %a %a %BOOL))
```

Built-in PRELUDE instances:

| Instance | Supports |
|----------|----------|
| `%INT` | `lt`, `gt`, `le`, `ge` |
| `%FLT` | `lt`, `gt`, `le`, `ge` |
| `%STR` | lexicographic comparison |

Examples:

```
(gt 3 2)
(le 1.5 2.0)
(lt "apple" "banana")
```

### `STRINGY`

Converts a value to `%STR`.

```
(cls STRINGY () (a)
  (str %a %STR))
```

Built-in PRELUDE instances:

| Instance | Result |
|----------|--------|
| `%BOOL` | `"true"` or `"false"` |
| `%INT` | decimal string |
| `%FLT` | decimal string |
| `%STR` | identity |
| `%USYM` | symbol name as string |

Examples:

```
(str 42)
(str true)
(str :ok)
```

## `CLI` Module

Import `CLI` when you want top-level command-line bindings generated from a
declarative spec:

```
(import CLI)
```

The core form is the `CLI` macro. It is a top-level declaration form, not a
parser-building function.

Supported clauses:

- `(:flag :name "-s" "--long")`
- `(:option :name "-s" "--long")`
- `(:arg :name)`
- `(:rest :name)`

Each clause name is a usym. `CLI` derives an ordinary top-level binding from
that usym:

- `:flag` binds a `%BOOL`
- `:option` binds a `%(Maybe %STR)`
- `:arg` binds a `%STR`
- `:rest` binds a `%(List %STR)`

Example:

```lisp
(import CLI)

(fun list-len ((xs %(List %STR))) %INT
  (case xs
    ((Empty) 0)
    ((Cons _ rest) (add 1 (list-len rest)))))

(CLI
  (:flag :verbose "-v" "--verbose")
  (:option :output "-o" "--output")
  (:arg :mode)
  (:arg :input)
  (:rest :extras))

(print (if verbose "true" "false"))
(case output
  ((Just path) (print path))
  (_ (print "missing")))
(print mode)
(print input)
(print (int-to-str (list-len extras)))
```

At expansion time, `CLI` splices in:

- one hidden binding that parses `argv`
- one visible top-level binding per declared clause

That means later top-level forms in the same file can use `verbose`, `output`,
`mode`, `input`, and `extras` directly.

Notes:

- there is no `required-option`; if a value is required, make it a positional
  `:arg`
- `:flag` produces a boolean
- `:option` produces `%(Maybe %STR)`
- `:arg` consumes one positional argument
- `:rest` consumes the remaining positional arguments and must be last
- `--` stops option parsing; everything after it is treated as positional.
- unknown options terminate the program with a parse error
- missing positional arguments terminate the program with a parse error

## `FILEIO` Module

Import `FILEIO` when you want file and directory operations:

```lisp
(import FILEIO)
```

`FILEIO` has two layers:

- whole-file helpers like `slurp` and `spurt`
- typed file handles plus scoped macros like `with-input`

Prefer the whole-file layer first. Use explicit handles when you actually need
streaming behavior.

### Error Type

Most `FILEIO` operations return `Either`:

```lisp
(type FileError ()
  (FileError %USYM %STR %STR))
```

Accessors:

- `error-op`
- `error-path`
- `error-message`

Example:

```lisp
(import FILEIO (slurp error-message))

(case (slurp "config.txt")
  ((Right text) (print text))
  ((Left err) (print (error-message err))))
```

### Whole-File Helpers

- `slurp : STR -> Either FileError STR`
- `lines : STR -> Either FileError (List STR)`
- `spurt : STR -> STR -> Either FileError UNIT`
- `append-file : STR -> STR -> Either FileError UNIT`
- `spurt-lines : STR -> List STR -> Either FileError UNIT`
- `append-lines : STR -> List STR -> Either FileError UNIT`

These names are intentionally perlish:

- `slurp` reads the whole file
- `spurt` overwrites the whole file
- `append-file` appends text to the end

`append-file` is named that way because plain `append` is already a compile-time
helper in the macro system.

### File Handles

`FILEIO` exposes three handle types:

- `InFile`
- `OutFile`
- `AppendFile`

Open/close functions:

- `open-in`, `close-in`
- `open-out`, `close-out`
- `open-append`, `close-append`

Reading and writing:

- `next-line : InFile -> Either FileError (Maybe STR)`
- `read-all : InFile -> Either FileError STR`
- `eof? : InFile -> Either FileError BOOL`
- `write : OutFile -> STR -> Either FileError UNIT`
- `write-append : AppendFile -> STR -> Either FileError UNIT`
- `say : OutFile -> STR -> Either FileError UNIT`
- `say-append : AppendFile -> STR -> Either FileError UNIT`
- `flush-out : OutFile -> Either FileError UNIT`
- `flush-append : AppendFile -> Either FileError UNIT`

`next-line` returns:

- `Left err` for an actual read failure
- `Right Nothing` at EOF
- `Right (Just line)` for a line

The name is `next-line`, not `read-line`, because `read-line` is already the
implicit PRELUDE function for stdin.

### Scoped File Macros

Use the handle macros for ergonomic block-scoped file access:

- `with-input`
- `with-output`
- `with-append`
- `foreach-line`

Example:

```lisp
(import FILEIO (write say flush-out error-message))

(case (with-output (fh "out.txt")
        (write fh "alpha")
        (say fh "beta")
        (flush-out fh))
  ((Right _) unit)
  ((Left err) (print (error-message err))))
```

`with-input`, `with-output`, and `with-append` are not ordinary sequencing
macros. Each body form must return `Either FileError a`. They short-circuit on
`Left`, then still close the handle before returning.

`foreach-line` is the easiest way to read a text file line by line:

```lisp
(import FILEIO (spurt unlink error-message))

(case (spurt "/tmp/example.txt" "red\nblue\n")
  ((Left err) (print (error-message err)))
  ((Right _)
    (case (foreach-line (line "/tmp/example.txt")
            (print line))
      ((Left err) (print (error-message err)))
      ((Right _)
        (unlink "/tmp/example.txt")))))
```

Plain `(import FILEIO)` is enough to make the `FILEIO` macros available. If you
also want unqualified runtime names like `slurp` or `error-message`, list them
explicitly in the import form.

A handle-oriented example:

```lisp
(import FILEIO (write say flush-out next-line eof? error-message))

(fun tap-line (result)
  (case result
    ((Left err) (Left err))
    ((Right maybe-line)
      (case maybe-line
        ((Nothing) (Right unit))
        ((Just line)
          (progn
            (print line)
            (Right unit)))))))

(case (with-output (fh "/tmp/report.txt")
        (write fh "alpha")
        (say fh "beta")
        (flush-out fh))
  ((Left err) (print (error-message err)))
  ((Right _)
    (case (with-input (fh "/tmp/report.txt")
            (tap-line (next-line fh))
            (tap-line (next-line fh)))
      ((Left err) (print (error-message err)))
      ((Right _) unit))))
```

### Filesystem Helpers

- `exists?`
- `is-file?`
- `is-dir?`
- `stat`
- `readdir`
- `mkdir`
- `mkdir-p`
- `unlink`
- `rename`
- `copy`

`stat` returns `Either FileError FileInfo`:

```lisp
(type FileKind ()
  (RegularFile)
  (Directory)
  (Other))

(type FileInfo ()
  (FileInfo %STR %FileKind %INT))
```

Accessors:

- `info-path`
- `info-kind`
- `info-size`

## Practical Advice

- reach for `fun` and `progn` immediately; they make code much clearer
- prefer `eq`, `lt`, `gt`, `le`, `ge`, and `str` when you want the generic
  class-based APIs
- use `if_`/`when`/`unless` only when you want truthiness semantics
- do not try to import `PRELUDE` explicitly

See also:

- [Typeclasses](typeclasses.md)
- [Macros](macros.md)
- [Types](types.md)
