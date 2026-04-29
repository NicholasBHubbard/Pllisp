# Macros

Pllisp macros transform syntax into new syntax before the surrounding program
is processed as ordinary code.

They are procedural, not just template substitutions. A macro body runs in a
compile-time environment, can inspect and build syntax, and returns the syntax
that replaces the macro call.

## When to Use a Macro

Use a macro when you want to:

- introduce a new surface syntax pattern
- avoid repeating a structural code pattern
- control evaluation of user-written code
- generate code from syntax, not from already evaluated values

Use an ordinary function when plain arguments and ordinary return values are
enough.

## Basic Macro Definition

```lisp
(mac unless (cond body)
  `(if ,cond unit ,body))
```

General form:

```lisp
(mac name (params...) body)
```

Example use:

```lisp
(unless false
  (print "ran"))
```

The macro call is replaced with the syntax returned by the body.

## Macros Are Top-Level Declarations

`mac` is a top-level form. In practice, that means:

- define macros at the file top level
- or define them inside a top-level `eval-when`
- do not expect nested `mac` forms inside normal runtime expressions to work

For example:

```lisp
(mac double (x)
  `(add ,x ,x))

(print (int-to-str (double 21)))
```

## Same-File Order Matters

Macros in the same file are available only after their definition has been
seen.

This works:

```lisp
(mac double (x)
  `(add ,x ,x))

(print (int-to-str (double 21)))
```

This does not:

```lisp
(print (int-to-str (double 21)))

(mac double (x)
  `(add ,x ,x))
```

That is deliberate. Pllisp no longer scans the whole file up front and makes
all later macros magically visible earlier.

Imported macro modules are different: imports are collected before expansion,
so imported macros are available throughout the file even if the `import` form
appears later.

## Quasiquote, Unquote, and Splicing

Most macros use quasiquote:

```lisp
(mac progn (&rest args)
  (if (eq (syntax-length args) 1)
    (syntax-car args)
    `(let ((_ ,(syntax-car args)))
       (progn ,@(syntax-cdr args)))))
```

Key pieces:

- `` `... `` builds syntax
- `,x` inserts one computed syntax value
- `,@xs` splices a list of syntax values into the surrounding list

Calling:

```lisp
(progn a b c)
```

expands to code shaped like:

```lisp
(let ((_ a))
  (let ((_ b))
    c))
```

## Quote

`quote` returns literal syntax without evaluating it:

```lisp
(mac literal-empty-list ()
  (quote ()))
```

Treat `quote`, quasiquote, unquote, and unquote-splicing as macro-writing
tools.

## Syntax Values

The public compile-time syntax type is `%SYNTAX`.

Important facts:

- macro parameters are `%SYNTAX`
- `&rest` parameters receive one syntax-list value of type `%SYNTAX`
- `quote` and quasiquote produce `%SYNTAX`
- `%SYNTAX` is compile-time only and cannot appear in ordinary runtime code

You can write explicit typed compile-time helpers against `%SYNTAX`:

```lisp
(eval-when (:compile-toplevel)
  (fun emit-inc ((x %SYNTAX)) %SYNTAX
    `(add ,x 1)))
```

## Macro Parameters

### Positional Parameters

```lisp
(mac pair (a b)
  `(Cons ,a (Cons ,b Empty)))
```

### Rest Parameters

`&rest` collects the remaining syntax arguments into a list:

```lisp
(mac call-with (f &rest xs)
  `(,f ,@xs))
```

### Destructuring Parameters

You can destructure simple list-shaped syntax directly in the parameter list:

```lisp
(mac when-bind ((name expr) body)
  `(let ((,name ,expr))
     (when ,name ,body)))
```

Called like:

```lisp
(when-bind (x maybe-value)
  (print x))
```

## Procedural Macro Bodies

A macro body can work in two styles:

- syntax-oriented code that directly inspects and rewrites syntax values
- ordinary compile-time helper code defined elsewhere and called from the macro

The first style is the classic procedural-macro style. A macro can compute,
branch, recurse, inspect lists of syntax, and build new syntax
programmatically:

```lisp
(mac count-args (&rest xs)
  `(int-to-str ,(syntax-length xs)))
```

or:

```lisp
(mac first-of (&rest xs)
  (syntax-car xs))
```

For larger compile-time logic, the better style is to put the logic in
`eval-when (:compile-toplevel ...)` helper bindings and keep the `mac` body
small:

```lisp
(eval-when (:compile-toplevel)
  (fun emit-default ()
    `(add 20 22)))

(mac answer ()
  (emit-default))
```

The result still has to be syntax. If a macro body returns something that
cannot be converted back into syntax, expansion fails.

## The Compile-Time Environment

Macro bodies do not run in the final runtime program. They run at compile
time, but they now use the ordinary typed pllisp language instead of a
separate ad hoc macro language.

That means compile-time code can use:

- ordinary expressions like `let`, `lam`, `if`, `case`, and function calls
- earlier declarations from the same module, such as constructors, typeclass
  methods, FFI declarations, and earlier top-level runtime bindings that are
  themselves compile-time-evaluable
- imported compile-time definitions from macro libraries
- imported runtime bindings from other modules when those bindings can be
  evaluated at compile time
- the public `%SYNTAX` API: `syntax-car`, `syntax-cdr`, `syntax-cons`,
  `syntax-length`, `syntax-symbol`, `syntax-symbol-name`, and related helpers
- compile-time helper functions loaded from `PRELUDE`, such as `append`,
  `reverse`, `map`, `filter`, and `foldl`
- `ref`, `deref`, and `set!`
- regex helpers such as `rx-compile`, `rx-match`, `rx-find`, `rx-sub`,
  `rx-gsub`, `rx-split`, and `rx-captures`
- `gensym` and `error`

Two boundaries still matter:

- compile-time code must return syntax if it is being used as a macro result
- FFI-backed runtime bindings are still not executable at macro expansion
  time; attempts to use them fail explicitly instead of acting like missing
  names

For example:

```lisp
(mac quote-all (&rest xs)
  (map (lam ((x %SYNTAX)) `(quote ,x)) xs))
```

## `eval-when`

Use `eval-when` when a module needs compile-time helper definitions or macros
that should become available to later forms.

General form:

```lisp
(eval-when (phases...)
  form...)
```

Supported phases:

- `:compile-toplevel`
- `:load-toplevel`
- `:execute`

Unquoted names such as `compile-toplevel` also work, but the `:name` spelling
is the clearer style.

### `:compile-toplevel`

Forms in `:compile-toplevel` are evaluated during macro expansion.

This is how you define typed helper functions for later macros:

```lisp
(eval-when (:compile-toplevel)
  (fun emit-double ((x %SYNTAX)) %SYNTAX
    `(add ,x ,x)))

(mac double (x)
  (emit-double x))
```

Those helper forms are checked as ordinary pllisp code. They can use language
features like:

- earlier top-level runtime function bindings from the same module
- `type` constructors and `case`
- imported constructors and typeclass methods
- earlier declaration forms in the same file

For example:

```lisp
(fun double-int ((x %INT)) %INT
  (add x x))

(eval-when (:compile-toplevel)
  (fun emit-double ((x %SYNTAX)) %SYNTAX
    (syntax-int (double-int (syntax-int-value x)))))

(mac doubled (x)
  (emit-double x))
```

For example:

```lisp
(type Flag () (Flag))

(eval-when (:compile-toplevel)
  (let ((default Flag))
    default)
  (fun emit-flag ()
    (case default
      ((Flag) `1))))

(mac use-flag ()
  (emit-flag))
```

This is also how you define macros from inside `eval-when`:

```lisp
(eval-when (:compile-toplevel)
  (mac double (x)
    `(add ,x ,x)))
```

### `:load-toplevel` and `:execute`

Forms in `:load-toplevel` or `:execute` are emitted into the program as normal
top-level code.

Example:

```lisp
(eval-when (:execute)
  (print "hello"))
```

### Combined Phases

You can combine phases:

```lisp
(eval-when (:compile-toplevel :execute)
  (let ((emit-double (lam (x) `(add ,x ,x))))
    emit-double)
  (print "runtime side"))
```

That example does two separate things:

- defines `emit-double` for later macro expansion
- also emits its body forms as normal top-level code because `:execute` is
  present

If you only want compile-time helper definitions, use `:compile-toplevel`
alone.

### What Actually Persists

At compile time, the useful persistent top-level forms are:

- `mac` definitions
- top-level `let` bindings inside `:compile-toplevel`
- earlier same-module runtime bindings that are compile-time-evaluable and are
  captured by later compile-time code
- imported runtime bindings that are compile-time-evaluable
- earlier and imported declaration surfaces such as constructors, typeclass
  methods, and FFI declarations

Those names become available to later forms in the same module, and to
importing modules where appropriate.

## Macros Capture Their Definition-Time Helpers

When you define a macro, it captures the compile-time helper environment that
exists at that point.

So this is the sane pattern:

```lisp
(eval-when (:compile-toplevel)
  (let ((emit-double (lam (x) `(add ,x ,x))))
    emit-double))

(mac double (x)
  (emit-double x))
```

Do not assume a macro will automatically pick up helper definitions that only
appear later in the file.

## Hygiene and `gensym`

Pllisp macros are not hygienic by default. If a macro introduces a temporary
name, that name can capture or collide with user code unless you generate a
fresh one.

Use `gensym` for hidden temporaries:

```lisp
(mac with-tmp (expr body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,expr))
       ,body)))
```

## Failing Deliberately

Use the compile-time `error` function when a macro detects invalid input:

```lisp
(mac expect-one (&rest xs)
  (if (eq (syntax-length xs) 1)
    (syntax-car xs)
    (error "expected exactly one item")))
```

The expansion stops with a macro error.

## Macros in Modules

Macros work across modules without any special machinery. A module can export:

- top-level `mac` definitions
- compile-time helper bindings created by top-level
  `eval-when (:compile-toplevel)` `let` forms

Example:

`BUILDERS.pll`:

```lisp
(module BUILDERS)

(fun double-int ((x %INT)) %INT
  (add x x))

(eval-when (:compile-toplevel)
  (fun emit-double-print ((expr %SYNTAX)) %SYNTAX
    `(print (int-to-str ,(syntax-int (double-int (syntax-int-value expr)))))))
```

`MACROS.pll`:

```lisp
(module MACROS)
(import BUILDERS)

(mac show-double (expr)
  (emit-double-print expr))
```

`main.pllisp`:

```lisp
(import MACROS)

(show-double 21)
```

That is a real supported pattern, not a workaround.

## Current Limits

- macros are top-level declarations
- same-file macro definitions are order-sensitive
- multi-clause macro definitions are not supported
- imported macro names and imported compile-time helper names share one
  compile-time namespace, so duplicate names across imports are errors
- module aliases do not namespace macro calls

For the module-level rules around macro imports, aliases, and collisions, see
[Modules](modules.md#macros-and-compile-time-imports).

For PRELUDE macros such as `fun`, `progn`, `if_`, `when`, and `cond`, see
[Standard Library](stdlib.md#macros).
