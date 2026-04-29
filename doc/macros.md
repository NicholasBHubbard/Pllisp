# Macros

Pllisp has a Common Lisp-style procedural macro system. Macros operate on
syntax and produce new syntax.

## When to Use a Macro

Use a macro when you want to:

- introduce a new surface syntax pattern
- avoid repeating a structural code pattern
- bind user-written syntax pieces directly instead of evaluated values

Use an ordinary function when evaluated arguments are enough.

## Defining Macros

```
(mac unless (cond body)
  `(if_ ,cond unit ,body))
```

The syntax is:

```
(mac name (params...) body)
```

The result of the macro body replaces the macro call in the surrounding code.

Macros are usually defined at top level.

## Quasiquote, Unquote, and Splicing

Most macros are written with quasiquote:

```
(mac progn (&rest args)
  (if (eq (length args) 1)
    (car args)
    `(let ((_ ,(car args))) (progn ,@(cdr args)))))
```

Key pieces:

- `` `... `` — build syntax
- `,x` — insert one computed syntax value
- `,@xs` — splice a list of syntax values into the surrounding list

This macro turns:

```
(progn a b c)
```

into code shaped like:

```
(let ((_ a))
  (let ((_ b))
    c))
```

## Quote

`quote` is available inside macro definitions and returns literal syntax
without evaluating it:

```
(mac make-list (&rest xs)
  (let ((build (lam (items)
    (if (null? items)
      (quote Empty)
      `(Cons ,(car items) ,(build (cdr items)))))))
    (build xs)))
```

Treat `quote`, quasiquote, unquote, and unquote-splicing as macro-writing
tools, not general runtime features.

## Macro Parameters

### Positional Parameters

```
(mac pair (a b)
  `(Cons ,a (Cons ,b Empty)))
```

### Rest Parameters

Rest parameters collect all remaining syntax arguments into a list:

```
(mac my-list (&rest xs)
  `(list ,@xs))
```

### Destructuring Parameters

You can match simple list structure directly in the parameter list:

```
(mac when-bind ((name expr) body)
  `(let ((,name ,expr))
     (when ,name ,body)))
```

That macro is called like:

```
(when-bind (x maybe-value)
  (print x))
```

## Macro-Time vs Runtime

Macro code runs in a separate macro evaluation environment with its own
builtins.

For example:

```
# Macro-time: eq compares syntax values
(mac is-zero (x)
  (if (eq x 0) `true `false))

# Runtime: eqi compares integer values
(if (eqi n 0) "zero" "nonzero")
```

Inside a macro body:

- `eq` works on syntax values
- `car`, `cdr`, `map`, `filter`, and `foldl` work on syntax lists
- `add`, `sub`, `mul`, `lt`, and `gt` work on macro-time integers

Those are macro-time tools. They are not the same functions as runtime
program code.

## `gensym` and Hygiene

Use `gensym` when a macro needs a fresh temporary name:

```
(mac with-tmp (expr body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,expr))
       ,body)))
```

That avoids accidental capture of a caller’s existing variable name.

## Failing Deliberately

Use the macro-time `error` builtin when a macro detects bad input:

```
(mac expect-one (xs)
  (if (eq (length xs) 1)
    (car xs)
    (error "expected exactly one item")))
```

## Macro-Time Builtins

| Function | Description |
|----------|-------------|
| `car`, `cdr` | Head / tail of list |
| `cons`, `list`, `append`, `reverse` | List construction |
| `length` | List length |
| `map`, `filter`, `foldl` | List transforms |
| `null?`, `symbol?`, `list?`, `string?`, `number?`, `bool?`, `type?` | Predicates |
| `eq`, `not` | Equality, negation |
| `add`, `sub`, `mul`, `lt`, `gt` | Integer arithmetic and comparison |
| `concat` | String concatenation |
| `sym-to-str`, `str-to-sym` | Symbol/string conversion |
| `usym-to-str`, `str-to-usym` | Uninterned symbol conversion |
| `gensym` | Generate a unique symbol |
| `error` | Abort expansion with an error |

## Practical Patterns

- use macros for syntax, not for ordinary computation
- keep expansions small and readable
- use quasiquote by default
- use `gensym` when introducing hidden temporaries
- push actual work into normal functions when possible

## Current Limits

- multi-clause macro definitions are not supported
- macro-writing tools like quasiquote and `quote` are for macro definitions,
  not general runtime code

For PRELUDE macros such as `fun`, `progn`, `if_`, `when`, and `cond`, see
[Standard Library](stdlib.md#macros).
