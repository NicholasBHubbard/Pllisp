# Macros

Pllisp has a Common Lisp-style procedural macro system. Macros transform
syntax before type checking, operating on s-expressions as data.

## Defining Macros

```
(mac unless (test body)
  `(if ,test unit ,body))
```

The macro body runs in a dedicated macro interpreter with its own set of
builtins (list processing, arithmetic, string operations). The result
replaces the macro call in the source.

## Quasiquoting

Quasiquote (`` ` ``) builds syntax templates. Unquote (`,`) evaluates and
splices a single value. Unquote-splicing (`,@`) flattens a list into the
surrounding form.

```
(mac progn (&rest args)
  (if (eq (length args) 1)
    (car args)
    `(let ((_ ,(car args))) (progn ,@(cdr args)))))
```

This recursively transforms `(progn a b c)` into
`(let ((_ a)) (let ((_ b)) c))`.

## Macro Parameters

- **Positional**: `(mac name (a b c) ...)`
- **Rest**: `(mac name (&rest args) ...)` — collects all arguments
- **Destructuring**: `(mac name ((a b) c) ...)` — matches list structure

### quote

`quote` prevents evaluation, returning its argument as literal syntax:

```
(mac make-list (&rest xs)
  (let ((build (lam (items)
    (if (null? items)
      (quote Empty)
      `(Cons ,(car items) ,(build (cdr items)))))))
    (build xs)))
```

## Macro-Time vs Runtime

The macro interpreter is a separate evaluation environment. It has its own
`eq` (structural equality on all syntax values), `car`/`cdr`, `length`,
`gensym`, etc. These are not the same as runtime builtins.

For example, `eq` in a macro body compares syntax values — it works on
symbols, lists, numbers, strings, everything. The runtime `eqi` only compares
integers. Macro-time `add` does compile-time arithmetic. They live in
different worlds.

```
# Macro-time: eq compares syntax
(mac is-zero (x) (if (eq x 0) `true `false))

# Runtime: eqi compares integers
(if (eqi n 0) "zero" "nonzero")
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
| `add`, `sub`, `mul`, `lt`, `gt` | Arithmetic |
| `concat` | String concatenation |
| `sym-to-str`, `str-to-sym` | Symbol/string conversion |
| `usym-to-str`, `str-to-usym` | Uninterned symbol conversion |
| `gensym` | Generate unique symbol |
| `error` | Abort expansion with error message |
