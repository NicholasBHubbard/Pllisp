# Reference

This page is a quick syntax and always-in-scope name cheat sheet.

## Reserved Words

These cannot be used where the program introduces a name, such as `let`
bindings, lambda parameters, optional/rest/keyword parameters, or pattern
variables:

`lam`, `let`, `if`, `true`, `false`, `unit`, `type`, `case`, `module`, `import`

## Literals

| Kind | Example |
|------|---------|
| Integer | `42` |
| Float | `3.14` |
| String | `"hello"` |
| Boolean | `true`, `false` |
| Unit | `unit` |
| Regex | `/[0-9]+/i` |
| Uninterned symbol | `:ok` |

## Type Syntax

| Type form | Example |
|-----------|---------|
| Primitive | `%INT` |
| Compile-time syntax | `%SYNTAX` |
| Function | `%(-> INT INT BOOL)` |
| Parameterized | `%(List INT)` |
| Nested parameterized | `%(Maybe (List STR))` |
| Type variable in user declarations | `a`, `b`, `f` |

`%SYNTAX` is a compile-time-only type. Use it in macro bodies and
`eval-when (:compile-toplevel ...)` helper code, not in ordinary runtime code.

## Special Forms

| Form | Syntax |
|------|--------|
| Lambda | `(lam (params...) body)` |
| Let | `(let ((name expr)...) body)` |
| If | `(if cond then else)` |
| Case | `(case expr (pattern body)...)` |
| Syntax-case | `(syntax-case expr (pattern body)...)` |
| Type | `(type Name (tyvars...) (Ctor args...)...)` |
| Class | `(cls CLASS (supers...) (tyvars...) (method %types...)...)` |
| Instance | `(inst CLASS %Type (method impl)...)` |
| Macro | `(mac name (params...) body)` |
| Eval-when | `(eval-when (phases...) form...)` |
| Module | `(module MODULE)` |
| Import | `(import MODULE [Alias] [(unquals...)])` |
| Field access | `(.field expr)` |
| FFI | `(ffi name [(:link-name "external_symbol")] (param-types...) ret-type)` |
| FFI variadic | `(ffi-var name [(:link-name "external_symbol")] (fixed-param-types...) ret-type)` |
| FFI struct | `(ffi-struct Name (field %CType)...)` |
| FFI enum | `(ffi-enum Name (Variant value)...)` |
| FFI callback | `(ffi-callback name (param-types...) ret-type)` |

`syntax-case` is compile-time only. Use it in macro bodies and
`eval-when (:compile-toplevel ...)` helper code.

FFI is unsafe by design. Function and callback signatures support scalar and
pointer ABI types. Named struct types like `%Point` and array types like
`(%ARR 8 %I8)` belong in `ffi-struct` field declarations, not in `ffi`,
`ffi-var`, or `ffi-callback` signatures. Variadic extra arguments are limited
to `%INT`, `%FLT`, `%STR`, and `%BOOL` values. Prefer raw foreign binding
names like `c-sqrt` plus `:link-name` for the real external symbol.

## Extended Lambda Lists

| Feature | Example |
|---------|---------|
| Required params | `(lam (x y) ...)` |
| Typed params | `(lam ((x %INT) (y %INT)) ...)` |
| Return annotation | `(lam ((x %INT)) %STR ...)` |
| Optional params | `(lam (x %opt (y 0)) ...)` |
| Rest param | `(lam (x &rest xs) ...)` |
| Keyword params | `(lam (&key (width 80) (height 24)) ...)` |

At call sites, keyword arguments are written as:

```
(f &key width 120 &key height 40)
```

## Import Forms

| Form | Meaning |
|------|---------|
| `(import MATH)` | qualified access only |
| `(import MATH (square))` | also bring `square` into scope |
| `(import MATH M)` | use alias `M` |
| `(import MATH M (square))` | alias + unqualified import |

## Patterns

| Pattern | Example | Matches |
|---------|---------|---------|
| Wildcard | `_` | Anything |
| Variable | `x` | Anything (binds) |
| Integer | `42` | Equal integer |
| Float | `3.14` | Equal float |
| String | `"hi"` | Equal string |
| Boolean | `true` | Equal boolean |
| Uninterned symbol | `:foo` | Equal usym |
| Constructor | `(Just x)` | ADT constructor |

## Primitive Runtime Functions

### Arithmetic

| Function | Type |
|----------|------|
| `add`, `sub`, `mul`, `div`, `mod` | `INT -> INT -> INT` |
| `addf`, `subf`, `mulf`, `divf` | `FLT -> FLT -> FLT` |

### Comparison

| Function | Type |
|----------|------|
| `eqi`, `lti` | `INT -> INT -> BOOL` |
| `eqf`, `ltf` | `FLT -> FLT -> BOOL` |
| `eqs`, `lts` | `STR -> STR -> BOOL` |

### String

| Function | Type |
|----------|------|
| `concat` | `STR -> STR -> STR` |
| `strlen` | `STR -> INT` |
| `substr` | `STR -> INT -> INT -> STR` |

### Conversion

| Function | Type |
|----------|------|
| `int-to-flt` | `INT -> FLT` |
| `flt-to-int` | `FLT -> INT` |
| `usym-to-str` | `USYM -> STR` |
| `str-to-usym` | `STR -> USYM` |

### References

| Function | Type |
|----------|------|
| `ref` | `a -> Ref a` |
| `deref` | `Ref a -> a` |
| `set!` | `Ref a -> a -> UNIT` |

## Implicit PRELUDE Macros

`fun`, `progn`, `foreach`, `if_`, `when`, `unless`, `cond`, `if-let`,
`when-let`, `unless-let`, `and`, `or`

## Implicit PRELUDE Functions and Methods

These are not primitive runtime functions, but they are still in scope through
the implicit `PRELUDE`.

### Derived Scalar Helpers

| Function | Type |
|----------|------|
| `not` | `BOOL -> BOOL` |
| `neg` | `INT -> INT` |
| `negf` | `FLT -> FLT` |
| `gti`, `lei`, `gei` | `INT -> INT -> BOOL` |
| `gtf`, `lef`, `gef` | `FLT -> FLT -> BOOL` |
| `gts`, `les`, `ges` | `STR -> STR -> BOOL` |
| `str-contains` | `STR -> STR -> BOOL` |

### I/O and Command Line

| Function | Type |
|----------|------|
| `print` | `STR -> UNIT` |
| `read-line` | `UNIT -> STR` |
| `is-eof` | `UNIT -> BOOL` |
| `argc` | `UNIT -> INT` |
| `argv` | `INT -> STR` |

### Formatting and Regex

| Function | Type |
|----------|------|
| `int-to-str` | `INT -> STR` |
| `flt-to-str` | `FLT -> STR` |
| `rx-match` | `RX -> STR -> BOOL` |
| `rx-find` | `RX -> STR -> STR` |
| `rx-sub` | `RX -> STR -> STR -> STR` |
| `rx-gsub` | `RX -> STR -> STR -> STR` |
| `rx-split` | `RX -> STR -> List STR` |
| `rx-captures` | `RX -> STR -> List STR` |
| `rx-compile` | `STR -> RX` |

### Typeclass Methods

`truthy`, `eq`, `lt`, `gt`, `le`, `ge`, `str`

See [Standard Library](stdlib.md) for details and examples.

## Compile-Time Syntax API

These are available inside macro definitions and
`eval-when (:compile-toplevel ...)` helper code. Compile-time code can also
call ordinary runtime bindings when those bindings can actually be evaluated at
macro expansion time. In practice that includes pure functions, references, and
regex helpers. FFI-backed runtime bindings are still unavailable at macro
expansion time.

### Syntax Constructors and Conversions

`syntax-lift`, `syntax-symbol`, `syntax-raw-symbol`, `syntax-int`, `syntax-float`, `syntax-string`,
`syntax-bool`, `syntax-usym`, `syntax-rx`, `syntax-type`, `syntax-empty`, `syntax-cons`,
`syntax-append`

### Syntax Inspectors and Predicates

`syntax-car`, `syntax-cdr`, `syntax-length`, `syntax-equal?`,
`syntax-null?`, `syntax-symbol?`, `syntax-list?`, `syntax-string?`,
`syntax-number?`, `syntax-bool?`, `syntax-type?`, `syntax-int-value`,
`syntax-float-value`, `syntax-string-value`, `syntax-symbol-name`,
`syntax-usym-name`

### Compile-Time Helpers

`append`, `reverse`, `map`, `filter`, `foldl`, `error`

See [Macros](macros.md) for usage patterns.

`syntax-symbol` participates in automatic hygiene. Use `syntax-raw-symbol`
only when you intentionally need to introduce a user-visible binding name.
