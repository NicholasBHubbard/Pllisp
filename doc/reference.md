# Reference

This page is a quick syntax and builtin cheat sheet.

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
| Function | `%(-> INT INT BOOL)` |
| Parameterized | `%(List INT)` |
| Nested parameterized | `%(Maybe (List STR))` |
| Type variable in user declarations | `a`, `b`, `f` |

## Special Forms

| Form | Syntax |
|------|--------|
| Lambda | `(lam (params...) body)` |
| Let | `(let ((name expr)...) body)` |
| If | `(if cond then else)` |
| Case | `(case expr (pattern body)...)` |
| Type | `(type Name (tyvars...) (Ctor args...)...)` |
| Class | `(cls CLASS (supers...) (tyvars...) (method %types...)...)` |
| Instance | `(inst CLASS %Type (method impl)...)` |
| Macro | `(mac name (params...) body)` |
| Module | `(module MODULE)` |
| Import | `(import MODULE [Alias] [(unquals...)])` |
| Field access | `(.field expr)` |
| FFI | `(ffi name (param-types...) ret-type)` |
| FFI variadic | `(ffi-var name (fixed-param-types...) ret-type)` |
| FFI struct | `(ffi-struct Name (field %CType)...)` |
| FFI enum | `(ffi-enum Name (Variant value)...)` |
| FFI callback | `(ffi-callback name (param-types...) ret-type)` |

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

## Runtime Builtins

### Arithmetic

| Function | Type |
|----------|------|
| `add`, `sub`, `mul`, `div`, `mod` | `INT -> INT -> INT` |
| `neg` | `INT -> INT` |
| `addf`, `subf`, `mulf`, `divf` | `FLT -> FLT -> FLT` |
| `negf` | `FLT -> FLT` |

### Comparison

| Function | Type |
|----------|------|
| `eqi`, `lti`, `gti`, `lei`, `gei` | `INT -> INT -> BOOL` |
| `eqf`, `ltf`, `gtf`, `lef`, `gef` | `FLT -> FLT -> BOOL` |
| `eqs`, `lts`, `gts`, `les`, `ges` | `STR -> STR -> BOOL` |

### Boolean

| Function | Type |
|----------|------|
| `and`, `or` | `BOOL -> BOOL -> BOOL` |
| `not` | `BOOL -> BOOL` |

### String

| Function | Type |
|----------|------|
| `concat` | `STR -> STR -> STR` |
| `strlen` | `STR -> INT` |
| `substr` | `STR -> INT -> INT -> STR` |
| `str-contains` | `STR -> STR -> BOOL` |

### I/O

| Function | Type |
|----------|------|
| `print` | `STR -> UNIT` |
| `read-line` | `UNIT -> STR` |
| `is-eof` | `UNIT -> BOOL` |

### Command Line

| Function | Type |
|----------|------|
| `argc` | `UNIT -> INT` |
| `argv` | `INT -> STR` |

### Conversion

| Function | Type |
|----------|------|
| `int-to-flt` | `INT -> FLT` |
| `flt-to-int` | `FLT -> INT` |
| `int-to-str` | `INT -> STR` |
| `flt-to-str` | `FLT -> STR` |
| `usym-to-str` | `USYM -> STR` |
| `str-to-usym` | `STR -> USYM` |

### Regex

| Function | Type |
|----------|------|
| `rx-match` | `RX -> STR -> BOOL` |
| `rx-find` | `RX -> STR -> STR` |
| `rx-sub` | `RX -> STR -> STR -> STR` |
| `rx-gsub` | `RX -> STR -> STR -> STR` |
| `rx-split` | `RX -> STR -> List STR` |
| `rx-captures` | `RX -> STR -> List STR` |
| `rx-compile` | `STR -> RX` |

### References

| Function | Type |
|----------|------|
| `ref` | `a -> Ref a` |
| `deref` | `Ref a -> a` |
| `set!` | `Ref a -> a -> UNIT` |

### GC

| Function | Type |
|----------|------|
| `gc-collect` | `UNIT -> UNIT` |
| `gc-heap-size` | `UNIT -> INT` |

## PRELUDE Convenience Names

These are not hardwired runtime builtins, but they are in scope through the
implicit PRELUDE.

### PRELUDE Macros

`fun`, `progn`, `if_`, `when`, `unless`, `cond`, `if-let`, `when-let`,
`unless-let`

### PRELUDE Typeclass Methods

`truthy`, `eq`, `lt`, `gt`, `le`, `ge`, `str`

See [Standard Library](stdlib.md) for details and examples.

## Macro-Time Builtins

These are available inside macro definitions:

`car`, `cdr`, `cons`, `list`, `append`, `reverse`, `length`, `map`, `filter`,
`foldl`, `null?`, `symbol?`, `list?`, `string?`, `number?`, `bool?`, `type?`,
`eq`, `not`, `add`, `sub`, `mul`, `lt`, `gt`, `concat`, `sym-to-str`,
`str-to-sym`, `usym-to-str`, `str-to-usym`, `gensym`, `error`

See [Macros](macros.md) for usage patterns.
