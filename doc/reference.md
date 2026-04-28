# Reference

## Reserved Words

These cannot be used as variable names:

`lam`, `let`, `if`, `true`, `false`, `unit`, `type`, `case`, `module`, `import`

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

## Builtin Functions

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

### CLI

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
