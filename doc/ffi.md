# FFI

Pllisp can call C functions and work with C-facing data layouts directly.
That power is intentionally unsafe. The compiler checks the pllisp-side shape
of your declaration, but it does not prove that your declaration matches the
real C ABI.

This page describes the user-facing FFI surface: declarations, C type names,
structs, arrays, enums, variadic functions, and callbacks.

## Safety First

The important boundaries are:

- `ffi` is usable for fixed-arity scalar and pointer signatures.
- `ffi-var` is intentionally narrow. Fixed parameters must still be scalar or
  pointer types, and extra variadic arguments must have pllisp type `%INT`,
  `%FLT`, `%STR`, or `%BOOL`.
- `ffi-callback` supports scalar and pointer arguments and returns, including
  float callbacks.
- Named struct types like `%Point` and inline array types like `(%ARR 8 %I8)`
  are for `ffi-struct` field declarations only. They are not supported in
  `ffi`, `ffi-var`, or `ffi-callback` signatures. Use `%PTR` when C expects a
  pointer to struct or buffer storage.
- Ownership and lifetime are still manual. If the C side expects memory to
  stay alive, pllisp will not enforce that contract for you.
- Prefer namespaced runtime helper symbols such as `pll-exit` over raw libc
  names in stdlib-style code.

## Declaring C Functions

Use `ffi` for fixed-arity C functions:

```
(ffi sqrt (%FLT) %FLT)
(print (flt-to-str (sqrt 4.0)))
```

Another example:

```
(ffi pow (%FLT %FLT) %FLT)
(print (flt-to-str (pow 2.0 10.0)))
```

Syntax:

```
(ffi name (param-types...) return-type)
```

These declarations are normally written at top level.

## FFI Type Names

In FFI declarations, you can use both pllisp-flavored aliases and explicit C
ABI types.

### Native-Size Aliases

| FFI type | Meaning |
|----------|---------|
| `%INT` | signed 64-bit integer |
| `%FLT` | 64-bit floating-point |
| `%STR` | pointer-compatible string value |
| `%UNIT` | no meaningful value / `void`-like use |

### Explicit C Types

| FFI type | Typical C meaning |
|----------|-------------------|
| `%I8`, `%I16`, `%I32`, `%I64` | signed integer widths |
| `%U8`, `%U16`, `%U32`, `%U64` | unsigned integer widths |
| `%F32`, `%F64` | `float`, `double` |
| `%PTR` | pointer |
| `%VOID` | `void` |

### Struct and Array Types

| FFI type | Meaning |
|----------|---------|
| `%Point` | named struct field type defined with `ffi-struct` |
| `(%ARR 8 %I8)` | inline fixed-size array field |

`%Point` and `(%ARR ...)` belong in `ffi-struct` field lists. They are not
valid function, variadic, or callback ABI types. Use `%PTR` for those cases.

## User-Level Value Behavior

At the pllisp level:

- integer ABI types behave like `%INT`
- floating ABI types behave like `%FLT`
- `%PTR` values behave like strings
- `%VOID` behaves like `unit`

That is why examples like these work:

```
(ffi strlen (%PTR) %I64)
(print (int-to-str (strlen "hello")))
```

```
(ffi puts (%PTR) %I32)
(puts "world")
```

This is a sharp edge worth understanding: pointer-like FFI values are exposed
through the language’s string/pointer representation.

## Variadic Functions

Use `ffi-var` for C variadics:

```
(ffi-var printf (%PTR) %I32)
(printf "%ld" 42)
```

The fixed parameter list goes in the declaration. Extra arguments are supplied
normally at the call site:

```
(ffi-var printf (%PTR) %I32)
(printf "%ld+%ld" 10 20)
```

The compiler rejects variadic signatures that use by-value structs, inline
arrays, or `%VOID` parameters.

It also rejects extra variadic arguments whose pllisp type is not one of:

- `%INT`
- `%FLT`
- `%STR`
- `%BOOL`

That restriction is deliberate. Variadic C calls are the weakest part of the
FFI, so the language keeps the supported surface small.

## Structs

Use `ffi-struct` to define a C-facing struct layout:

```
(ffi-struct Point (x %I32) (y %I32))
```

This gives you:

- a constructor named `Point`
- field access via `(.x value)` and `(.y value)`

Example:

```
(ffi-struct Point (x %I32) (y %I32))

(let ((p (Point 10 20)))
  (print (int-to-str (.x p))))
```

Struct fields can use mixed numeric types:

```
(ffi-struct Rec (a %I64) (b %F64) (c %I32))
```

Accessing them:

```
(let ((r (Rec 42 3.14 7)))
  (print (int-to-str (.a r))))
```

## Arrays in Structs

Fixed-size inline arrays are declared as `(%ARR count %ElemType)`:

```
(ffi-struct Buf (data (%ARR 8 %I8)) (len %I32))
```

Array fields behave like pointer-like string values at the pllisp level. That
makes them useful as writeable C buffers:

Array fields are not passed positionally to the generated constructor. Only the
scalar fields appear as constructor arguments. For this declaration:

```
(ffi-struct Buf (data (%ARR 8 %I8)) (len %I32))
```

the constructor call is:

```
(Buf 4)
```

not a two-argument call, because the inline array storage is part of the
struct itself.

```
(ffi-struct Buf (data (%ARR 8 %I8)) (len %I32))
(ffi-var snprintf (%PTR %I64 %PTR) %I32)

(let ((b (Buf 4)))
  (let ((_ (snprintf (.data b) 8 "hey")))
    (print (.data b))))
```

## Nested Structs

You can use one named struct inside another:

```
(ffi-struct Point (x %I32) (y %I32))
(ffi-struct Line (start %Point) (end %Point))
```

Nested field access works naturally:

```
(let ((p1 (Point 1 2))
      (p2 (Point 3 4)))
  (let ((ln (Line p1 p2)))
    (print (int-to-str (.x (.start ln))))))
```

## Enums

Use `ffi-enum` for named integer constants:

```
(ffi-enum Color (RED 0) (GREEN 1) (BLUE 2))
(print (int-to-str GREEN))
```

Enum variants are ordinary integer-like values at the pllisp level:

```
(ffi-enum Status (OK 0) (ERR 1))

(let ((s OK))
  (print (if (eqi s OK) "success" "failure")))
```

## Strings and Buffers

Pllisp strings can be passed directly to `%PTR` parameters:

```
(ffi strlen (%PTR) %I64)
(print (int-to-str (strlen "hello")))
```

You can also prepare a mutable buffer-like value and let C fill it:

```
(ffi-var snprintf (%PTR %I64 %PTR) %I32)

(let ((buf (substr "xxxxxxxxxxxxxxxxxxxx" 0 20)))
  (let ((_ (snprintf buf 20 "%ld" 42)))
    (print buf)))
```

That example works because `buf` is a pointer-compatible string value.

## Callbacks

Use `ffi-callback` when a C library expects a function pointer:

```
(ffi-callback int-cb (%I64) %I64)
```

That declaration produces a wrapper function. Call it on a pllisp closure to
obtain a C-callable function pointer:

```
(ffi pll_test_apply_int (%PTR %I64) %I64)
(ffi-callback int-cb (%I64) %I64)

(let ((doubler (int-cb (lam (x) (mul x 2)))))
  (print (int-to-str (pll_test_apply_int doubler 21))))
```

Closures may capture surrounding values:

```
(ffi pll_test_apply_int (%PTR %I64) %I64)
(ffi-callback int-cb (%I64) %I64)

(let ((offset 10)
      (adder (int-cb (lam (x) (add x offset)))))
  (print (int-to-str (pll_test_apply_int adder 32))))
```

Float callbacks are supported too:

```
(ffi pll_test_apply_flt64 (%PTR %F64) %F64)
(ffi-callback flt64-cb (%F64) %F64)

(let ((twice (flt64-cb (lam (x) (mulf x 2.0)))))
  (print (flt-to-str (pll_test_apply_flt64 twice 21.25))))
```

So are mixed numeric signatures:

```
(ffi pll_test_apply_mix_num (%PTR %I64 %F64) %F64)
(ffi-callback mix-cb (%I64 %F64) %F64)

(let ((combine (mix-cb (lam (i x) (addf (int-to-flt i) x)))))
  (print (flt-to-str (pll_test_apply_mix_num combine 40 2.5))))
```

The callback boundary is still limited to scalar and pointer ABI types. Use
`%PTR` when a C callback argument is really a pointer to richer data.

## Interop with C Functions that Read or Mutate Structs

Passing a struct value to a `%PTR` parameter lets C code inspect or mutate it:

```
(ffi-struct Point (x %I32) (y %I32))
(ffi pll_point_sum (%PTR) %I64)

(let ((p (Point 7 11)))
  (print (int-to-str (pll_point_sum p))))
```

And mutation:

```
(ffi-struct Point (x %I32) (y %I32))
(ffi pll_point_sum (%PTR) %I64)
(ffi pll_point_scale (%PTR %I64) %VOID)

(let ((p (Point 3 4)))
  (let ((_ (pll_point_scale p 10)))
    (print (int-to-str (pll_point_sum p)))))
```

## Practical Advice

- treat FFI declarations as unsafe escape hatches
- start with simple fixed-arity functions before adding structs or callbacks
- use explicit widths like `%I32` and `%F64` when ABI precision matters
- remember that pointer-like values appear as strings at the pllisp level
- use `%PTR`, not `%Point` or `(%ARR ...)`, in function and callback signatures
- keep variadic calls boring: ints, floats, strings, booleans
- keep FFI declarations near the top of the file
- do not expect FFI declarations to export cleanly across modules right now

## What FFI Does Not Change

The FFI declaration syntax lives alongside normal pllisp code. The rest of the
language still uses normal pllisp types like `%INT`, `%STR`, `%BOOL`, `List`,
`Maybe`, and your own ADTs.
