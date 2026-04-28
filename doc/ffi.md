# FFI

Pllisp can call C functions and work with C data structures directly.

## Declaring C Functions

```
(ffi strlen (%PTR) %I64)
(ffi puts (%PTR) %I32)
```

Syntax: `(ffi name (param-types...) return-type)`.

## Variadic Functions

```
(ffi-var printf (%PTR) %I32)
```

The parameter list specifies the fixed parameters. Additional arguments
are passed according to C variadic calling conventions.

## Structs

```
(ffi-struct Point (x %I32) (y %I32))
```

This defines a struct layout and generates a constructor. Fields are accessed
with `.field` syntax:

```
(let ((p (Point 10 20)))
  (print (int-to-str (.x p))))
```

Fixed-size arrays use `(%ARR count type)`:

```
(ffi-struct Buf (data (%ARR 256 %I8)) (len %I32))
```

Structs can nest — use another struct type as a field type.

## Enums

```
(ffi-enum Color (RED 0) (GREEN 1) (BLUE 2))
```

Enum variants are integer constants. Compare them with `eqi`:

```
(let ((c RED))
  (if (eqi c GREEN) (print "green") (print "not green")))
```

## Callbacks

Wrap a pllisp closure into a C-callable function pointer:

```
(ffi-callback on-event (%I64) %I64)
```

This allows passing pllisp functions to C libraries that expect function
pointer arguments.

## C Type Mapping

| Pllisp | C |
|--------|---|
| `%I8` / `%I16` / `%I32` / `%I64` | `int8_t` / `int16_t` / `int32_t` / `int64_t` |
| `%U8` / `%U16` / `%U32` / `%U64` | `uint8_t` / `uint16_t` / `uint32_t` / `uint64_t` |
| `%F32` / `%F64` | `float` / `double` |
| `%PTR` | `void*` |
| `%VOID` | `void` |
