# FOLDABLE

Import `FOLDABLE` when you want to reduce a structure down to a single value:

```lisp
(import FOLDABLE)
```

The core class is:

```lisp
(class FOLDABLE () (t)
  (foldr %(-> a b b) b %(t a) b))
```

`FOLDABLE` is independent of `FUNCTOR`, `APPLICATIVE`, and `TRAVERSABLE`.
It only says that a structure can be folded.

## Methods

- `foldr` reduces a structure from the right

## Helpers

- `foldl` builds a left fold on top of `foldr`
- `to-list` rebuilds the contents as a `List`
- `length` counts the number of elements
- `null?` reports whether the structure is empty

Example:

```lisp
(import FOLDABLE (foldr))

(print
  (int-to-str
    (foldr (lam ((x %INT) (acc %INT)) (add x acc))
           0
           (Cons 1 (Cons 2 (Cons 3 Empty))))))
```

## Shipped Instances

Today the stdlib ships `FOLDABLE` instances for:

- `List`
- `Maybe`
- `Either e`
- `Pair e`
