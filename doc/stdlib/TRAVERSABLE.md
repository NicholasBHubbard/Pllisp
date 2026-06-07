# TRAVERSABLE

Import `TRAVERSABLE` when you want to walk a structure left to right while
threading effects through an `APPLICATIVE`:

```lisp
(import TRAVERSABLE)
```

The core class is:

```lisp
(class TRAVERSABLE (FUNCTOR) (t)
  (traverse ((APPLICATIVE f)) %(-> a (f b)) %(t a) %(f %(t b))))
```

`TRAVERSABLE` is a subclass of `FUNCTOR`. Its `traverse` method also requires
an `APPLICATIVE` instance for the effect constructor `f`.

## Methods

- `traverse` maps each element to an applicative effect and rebuilds the
  original shape inside that effect

Example:

```lisp
(import TRAVERSABLE (traverse))

(case (traverse (lam ((x %INT)) (Just (add x 1)))
                (Cons 41 Empty))
  ((Just ys)
    (case ys
      ((Cons y Empty) (print (int-to-str y)))
      (_ (print "bad"))))
  ((Nothing) (print "nothing")))
```

## Shipped Instances

Today the stdlib ships `TRAVERSABLE` instances for:

- `List`
- `Maybe`
- `Either e`
- `Pair e`
