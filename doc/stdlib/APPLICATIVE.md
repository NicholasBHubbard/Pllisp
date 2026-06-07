# APPLICATIVE

Import `APPLICATIVE` when you want applicative composition helpers and
instances:

```lisp
(import APPLICATIVE)
```

The core class is:

```lisp
(class APPLICATIVE (FUNCTOR) (f)
  (pure %a %(f a))
  (ap %(f %(-> a b)) %(f a) %(f b)))
```

`APPLICATIVE` is a subclass of `FUNCTOR`, so an applicative instance requires
the matching `FUNCTOR` instance for the same type constructor.

## Methods

- `pure` lifts a plain value into the applicative
- `ap` applies an applicative function value to an applicative argument value

Example:

```lisp
(import APPLICATIVE (pure ap))

(case (ap (Just (lam ((x %INT)) (add x 1))) (pure 41))
  ((Just y) (print (int-to-str y)))
  (_ (print "nothing")))
```

## Shipped Instances

Today the stdlib ships `APPLICATIVE` instances for:

- `List`
- `Maybe`
- `Either e`
