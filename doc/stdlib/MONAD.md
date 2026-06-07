# MONAD

Import `MONAD` when you want monadic `bind` and the `do-let` macro:

```lisp
(import MONAD)
```

If you want to return a plain value from a `do-let` body, also import `pure`
from `APPLICATIVE`:

```lisp
(import APPLICATIVE (pure))
(import MONAD)
```

The core class is:

```lisp
(class MONAD (APPLICATIVE) (m)
  (bind %(m a) %(-> a (m b)) %(m b)))
```

`MONAD` is a subclass of `APPLICATIVE`, so a monad instance also requires the
matching applicative and functor instances.

## `do-let`

`do-let` is sequential monadic binding sugar:

```lisp
(do-let ((x mx)
         (y my))
  (pure (add x y)))
```

That expands to nested `bind` calls.

The body has an implicit `progn`, so multiple ordinary expressions are fine:

```lisp
(do-let ((x (Just 20)))
  (print "debug")
  (pure (add x 22)))
```

Two constraints matter:

- `do-let` does not implicitly call `pure`
- the final body expression must already be monadic

So nested `do-let` composes naturally:

```lisp
(do-let ((x (Just 20)))
  (do-let ((y (Just 22)))
    (pure (add x y))))
```

## Shipped Instances

Today the stdlib ships `MONAD` instances for:

- `List`
- `Maybe`
- `Either e`
