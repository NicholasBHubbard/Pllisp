# Typeclasses

Typeclasses provide ad-hoc polymorphism, compiled to dictionary passing.

## Defining a Class

```
(cls SHOW () (a)
  (show %a %STR))
```

The syntax is `(cls Name (superclasses...) (typevars...) methods...)`.

Each method signature lists its argument types and return type:

```
(cls EQUAL () (a)
  (equal %a %a %BOOL)
  (nequal %a %a %BOOL))
```

## Superclasses

Superclasses go in the first parenthesized list:

```
(cls APPLICATIVE (FUNCTOR) (f)
  (pure %a %(f a))
  (ap %(f (-> a b)) %(f a) %(f b)))
```

An `APPLICATIVE` instance requires a `FUNCTOR` instance for the same type.

## Defining Instances

```
(inst SHOW %INT
  (show (lam ((x %INT)) (int-to-str x))))
```

Each method in the instance must be a lambda matching the class method's
type signature.

## Parametric Instances

Instances can be defined for parameterized types:

```
(inst SHOW %(Maybe a)
  (show (lam ((x %(Maybe a)))
    (case x
      ((Just v) (concat "Just(" (concat (show v) ")")))
      (_ "Nothing")))))
```

This works for any `Maybe a` where `a` itself has a `SHOW` instance. The
compiler resolves the inner `(show v)` call through dictionary passing.

## Higher-Kinded Types

Type class parameters can be type constructors, not just ground types.
This enables abstractions like `FUNCTOR`:

```
(cls FUNCTOR () (f)
  (map %(-> a b) %(f a) %(f b)))

(inst FUNCTOR %Maybe
  (map (lam ((fn %(-> a b)) (mx %(Maybe a)))
    (case mx
      ((Just x) (Just (fn x)))
      (_ Nothing)))))

(inst FUNCTOR %List
  (map (lam ((fn %(-> a b)) (xs %(List a)))
    (case xs
      ((Empty) Empty)
      ((Cons h t) (Cons (fn h) (map fn t)))))))
```

Here `f` is a type constructor (kind `* -> *`), instantiated to `Maybe` or
`List`.

## How Typeclasses Compile

Instance resolution happens entirely at compile time. When the compiler can
see the concrete type at a call site, it inlines the instance method directly —
`(show 42)` becomes a direct call to the `SHOW %INT` implementation with no
indirection.

For polymorphic functions, dictionaries are passed as extra arguments. A
function like:

```
(let ((print-it (lam (x) (print (show x)))))
  (print-it 42))
```

compiles `print-it` to take an additional dictionary parameter. At the call
site `(print-it 42)`, the compiler passes the `SHOW %INT` dictionary. The
dictionary is a compile-time-resolved value, not a vtable lookup.

See also: [PRELUDE typeclasses](stdlib.md#typeclasses)
