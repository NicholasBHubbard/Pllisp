# Typeclasses

Typeclasses provide ad-hoc polymorphism.

## Why Use Them

Use a typeclass when:

- several types should support the same operation name
- you want polymorphic helper functions that depend on a capability
- a plain sum type would be the wrong abstraction

Common PRELUDE examples are `EQ`, `ORD`, `TRUTHY`, and `STRINGY`.

## Defining a Class

```
(cls SHOW () (a)
  (show %a %STR))
```

The syntax is:

```
(cls CLASS-NAME (superclasses...) (typevars...) method-signatures...)
```

Each method signature lists argument types followed by the return type:

```
(cls EQUAL () (a)
  (equal %a %a %BOOL)
  (nequal %a %a %BOOL))
```

Rules worth remembering:

- class names are usually written in `UPPER CASE`
- a class needs at least one type variable
- a class needs at least one method

## Defining Instances

```
(inst SHOW %INT
  (show (lam ((x %INT)) (int-to-str x))))
```

Each method in the instance must be a lambda matching the declared method
shape.

For a class with multiple methods:

```
(cls EQUAL () (a)
  (equal %a %a %BOOL)
  (nequal %a %a %BOOL))

(inst EQUAL %INT
  (equal  (lam ((x %INT) (y %INT)) (eqi x y)))
  (nequal (lam ((x %INT) (y %INT)) (not (eqi x y)))))
```

## Calling Methods

Once a class and matching instance are in scope, the method name is used like
an ordinary function:

```
(cls SHOW () (a)
  (show %a %STR))

(inst SHOW %INT
  (show (lam ((x %INT)) (int-to-str x))))

(print (show 42))
```

Methods work inside helper functions too:

```
(let ((display (lam (x) (print (show x)))))
  (display 42))
```

This is the main payoff: the helper stays polymorphic while still demanding a
capability.

## Parametric Instances

Instances can be defined for parameterized types:

```
(inst SHOW %(Maybe a)
  (show (lam ((x %(Maybe a)))
    (case x
      ((Just _) "Just")
      (_ "Nothing")))))
```

This works for any `Maybe a`.

Another common pattern is a capability on a container shape:

```
(cls BOOLISH () (a)
  (boolish? %a %BOOL))

(inst BOOLISH %(List a)
  (boolish? (lam ((xs %(List a)))
    (case xs
      ((Cons _ _) true)
      (_ false)))))
```

## Superclasses

Superclasses go in the first parenthesized list:

```
(cls APPLICATIVE (FUNCTOR) (f)
  (pure %a %(f a))
  (ap %(f %(-> a b)) %(f a) %(f b)))
```

An `APPLICATIVE` instance requires a `FUNCTOR` instance for the same type
constructor.

If the superclass instance is missing, that is an error.

## Higher-Kinded Typeclasses

Typeclass parameters can be type constructors, not just ground types.

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

Here `f` is a type constructor of shape `* -> *`, instantiated to `Maybe` or
`List`.

Another example with a custom type:

```
(type Box (a)
  (MkBox a))

(cls BOXMAP () (f)
  (bmap %(-> a b) %(f a) %(f b)))

(inst BOXMAP %Box
  (bmap (lam ((fn %(-> a b)) (box %(Box a)))
    (case box
      ((MkBox x) (MkBox (fn x)))))))
```

## Good Patterns

- keep classes small and focused
- prefer parametric instances when behavior depends on the outer shape
- keep method names lowercase even when the class name is uppercase
- use PRELUDE classes when they already match the capability you need

## Common Errors

- calling a method with no matching instance in scope
- forgetting a required superclass instance
- using a ground type where a higher-kinded class expects a type constructor
- giving an instance method the wrong lambda shape

## Module Use

Across modules, typeclass methods export normally, but the class declaration
name itself does not currently behave like an exported value. In practice,
import the methods you need and keep the class definition in the providing
module.

See also: [PRELUDE typeclasses](stdlib.md#typeclasses)
