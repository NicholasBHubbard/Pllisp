# CLI

Import `CLI` when you want top-level command-line bindings generated from a
declarative spec:

```
(import CLI)
```

The core form is the `CLI` macro. It is a top-level declaration form, not a
parser-building function.

Supported clauses:

- `(:flag :name "-s" "--long")`
- `(:option :name "-s" "--long")`
- `(:arg :name)`
- `(:rest :name)`

Each clause name is a usym. `CLI` derives an ordinary top-level binding from
that usym:

- `:flag` binds a `%BOOL`
- `:option` binds a `%(Maybe %STR)`
- `:arg` binds a `%STR`
- `:rest` binds a `%(List %STR)`

Example:

```lisp
(import CLI)

(fun list-len ((xs %(List %STR))) %INT
  (case xs
    ((Empty) 0)
    ((Cons _ rest) (add 1 (list-len rest)))))

(CLI
  (:flag :verbose "-v" "--verbose")
  (:option :output "-o" "--output")
  (:arg :mode)
  (:arg :input)
  (:rest :extras))

(print (if verbose "true" "false"))
(case output
  ((Just path) (print path))
  (_ (print "missing")))
(print mode)
(print input)
(print (int-to-str (list-len extras)))
```

At expansion time, `CLI` splices in:

- one hidden binding that parses `argv`
- one visible top-level binding per declared clause

That means later top-level forms in the same file can use `verbose`, `output`,
`mode`, `input`, and `extras` directly.

Notes:

- there is no `required-option`; if a value is required, make it a positional
  `:arg`
- `:flag` produces a boolean
- `:option` produces `%(Maybe %STR)`
- `:arg` consumes one positional argument
- `:rest` consumes the remaining positional arguments and must be last
- `--` stops option parsing; everything after it is treated as positional
- unknown options terminate the program with a parse error
- missing positional arguments terminate the program with a parse error
