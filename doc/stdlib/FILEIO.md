# FILEIO

Import `FILEIO` when you want file and directory operations:

```lisp
(import FILEIO)
```

`FILEIO` has two layers:

- whole-file helpers like `slurp` and `spurt`
- typed file handles plus scoped macros like `with-input`

Prefer the whole-file layer first. Use explicit handles when you actually need
streaming behavior.

## Error Type

Most `FILEIO` operations return `Either`:

```lisp
(type FileError ()
  (FileError %USYM %STR %STR))
```

Accessors:

- `error-op`
- `error-path`
- `error-message`

Example:

```lisp
(import FILEIO (slurp error-message))

(case (slurp "config.txt")
  ((Right text) (print text))
  ((Left err) (print (error-message err))))
```

## Whole-File Helpers

- `slurp : STR -> Either FileError STR`
- `lines : STR -> Either FileError (List STR)`
- `spurt : STR -> STR -> Either FileError UNIT`
- `append-file : STR -> STR -> Either FileError UNIT`
- `spurt-lines : STR -> List STR -> Either FileError UNIT`
- `append-lines : STR -> List STR -> Either FileError UNIT`

These names are intentionally perlish:

- `slurp` reads the whole file
- `spurt` overwrites the whole file
- `append-file` appends text to the end

`append-file` is named that way because plain `append` is already a compile-time
helper in the macro system.

## File Handles

`FILEIO` exposes three handle types:

- `InFile`
- `OutFile`
- `AppendFile`

Open/close functions:

- `open-in`, `close-in`
- `open-out`, `close-out`
- `open-append`, `close-append`

Reading and writing:

- `next-line : InFile -> Either FileError (Maybe STR)`
- `read-all : InFile -> Either FileError STR`
- `eof? : InFile -> Either FileError BOOL`
- `write : OutFile -> STR -> Either FileError UNIT`
- `write-append : AppendFile -> STR -> Either FileError UNIT`
- `say : OutFile -> STR -> Either FileError UNIT`
- `say-append : AppendFile -> STR -> Either FileError UNIT`
- `flush-out : OutFile -> Either FileError UNIT`
- `flush-append : AppendFile -> Either FileError UNIT`

`next-line` returns:

- `Left err` for an actual read failure
- `Right Nothing` at EOF
- `Right (Just line)` for a line

The name is `next-line`, not `read-line`, because `read-line` is already the
implicit PRELUDE function for stdin.

## Scoped File Macros

Use the handle macros for ergonomic block-scoped file access:

- `with-input`
- `with-output`
- `with-append`
- `foreach-line`

Example:

```lisp
(import FILEIO (write say flush-out error-message))

(case (with-output (fh "out.txt")
        (write fh "alpha")
        (say fh "beta")
        (flush-out fh))
  ((Right _) unit)
  ((Left err) (print (error-message err))))
```

`with-input`, `with-output`, and `with-append` are not ordinary sequencing
macros. Each body form must return `Either FileError a`. They short-circuit on
`Left`, then still close the handle before returning.

`foreach-line` is the easiest way to read a text file line by line:

```lisp
(import FILEIO (spurt unlink error-message))

(case (spurt "/tmp/example.txt" "red\nblue\n")
  ((Left err) (print (error-message err)))
  ((Right _)
    (case (foreach-line (line "/tmp/example.txt")
            (print line))
      ((Left err) (print (error-message err)))
      ((Right _)
        (unlink "/tmp/example.txt")))))
```

Plain `(import FILEIO)` is enough to make the `FILEIO` macros available. If you
also want unqualified runtime names like `slurp` or `error-message`, list them
explicitly in the import form.

A handle-oriented example:

```lisp
(import FILEIO (write say flush-out next-line eof? error-message))

(fun tap-line (result)
  (case result
    ((Left err) (Left err))
    ((Right maybe-line)
      (case maybe-line
        ((Nothing) (Right unit))
        ((Just line)
          (progn
            (print line)
            (Right unit)))))))

(case (with-output (fh "/tmp/report.txt")
        (write fh "alpha")
        (say fh "beta")
        (flush-out fh))
  ((Left err) (print (error-message err)))
  ((Right _)
    (case (with-input (fh "/tmp/report.txt")
            (tap-line (next-line fh))
            (tap-line (next-line fh)))
      ((Left err) (print (error-message err)))
      ((Right _) unit))))
```

## Filesystem Helpers

- `exists?`
- `is-file?`
- `is-dir?`
- `stat`
- `readdir`
- `mkdir`
- `mkdir-p`
- `unlink`
- `rename`
- `copy`

`stat` returns `Either FileError FileInfo`:

```lisp
(type FileKind ()
  (RegularFile)
  (Directory)
  (Other))

(type FileInfo ()
  (FileInfo %STR %FileKind %INT))
```

Accessors:

- `info-path`
- `info-kind`
- `info-size`
