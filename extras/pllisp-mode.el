;;; pllisp-mode.el --- Major mode for editing pllisp source code -*- lexical-binding: t; -*-

(require 'lisp-mode)

(defgroup pllisp nil
  "Major mode for pllisp."
  :group 'languages)

(defconst pllisp-special-forms
  '("lam" "let" "if" "case" "type" "cls" "inst"
    "module" "import" "mac" "fun" "ffi" "ffi-struct"
    "ffi-var" "ffi-enum" "ffi-callback" "progn")
  "Special forms and declaration keywords.")

(defconst pllisp-builtins
  '("add" "sub" "mul" "div" "mod" "neg"
    "addf" "subf" "mulf" "divf" "negf"
    "eqi" "lt" "gt" "le" "ge"
    "eqf" "ltf" "gtf" "lef" "gef"
    "eqs" "and" "or" "not"
    "concat" "strlen" "substr" "str-contains"
    "print" "read-line" "is-eof"
    "argc" "argv"
    "int-to-flt" "flt-to-int" "int-to-str" "flt-to-str"
    "usym-to-str" "str-to-usym"
    "rx-match" "rx-find" "rx-sub" "rx-gsub"
    "rx-split" "rx-captures" "rx-compile"
    "ref" "deref" "set!"
    "gc-collect" "gc-heap-size")
  "Built-in functions.")

(defconst pllisp-constants
  '("true" "false" "unit")
  "Built-in constants.")

(defconst pllisp-font-lock-keywords
  (let ((special-re (concat "(" (regexp-opt pllisp-special-forms t) "\\_>"))
        (builtin-re (concat "(" (regexp-opt pllisp-builtins t) "\\_>"))
        (constant-re (regexp-opt pllisp-constants 'words)))
    `(;; Special forms: keyword after open paren
      (,special-re 1 font-lock-keyword-face)
      ;; Builtin functions: after open paren
      (,builtin-re 1 font-lock-builtin-face)
      ;; Constants: true, false, unit
      (,constant-re . font-lock-constant-face)
      ;; Uninterned symbols: :foo
      ("\\<:\\([A-Za-z][A-Za-z0-9_-]*\\)" . font-lock-constant-face)
      ;; Regex literals: /pattern/flags
      ("\\(/[^/\n]*/[imsx]*\\)" 1 font-lock-string-face)
      ;; Type annotations: %INT, %(List INT), etc.
      ("%\\(?:(\\([^)]*\\))\\|\\([A-Za-z][A-Za-z0-9_-]*\\)\\)"
       . font-lock-type-face)
      ;; ALL CAPS: typeclasses, modules, primitive types (2+ uppercase chars, no lowercase)
      ("\\<\\([A-Z][A-Z0-9_-]+\\)\\>" 1 font-lock-type-face)
      ;; TitleCase: constructors and type names (uppercase start, has lowercase)
      ("\\<\\([A-Z][a-z][A-Za-z0-9_-]*\\)\\>" 1 font-lock-function-name-face)
      ;; &rest, &key, %opt parameter markers
      ("\\(?:^\\|[[:space:](]\\)\\([&%][A-Za-z][A-Za-z0-9_-]*\\)" 1 font-lock-preprocessor-face)
      ;; .field access
      ("\\<\\.[a-z][A-Za-z0-9_-]*\\>" . font-lock-variable-name-face)))
  "Font-lock rules for pllisp.")

(defvar pllisp-mode-syntax-table
  (let ((st (make-syntax-table lisp-mode-syntax-table)))
    ;; # starts line comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; ; is not a comment character in pllisp
    (modify-syntax-entry ?\; "_" st)
    ;; % is a symbol constituent (type prefix, %opt)
    (modify-syntax-entry ?% "'" st)
    ;; & is a symbol constituent (&rest, &key)
    (modify-syntax-entry ?& "_" st)
    ;; . is a symbol constituent for field access
    (modify-syntax-entry ?. "_ p" st)
    ;; ! is a symbol constituent (set!)
    (modify-syntax-entry ?! "_" st)
    ;; - is a symbol constituent (kebab-case)
    (modify-syntax-entry ?- "_" st)
    st)
  "Syntax table for pllisp-mode.")

(defun pllisp--indent-function (_indent-point state)
  "Pllisp indent: always 2 from the indentation of the containing form's line."
  (save-excursion
    (goto-char (elt state 1))
    (back-to-indentation)
    (+ 2 (current-column))))

(defun pllisp-indent-line (&optional _whole-exp)
  "Indent current line as pllisp code."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (null indent)
        (setq indent (current-column))
      (when (listp indent) (setq indent (car indent))))
    (setq shift-amt (- indent (current-column)))
    (unless (zerop shift-amt)
      (delete-region beg (point))
      (indent-to indent))
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

;;;###autoload
(define-derived-mode pllisp-mode lisp-mode "Pllisp"
  "Major mode for editing pllisp source code."
  :group 'pllisp
  :syntax-table pllisp-mode-syntax-table
  (setq-local font-lock-defaults '(pllisp-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local lisp-body-indent 2)
  (setq-local indent-line-function #'pllisp-indent-line)
  (setq-local lisp-indent-function #'pllisp--indent-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pll\\'" . pllisp-mode))

(provide 'pllisp-mode)
;;; pllisp-mode.el ends here
