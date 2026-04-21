;;; pllisp-mode.el --- Major mode for editing pllisp source code -*- lexical-binding: t; -*-

(require 'lisp-mode)

(defgroup pllisp nil
  "Major mode for pllisp."
  :group 'languages)

(defconst pllisp-special-forms
  '("lam" "let" "if" "case" "type" "cls" "inst"
    "module" "import" "mac" "fun" "ffi" "ffi-struct"
    "ffi-var" "ffi-enum" "ffi-callback")
  "Special forms and declaration keywords.")

(defconst pllisp-constants
  '("true" "false" "unit")
  "Built-in constants.")

(defconst pllisp-font-lock-keywords
  (let ((special-re (concat "(" (regexp-opt pllisp-special-forms t)))
        (constant-re (regexp-opt pllisp-constants 'words)))
    `(;; Special forms: highlight keyword after open paren
      (,special-re 1 font-lock-keyword-face)
      ;; Constants
      (,constant-re . font-lock-constant-face)
      ;; Type annotations: %Str, %Int, %(List %Int), etc.
      ("%\\(?:(\\([^)]*\\))\\|\\([A-Za-z][A-Za-z0-9_-]*\\)\\)"
       . font-lock-type-face)
      ;; Constructor names: capitalized identifiers
      ("\\<\\([A-Z][A-Za-z0-9_-]*\\)\\>" 1 font-lock-type-face)
      ;; &rest, &key, %opt parameter markers
      ("\\<[&%][A-Za-z][A-Za-z0-9_-]*\\>" . font-lock-preprocessor-face)
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
    ;; % is a symbol constituent (type prefix)
    (modify-syntax-entry ?% "'" st)
    ;; . is a symbol constituent for field access
    (modify-syntax-entry ?. "_ p" st)
    ;; ! is a symbol constituent (set!)
    (modify-syntax-entry ?! "_" st)
    ;; - is a symbol constituent (kebab-case)
    (modify-syntax-entry ?- "_" st)
    st)
  "Syntax table for pllisp-mode.")

;;;###autoload
(define-derived-mode pllisp-mode lisp-mode "Pllisp"
  "Major mode for editing pllisp source code."
  :group 'pllisp
  :syntax-table pllisp-mode-syntax-table
  (setq-local font-lock-defaults '(pllisp-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local lisp-indent-function #'lisp-indent-function))

;; Indent rules: number = distinguished args before body
(dolist (sym '(lam let if case type cls inst mac fun
               module import ffi ffi-struct
               ffi-var ffi-enum ffi-callback))
  (put sym 'lisp-indent-function 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pll\\'" . pllisp-mode))

(provide 'pllisp-mode)
;;; pllisp-mode.el ends here
