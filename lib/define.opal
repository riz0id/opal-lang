;; define.opal — user-facing define-style macros, written using the
;; Stage-1 syntax-inspector primitives.
;;
;; This module is the first real demonstration that the transformer
;; language supports more than constant outputs. `plain-define` takes a
;; macro use `(plain-define <id> <rhs>)` and emits `(define <id> <rhs>)`
;; — substituting the caller's identifier into the output. The
;; mechanism:
;;
;;   1. `(syntax-e stx)` peels one layer of the input wrapper, yielding
;;      `(DatumList [#'plain-define #'<id> #'<rhs>])`.
;;   2. `(cdr (syntax-e stx))` drops the macro's name, leaving the args
;;      `(DatumList [#'<id> #'<rhs>])`.
;;   3. `(cons (quote-syntax define) ...)` prepends the symbol `define`,
;;      giving `(DatumList [#'define #'<id> #'<rhs>])`.
;;   4. `(datum->syntax stx ...)` lifts the datum back to a syntax
;;      object, using `stx`'s lexical info (so the output inherits the
;;      macro call's source location and lexical context).
;;
;; The full set of Stage-1 primitives this exercises:
;;   `syntax-e`, `cdr`, `cons`, `quote-syntax`, `datum->syntax`.

(module define

  (export plain-define)

  (define-syntax plain-define
    (lambda (stx)
      (datum->syntax stx
        (cons (quote-syntax define)
              (cdr (syntax-e stx)))))))
