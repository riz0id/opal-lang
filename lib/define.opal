;; define.opal — user-facing define-style macros.
;;
;; Today this module provides a single working transformer
;; (`define-true`) that demonstrates the macro pipeline end-to-end.
;; The original aspiration — a `plain-define` macro that turns
;; `(plain-define x v)` into the core `(define x v)`, plus a `for`
;; macro that takes `(for (i lo) hi body)` and lowers to a loop —
;; needs template-construction primitives that Opal's transformer
;; language doesn't yet support:
;;
;;   * No `quasi-syntax`/`unsyntax` to splice variables into output
;;     templates.
;;   * No `syntax->list` / `syntax-case` / `car` / `cdr` to
;;     destructure the macro input.
;;
;; A transformer can today only:
;;   * Run lambda calculus over its input syntax (with no introspection
;;     primitives).
;;   * Return a literal syntax object via `(quote-syntax …)`.
;;
;; So the macros below are constant transformers — useful as
;; demonstrations of the pipeline, not as a real user-level macro
;; library. The full set will land once the transformer DSL grows
;; primitives or a template form. Tracked aspirationally below.

(module define

  (import core-bool)

  (export define-true)

  ;; A macro that produces a binding for the symbol `t` with the
  ;; value `true` (imported from `core-bool`). Any call shape works,
  ;; e.g. `(define-true)` or `(define-true anything)` — the macro
  ;; ignores its input and emits the same literal output.
  ;;
  ;; This is the most non-trivial transformer the current language
  ;; supports: it constructs an output that references an *imported*
  ;; identifier (`true`), exercising the import/binding-store path.
  (define-syntax define-true
    (lambda (stx)
      (quote-syntax (define t true)))))

;; -------------------------------------------------------------------
;; Aspirational, not implementable today (kept as documentation):
;;
;;   (define-syntax plain-define
;;     (lambda (stx)
;;       ;; needs: syntax->list, list accessor primitives
;;       (let ([id  (syntax-cadr  stx)]
;;             [rhs (syntax-caddr stx)])
;;         (quasi-syntax (define (unsyntax id) (unsyntax rhs))))))
;;
;;   (define-syntax for
;;     (lambda (stx)
;;       ;; expands (for (i lo) hi body) into a letrec-values loop.
;;       ;; needs: same primitives as plain-define plus a way to
;;       ;; construct multi-form output (a `begin` or `letrec-values`
;;       ;; template).
;;       …))
