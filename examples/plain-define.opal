;; plain-define.opal — end-to-end Stage-1 primitive demonstration.
;;
;; Defines `plain-define` as a syntactic alias for `define`, written
;; using Stage-1 syntax-inspector primitives (syntax-e, cdr, cons,
;; quote-syntax, datum->syntax). Then exercises the alias by
;; introducing `x`. The expander iterates partial-expand on the macro
;; output, so `(plain-define x 42)` lifts to a real `(define x 42)`
;; binding in the module's namespace.
;;
;; Expected namespace after expansion:
;;   defns_variables = { 'x => 42 }

(module plain-define-demo

  (export x)

  ;; The macro — exactly as in lib/define.opal.
  (define-syntax plain-define
    (lambda (stx)
      (datum->syntax stx
        (cons (quote-syntax define)
              (cdr (syntax-e stx))))))

  ;; The macro call expands to `(define x 42)`.
  (plain-define x 42))
