(module syntax
  (import bool)
  (export)
  (module-begin

    (define-syntax stx-true
      (lambda (stx) (quote-syntax true)))

    (define-syntax stx-false
      (lambda (stx) (quote-syntax false)))

  )
)