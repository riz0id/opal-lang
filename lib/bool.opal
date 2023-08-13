
(module bool
  (import opal)

  (export true)

  (define-syntax not (lambda (x) (quote-syntax #f)))

  (define x (not #t))
)