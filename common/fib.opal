(let-syntax 
  ([true (syntax #t)]
   [test (syntax-local-value #'true)])
  test)

   [test (lambda (_) (syntax-local-value #'true))])