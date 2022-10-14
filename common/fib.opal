
(let-syntax 
  ([true (syntax #t)])
  (let-syntax 
    ([test (lambda (_) 
      (syntax-local-value #'true))])
    test))
