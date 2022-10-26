(let-syntax 
  ([false (lambda (_) 
      (define-value f #f)
      (syntax f))])
  false)