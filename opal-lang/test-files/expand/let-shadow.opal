(let-syntax 
    [id (lambda lambda (syntax #t))]
  (let-syntax 
      [id (lambda let-syntax ((syntax-local-value (syntax id)) let-syntax))]
    (id #t)))
