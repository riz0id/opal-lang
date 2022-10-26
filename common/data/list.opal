
(let-syntax 
  ([stx #'(list #t #f)])
  (let-syntax 
    ([macro (lambda (_) #'(syntax-e stx))])
    macro))
    
(syntax-e #'())

(let-syntax 
  ([g (lambda (_) 
    (syntax (gensym (quote base))))])
  g)

(let-syntax 
  ([stx #'(list #t #f)])
  (let-syntax 
    ([macro (lambda (_) 
      #'(syntax-e stx))])
    macro))

(syntax->datum (syntax #t))

(quasisyntax (unsyntax (syntax #t)))

(let-syntax 
  ([false (syntax #f)]
   [true  (syntax #t)])
  (let-syntax
    ([truth-values (lambda (stx)
      (quasisyntax (list #,(syntax-local-value #'true)
                         #,(syntax-local-value #'false))))])
    truth-values))