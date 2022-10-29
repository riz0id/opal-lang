
(module
  
  (define-value syntax/head
    (lambda (stx)
      (if (syntax? stx)
          (head (syntax-e stx))
          (head stx))))

  (syntax/head (quote-syntax a))
  
  )