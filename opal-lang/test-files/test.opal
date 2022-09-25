
'(#t #f #t #f)

(lambda (x y) 
  (let-syntax 
    [const (lambda stx #'x)]
    (const y)))

(let-syntax 
  [(const-stx (lambda (x stx) x))]
  (const-stx #t #f))