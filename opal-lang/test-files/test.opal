
(let [(x #t)] x)

(let 
  [(const (lambda (x y) x))]
  (const #t #t))

(let-syntax 
  [(const-stx (lambda (x stx) x))]
  (const-stx #t #f))