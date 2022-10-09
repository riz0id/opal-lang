
(let-syntax 
  [(stx-const-true (lambda (stx) #'#t))]
  (stx-const-true #f))

((lambda (x y) x) (syntax #t) #f)

(lambda (x) 
  (case x [(#t) #f] 
          [(#f) #t]))

(lambda (n) 
  (if (<= 0 n) 
    (+ (fib (- n 1)) (fib (- n 2)))
    1))