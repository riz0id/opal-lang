
(lambda (x) 
  (case x [(#t) #f] 
          [(#f) #t]))

((lambda (x y) x) #t #f)

(lambda (n) 
  (if (<= 0 n) 
    (+ (fib (- n 1)) (fib (- n 2)))
    1))