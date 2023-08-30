(module define

  (export
    define)

  (define-syntax plain-define
    (lambda (stx) (define (list)))

)