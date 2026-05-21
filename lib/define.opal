(module define


  (export
    define)

  (define-syntax plain-define
    (lambda (stx) (define (list)))

  ;; (for (i 1) 10
  ;;   (print i))
  (define-syntax (for stx)
    (let [(stx-arg1 (car stx))
          (idx-name (car stx-arg1))
          (idx-start (cdr stx-arg1))
          (idx-end (car (cdr stx-arg1)))
          (loop-body (car (cdr (cdr stx))))]
      
      )

)