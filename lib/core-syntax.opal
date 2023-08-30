(module core-syntax
  (import core-bool)

  (export stx-true)

  (define stx-true
    (begin
      (define t true)

      (lambda (stx) t))))