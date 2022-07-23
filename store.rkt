#lang racket

(provide (all-defined-out))

  
(define STORE 'uninitialized)


(define initialize-store!
  (lambda ()
    (set! STORE '())))


(define newref
  (lambda (val)
    (let ([new-ref (length STORE)])
      (set! STORE (append STORE (list val)))
      new-ref)))


(define deref
  (lambda (ref)
    (list-ref STORE ref)))


(define setref!
  (lambda (ref val)
    (set! STORE
          (letrec ([setref-inner (lambda (store1 ref1)
                                   (cond [(null? store1) (error "Invalid reference")]
                                         [(zero? ref1) (cons val (cdr store1))]
                                         [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
            (setref-inner STORE ref)))))