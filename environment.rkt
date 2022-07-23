#lang racket

(require (lib "eopl.ss" "eopl"))

(provide (all-defined-out))


(define-datatype environment environment?
  (empty-env)
  (extended-env
   (bvar symbol?)
   (bval integer?)
   (saved-env environment?)))


(define apply-env
  (lambda (search-var env)
    (cases environment env
      [empty-env () null]
      [extended-env (bvar bval saved-env)
                    (if (eqv? search-var bvar) bval (apply-env search-var saved-env))])))