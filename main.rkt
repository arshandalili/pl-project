#lang racket

(require "interpreter.rkt")

(define (evaluate-string program)
  (run program))

(define (evaluate path)
  (run (file->string path)))

(evaluate "./tests/binary-search.txt")