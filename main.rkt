#lang racket

(require "interpreter.rkt")


(define (evaluate path) (run (file->string path)))


(evaluate "./tests/binary-search.txt")