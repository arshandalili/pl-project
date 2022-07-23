#lang racket

(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(provide (all-defined-out))


(define-datatype value value?
  (none-value)
  (normal-value
   (val (lambda (x) #t)))
  (lazy-value
   (exp expression?)
   (env environment?))
  (func-value
   (func function-definition-statement?)))
   

(define-datatype program program?
  (a-program
   (stmts statements?)))


(define-datatype statements statements?
  (statements-single
   (stmt statement?))
  (statements-multiple
   (stmts statements?)
   (stmt statement?)))


(define-datatype statement statement?
  (statement-simple
   (stmt simple-statement?))
  (statement-compound
   (stmt compound-statement?)))


(define-datatype simple-statement simple-statement?
  (simple-statement-assignment
   (stmt assignment-statement?))
  (simple-statement-global
   (stmt global-statement?))
  (simple-statement-return
   (stmt return-statement?))
  (simple-statement-pass
   (stmt pass-statement?))
  (simple-statement-break
   (stmt break-statement?))
  (simple-statement-continue
   (stmt continue-statement?))
  (simple-statement-print
   (stmt print-statement?)))


(define-datatype compound-statement compound-statement?
  (compound-statement-function-definition
   (stmt function-definition-statement?))
  (compound-statement-if
   (stmt if-statement?))
  (compound-statement-for
   (stmt for-statement?)))


(define-datatype assignment-statement assignment-statement?
  (an-assignment-statement
   (id symbol?)
   (exp expression?)))


(define-datatype global-statement global-statement?
  (a-global-statement
   (id symbol?)))


(define-datatype return-statement return-statement?
  (return-statement-void)
  (return-statement-with-value
   (exp expression?)))


(define-datatype pass-statement pass-statement?
  (a-pass-statement))


(define-datatype break-statement break-statement?
  (a-break-statement))


(define-datatype continue-statement continue-statement?
  (a-continue-statement))


(define-datatype print-statement print-statement?
  (a-print-statement
   (exps expressions?)))


(define-datatype function-definition-statement function-definition-statement?
  (function-definition-statement-without-parameter
   (id symbol?)
   (stmts statements?))
  (function-definition-statement-with-parameter
   (id symbol?)
   (params parameters?)
   (stmts statements?)))


(define-datatype if-statement if-statement?
  (an-if-statement
   (exp expression?)
   (stmts-true statements?)
   (stmts-false statements?)))


(define-datatype for-statement for-statement?
  (a-for-statement
   (id symbol?)
   (exp expression?)
   (stmts statements?)))


(define-datatype parameters parameters?
  (parameters-single
   (param parameter-with-default-value?))
  (parameters-multiple
   (params parameters?)
   (param parameter-with-default-value?)))


(define-datatype parameter-with-default-value parameter-with-default-value?
  (a-parameter-with-default-value
   (id symbol?)
   (exp expression?)))


(define-datatype expression expression?
  (an-expression
   (dis disjunction?)))


(define-datatype disjunction disjunction?
  (disjunction-as-con
   (con conjunction?))
  (disjunction-as-dis-or-con
   (dis disjunction?)
   (con conjunction?)))


(define-datatype conjunction conjunction?
  (conjunction-as-inv
   (inv inversion?))
  (conjunction-as-con-and-inv
   (con conjunction?)
   (inv inversion?)))


(define-datatype inversion inversion?
  (inversion-as-not-inv
   (inv inversion?))
  (inversion-as-com
   (com comparison?)))


(define-datatype comparison comparison?
  (comparison-sum
   (s sum?))
  (comparison-EQ-sum
   (s EQ-sum?))
  (comparison-LT-sum
   (s LT-sum?))
  (comparison-GT-sum
   (s GT-sum?)))


(define-datatype sum sum?
  (sum-addition
   (s sum?)
   (t term?))
  (sum-subtraction
   (s sum?)
   (t term?))
  (sum-as-term
   (t term?)))


(define-datatype EQ-sum EQ-sum?
  (an-EQ-sum
   (lhs sum?)
   (rhs sum?)))


(define-datatype LT-sum LT-sum?
  (a-LT-sum
   (lhs sum?)
   (rhs sum?)))


(define-datatype GT-sum GT-sum?
  (a-GT-sum
   (lhs sum?)
   (rhs sum?)))


(define-datatype term term?
  (term-multiplication
   (t term?)
   (f factor?))
  (term-division
   (t term?)
   (f factor?))
  (term-as-factor
   (f factor?)))


(define-datatype factor factor?
  (factor-affirmation
   (p power?))
  (factor-negation
   (p power?))
  (factor-as-power
   (p power?)))


(define-datatype power power?
  (power-exponentiation
   (a atom?)
   (f factor?))
  (power-as-primary
   (p primary?)))


(define-datatype atom atom?
  (atom-id
   (id symbol?))
  (atom-bool
   (bool boolean?))
  (atom-none)
  (atom-number
   (num number?))
  (atom-lst
   (lst lst?)))


(define-datatype primary primary?
  (primary-atom
   (a atom?))
  (primary-lst-index
   (p primary?)
   (exp expression?))
  (primary-call-function-no-args
   (p primary?))
  (primary-call-function-with-args
   (p primary?)
   (args arguments?)))


(define-datatype arguments arguments?
  (arguments-single
   (exp expression?))
  (arguments-multiple
   (args arguments?)
   (exp expression?)))


(define-datatype lst lst?
  (empty-lst)
  (non-empty-lst
   (exps expressions?)))


(define-datatype expressions expressions?
  (expressions-single
   (exp expression?))
  (expressions-multiple
   (exps expressions?)
   (exp expression?)))