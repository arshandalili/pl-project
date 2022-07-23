#lang racket

(require "store.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "datatype.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(provide run ENV)


(define break-signal #f)
(define return-signal '(#f))
(define continue-signal #f)
(define SKIP-MODE (lambda () (or break-signal (car return-signal) continue-signal)))
(define return-value null)
(define global-refs '())


(define ENV-history 'uninitialized)
(define ENV 'uninitialized)


(define initialize-env!
  (lambda ()
    (begin
      (set! ENV-history '())
      (set! ENV (empty-env)))))


(define global-env (lambda () (list-ref ENV-history)))
(define env-depth (lambda () (length ENV-history)))


(define apply-prev-env
  (lambda (id depth)
    (if (= depth (length ENV-history))
        null
        (let ([ref (apply-env id (list-ref ENV-history depth))])
          (if (null? ref) (apply-prev-env id (add1 depth)) ref)))))
              

(define push-env
  (lambda (env)
    (begin
      (set! ENV-history (cons ENV ENV-history))
      (set! ENV env))))


(define pop-env
  (lambda ()
    (begin
      (set! ENV (car ENV-history))
      (set! ENV-history (cdr ENV-history)))))


(define run
  (lambda (pgm-string)
    (define parse-tree (PARSER (lex-this LEXER (open-input-string pgm-string))))
    (begin
      (initialize-env!)
      (initialize-store!)
      (value-of-program parse-tree))))


(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stmts) (value-of-statements stmts)))))


(define value-of-statements
  (lambda (input)
    (cases statements input
      (statements-single (stmt) (value-of-statement stmt))
      (statements-multiple (stmts stmt) (begin (value-of-statements stmts) (value-of-statement stmt))))))


(define value-of-statement
  (lambda (input)
    (if (SKIP-MODE)
        (void)
        (cases statement input
          (statement-simple (stmt) (value-of-simple-statement stmt))
          (statement-compound (stmt) (value-of-compound-statement stmt))))))


(define value-of-simple-statement
  (lambda (input)
    (cases simple-statement input
      (simple-statement-assignment (stmt) (value-of-assignment-statement stmt))
      (simple-statement-global (stmt) (value-of-global-statement stmt))
      (simple-statement-return (stmt) (value-of-return-statement stmt))
      (simple-statement-pass (stmt) (value-of-pass-statement stmt))
      (simple-statement-break (stmt) (value-of-break-statement stmt))
      (simple-statement-continue (stmt) (value-of-continue-statement stmt))
      (simple-statement-print (stmt) (value-of-print-statement stmt)))))


(define value-of-compound-statement
  (lambda (input)
    (cases compound-statement input
      (compound-statement-function-definition (stmt) (value-of-function-definition-statement stmt))
      (compound-statement-if (stmt) (value-of-if-statement stmt))
      (compound-statement-for (stmt) (value-of-for-statement stmt)))))


(define value-of-assignment-statement
  (lambda (input)
    (cases assignment-statement input
      (an-assignment-statement (id exp) (set! ENV (extended-env id (newref (lazy-value exp ENV)) ENV))))))


(define value-of-global-statement
  (lambda (input)
    (cases global-statement input
      (a-global-statement (id) (let ([old-ref (apply-prev-env id 0)])
                                 (let ([new-ref (newref (deref old-ref))])
                                   (begin
                                     (set! global-refs (cons (list (env-depth) id old-ref) global-refs))
                                     (set! ENV (extended-env id new-ref ENV)))))))))
                                 

(define value-of-return-statement
  (lambda (input)
    (begin
      (set! return-signal (cons #t (cdr return-signal)))
      (cases return-statement input
        (return-statement-void () (set! return-value null))
        (return-statement-with-value (exp)(set! return-value (value-of-expression exp)))))))


(define value-of-pass-statement
  (lambda (input)
    (cases pass-statement input
      (a-pass-statement () (void)))))


(define value-of-break-statement
  (lambda (input)
    (cases break-statement input
      (a-break-statement () (set! break-signal #t)))))


(define value-of-continue-statement
  (lambda (input)
    (cases continue-statement input
      (a-continue-statement () (set! continue-signal #t)))))


(define value-of-print-statement
  (lambda (input)
    (cases print-statement input
      (a-print-statement (exps) (void (map displayln (value-of-expressions exps)))))))


(define value-of-function-definition-statement
  (lambda (input)
    (cases function-definition-statement input
      (function-definition-statement-without-parameter (id stmts) (set! ENV (extended-env id (newref (func-value input)) ENV)))
      (function-definition-statement-with-parameter (id params stmts) (set! ENV (extended-env id (newref (func-value input)) ENV))))))


(define value-of-if-statement
  (lambda (input)
    (cases if-statement input
      (an-if-statement (exp stmts-true stmts-false)
                       (if (value-of-expression exp) (value-of-statements stmts-true) (value-of-statements stmts-false))))))


(define value-of-for-statement
  (lambda (input)
    (cases for-statement input
      (a-for-statement (id exp stmts) (begin
                                        (set! break-signal #f)
                                        (let loop ([var id][list-val (value-of-expression exp)])
                                          (if (or (null? list-val) break-signal)
                                              (void)
                                              (begin
                                                (set! continue-signal #f)
                                                (set! ENV (extended-env id (newref (normal-value (car list-val))) ENV))
                                                (value-of-statements stmts)
                                                (loop var (cdr list-val)))))
                                        (set! break-signal #f)
                                        (set! continue-signal #f))))))


(define value-of-parameters
  (lambda (input)
    (cases parameters input
      (parameters-single (param) (list (value-of-parameter-with-default-value param)))
      (parameters-multiple (params param) (append (value-of-parameters params) (list (value-of-parameter-with-default-value param)))))))


(define value-of-parameter-with-default-value
  (lambda (input)
    (cases parameter-with-default-value input
      (a-parameter-with-default-value (id exp) (value-of-expression exp)))))


(define value-of-expression
  (lambda (input)
    (cases expression input
      (an-expression (dis) (value-of-disjunction dis)))))


(define value-of-disjunction
  (lambda (input)
    (cases disjunction input
      (disjunction-as-con (con) (value-of-conjunction con))
      (disjunction-as-dis-or-con (dis con) (or (value-of-disjunction dis) (value-of-conjunction con))))))


(define value-of-conjunction
  (lambda (input)
    (cases conjunction input
      (conjunction-as-inv (inv) (value-of-inversion inv))
      (conjunction-as-con-and-inv (con inv) (and (value-of-conjunction con) (value-of-inversion inv))))))


(define value-of-inversion
  (lambda (input)
    (cases inversion input
      (inversion-as-not-inv (inv) (not (value-of-inversion inv)))
      (inversion-as-com (com) (value-of-comparison com)))))


(define value-of-comparison
  (lambda (input)
    (cases comparison input
      (comparison-sum (s) (value-of-sum s))
      (comparison-EQ-sum (s) (value-of-EQ-sum s))
      (comparison-LT-sum (s) (value-of-LT-sum s))
      (comparison-GT-sum (s) (value-of-GT-sum s)))))


(define value-of-sum
  (lambda (input)
    (cases sum input
      (sum-addition (s t) (+! (value-of-sum s) (value-of-term t)))
      (sum-subtraction (s t) (- (value-of-sum s) (value-of-term t)))
      (sum-as-term (t) (value-of-term t)))))


(define +!
  (lambda (x y)
      (cond
        [(and (number? x) (number? y)) (+ x y)]
        [(and (list? x) (list? y)) (append x y)])))


(define value-of-EQ-sum
  (lambda (input)
    (cases EQ-sum input
      (an-EQ-sum (lhs rhs) (equal? (value-of-sum lhs) (value-of-sum rhs))))))


(define value-of-LT-sum
  (lambda (input)
    (cases LT-sum input
      (a-LT-sum (lhs rhs) (< (value-of-sum lhs) (value-of-sum rhs))))))


(define value-of-GT-sum
  (lambda (input)
    (cases GT-sum input
      (a-GT-sum (lhs rhs) (> (value-of-sum lhs) (value-of-sum rhs))))))


(define value-of-term
  (lambda (input)
    (cases term input
      (term-multiplication (t f) (let ([left-val (value-of-term t)])
                                   (if (zero? left-val) 0 (* left-val (value-of-factor f)))))
      (term-division (t f) (exact->inexact (/ (value-of-term t) (value-of-factor f))))
      (term-as-factor (f) (value-of-factor f)))))


(define value-of-factor
  (lambda (input)
    (cases factor input
      (factor-affirmation (p) (value-of-power p))
      (factor-negation (p) (- (value-of-power p)))
      (factor-as-power (p) (value-of-power p)))))


(define value-of-power
  (lambda (input)
    (cases power input
      (power-exponentiation (a f) (expt (value-of-atom a) (value-of-factor f)))
      (power-as-primary (p) (value-of-primary p)))))


(define value-of-atom
  (lambda (input)
    (cases atom input
      (atom-id (id) (let ([v (deref (apply-env id ENV))])
                      (if (value? v)
                          (cases value v
                            (none-value () null)
                            (normal-value (val) val)
                            (lazy-value (exp env) (begin (push-env env) (let ([val (value-of-expression exp)]) (pop-env) val)))
                            (func-value (func) func))
                          v)))
      (atom-bool (bool) bool)
      (atom-none () 'None)
      (atom-number (num) num)
      (atom-lst (lst) (value-of-lst lst)))))


(define value-of-primary
  (lambda (input)
    (cases primary input
      (primary-atom (a) (value-of-atom a))
      (primary-lst-index (p exp) (list-ref (value-of-primary p) (inexact->exact (value-of-expression exp))))
      (primary-call-function-no-args (p) (value-of-call-function (value-of-primary p) null))
      (primary-call-function-with-args (p args) (value-of-call-function (value-of-primary p) args)))))


(define value-of-arguments
  (lambda (input)
    (cases arguments input
      (arguments-single (exp) (list (value-of-expression exp)))
      (arguments-multiple (args exp) (append (value-of-arguments args) (list (value-of-expression exp)))))))


(define value-of-lst
  (lambda (input)
    (cases lst input
      (empty-lst () '())
      (non-empty-lst (exps) (value-of-expressions exps)))))


(define value-of-expressions
  (lambda (input)
    (cases expressions input
      (expressions-single (exp) (list (value-of-expression exp)))
      (expressions-multiple (exps exp) (append (value-of-expressions exps) (list (value-of-expression exp)))))))


(define value-of-call-function
  (lambda (func args)
    (begin
      (set! return-signal (cons #f return-signal))
      (set! return-value null)
      (cases function-definition-statement func
        (function-definition-statement-without-parameter (id stmts) (begin
                                                                      (push-env (call-func-env id func '() '()))
                                                                      (value-of-statements stmts)))
        (function-definition-statement-with-parameter (id params stmts) (begin
                                                                          (push-env (call-func-env id func params args))
                                                                          (value-of-statements stmts))))
      (let loop ([stop #f])
        (if stop
            (void)
            (if (null? global-refs)
                (loop #t)
                (begin
                  (let ([depth (car (car global-refs))] [id (cadr (car global-refs))] [old-ref (caddr (car global-refs))])
                    (if (= (env-depth) depth)
                        (begin
                          (setref! old-ref (deref (apply-env id ENV)))
                          (set! global-refs (cdr global-refs)))
                        (set! stop #t)))
                  (loop stop)))))
      (pop-env)
      (set! return-signal (cdr return-signal))
      (let ([val return-value])
        (set! return-value null) val))))
        

(define call-func-env
  (lambda (id func params args)
    (let ([env (extended-env id (newref (func-value func)) (empty-env))])                  
      (let loop ([vars (func-input-vars params)] [vals (func-input-vals params args)])
        (if (null? vars) env (extended-env (car vars) (newref (car vals)) (loop (cdr vars) (cdr vals)))))))) 


(define func-input-vars
  (lambda (params)
    (if (null? params) '()
        (let loop ([params params])
          (cases parameters params
            (parameters-single (param)
                               (cases parameter-with-default-value param
                                 (a-parameter-with-default-value (id exp) (list id))))
            (parameters-multiple (params param)
                                 (cases parameter-with-default-value param
                                   (a-parameter-with-default-value (id exp) (append (loop params) (list id))))))))))


(define func-input-vals
  (lambda (params args)
    (cond
      [(null? params) '()]
      [(null? args) (value-of-parameters params)]
      [else (let ([params-vals (value-of-parameters params)] [args-vals (value-of-arguments args)])
              (let loop ([params-vals params-vals] [args-vals args-vals])
                (cond
                  [(null? params-vals) '()]
                  [(null? args-vals) params-vals]
                  [else (cons (car args-vals) (loop (cdr params-vals) (cdr args-vals)))])))])))