#lang racket

(require "lexer.rkt")
(require "datatype.rkt")
(require parser-tools/lex (prefix-in : parser-tools/lex-sre) parser-tools/yacc)

(provide PARSER)


(define PARSER
  (parser
   (start Program)
   (end EOF)
   (error void)
   (tokens tokens empty-tokens)
   (grammar 
    (Program
     [(Statements) (a-program $1)])
    (Statements
     [(Statement \;) (statements-single $1)]
     [(Statements Statement \;) (statements-multiple $1 $2)])
    (Statement
     [(Simple-stmt) (statement-simple $1)]
     [(Compound-stmt) (statement-compound $1)])
    (Simple-stmt
     [(Assignment) (simple-statement-assignment $1)]
     [(Global-stmt) (simple-statement-global $1)]
     [(Return-stmt) (simple-statement-return $1)]
     [(pass) (simple-statement-pass (a-pass-statement))]
     [(break) (simple-statement-break (a-break-statement))]
     [(continue) (simple-statement-continue (a-continue-statement))]
     [(print \( Expressions \)) (simple-statement-print (a-print-statement $3))])
    (Compound-stmt
     [(Function-def) (compound-statement-function-definition $1)]
     [(If-stmt) (compound-statement-if $1)]
     [(For-stmt) (compound-statement-for $1)])
    (Assignment
     [(ID = Expression) (an-assignment-statement $1 $3)])
    (Return-stmt
     [(return) (return-statement-void)]
     [(return Expression) (return-statement-with-value $2)])
    (Global-stmt
     [(global ID) (a-global-statement $2)])
    (Function-def
     [(def ID \( \) \: Statements) (function-definition-statement-without-parameter $2 $6)]
     [(def ID \( Params \) \: Statements) (function-definition-statement-with-parameter $2 $4 $7)])
    (Params
     [(Param-with-default) (parameters-single $1)]
     [(Params \, Param-with-default) (parameters-multiple $1 $3)])
    (Param-with-default
     [(ID = Expression) (a-parameter-with-default-value $1 $3)])
    (If-stmt
     [(if Expression \: Statements else \: Statements) (an-if-statement $2 $4 $7)])
    (For-stmt
     [(for ID in Expression \: Statements) (a-for-statement $2 $4 $6)])
    (Expression
     [(Disjunction) (an-expression $1)])
    (Disjunction
     [(Conjunction) (disjunction-as-con $1)]
     [(Disjunction or Conjunction) (disjunction-as-dis-or-con $1 $3)])
    (Conjunction
     [(Inversion) (conjunction-as-inv $1)]
     [(Conjunction and Inversion) (conjunction-as-con-and-inv $1 $3)])
    (Inversion
     [(not Inversion) (inversion-as-not-inv $2)]
     [(Comparison) (inversion-as-com $1)])
    (Comparison
     [(Sum) (comparison-sum $1)]
     [(Eq-Sum) (comparison-EQ-sum $1)]
     [(Lt-Sum) (comparison-LT-sum $1)]
     [(Gt-Sum) (comparison-GT-sum $1)])
    (Eq-Sum
     [(Sum == Sum) (an-EQ-sum $1 $3)])
    (Lt-Sum
     [(Sum < Sum) (a-LT-sum $1 $3)])
    (Gt-Sum
     [(Sum > Sum) (a-GT-sum $1 $3)])
    (Sum
     [(Sum + Term) (sum-addition $1 $3)]
     [(Sum - Term) (sum-subtraction $1 $3)]
     [(Term) (sum-as-term $1)])
    (Term
     [(Term * Factor) (term-multiplication $1 $3)]
     [(Term / Factor) (term-division $1 $3)]
     [(Factor) (term-as-factor $1)])
    (Factor
     [(+ Power) (factor-affirmation $2)]
     [(- Power) (factor-negation $2)]
     [(Power) (factor-as-power $1)])
    (Power
     [(Atom ** Factor) (power-exponentiation $1 $3)]
     [(Primary) (power-as-primary $1)])
    (Primary
     [(Atom) (primary-atom $1)]
     [(Primary \[ Expression \]) (primary-lst-index $1 $3)]
     [(Primary \( \)) (primary-call-function-no-args $1)]
     [(Primary \( Arguments \)) (primary-call-function-with-args $1 $3)])
    (Arguments
     [(Expression) (arguments-single $1)]
     [(Arguments \, Expression) (arguments-multiple $1 $3)])
    (Atom
     [(ID) (atom-id $1)]
     [(BOOL) (atom-bool $1)]
     [(None) (atom-none)]
     [(NUM) (atom-number $1)]
     [(List) (atom-lst $1)])
    (List
     [(\[ Expressions \]) (non-empty-lst $2)]
     [(\[ \]) (empty-lst)])
    (Expressions
     [(Expressions \, Expression) (expressions-multiple $1 $3)]
     [(Expression) (expressions-single $1)]))))