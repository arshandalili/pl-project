#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre) parser-tools/yacc)

(provide LEXER tokens empty-tokens lex-this)


(define-lex-abbrevs
  [digit (:/ #\0 #\9)]
  [punctuation (char-set "()[],:;")]
  [letter (:or (:/ #\a #\z) (:/ #\A #\Z) #\_)]
  [identifier (:: letter (:* (:or letter digit)))]
  [number (:or (:+ digit) (:: (:+ digit) #\. (:+ digit)))]
  [operator (:or "+" "-" "*" "**" "/" "=" "==" "<" ">" "and" "or" "not")]
  [keyword (:or "None" "break" "continue" "def" "else" "for" "global" "if" "in" "pass" "print" "return")])


(define LEXER
  (lexer
   ["True" (token-BOOL #t)]
   ["False" (token-BOOL #f)]
   [(:or punctuation operator keyword) (string->symbol lexeme)]
   [identifier (token-ID (string->symbol lexeme))]
   [number (token-NUM (string->number lexeme))]
   [whitespace (LEXER input-port)]
   [(eof) (token-EOF)]))


(define-tokens tokens (BOOL ID NUM)) 
(define-empty-tokens empty-tokens
  (\( \) \[ \] \, \: \; + - * ** / = == < > EOF None and break continue def else for global if in not or pass print return))


(define lex-this (lambda (lexer input) (lambda () (lexer input))))