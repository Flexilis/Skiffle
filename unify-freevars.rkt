#lang racket

;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda ((sym . int)*) ((sym . int)*) ((sym . int)*) e*) | (e e*) | (var sym int)
;;                                      --->>>
;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda ((sym int int)*) ((sym . int)*) ((sym . int)*) e*) | (e e*) | (var sym int)

;; (freevar internal-offsets external-offsets)

(require "general-walker.rkt")
(provide unify-freevars)

(define vars (make-hash))

(define (print-id x) (println x) x)

(define (unify-freevars code)
  (for/list ([expr code])
    (unify-freevars-expr expr)))

(define (unify-freevars-expr expr)
  (match expr
    [(list 'let (list (list (cons names offsets) values) ...) body ...)
     (for ([name names] [offset offsets])
       (hash-set! vars name offset))
     (list-rest
      'let
      (build-assoc offsets (map unify-freevars-expr values))
      (unify-freevars body))]
    [(list 'begin body ...)
     (list-rest
      'begin
      (unify-freevars body))]
    [(list 'lambda
           (list (cons freevars offsets) ...)
           (list (cons boundvars bound-offsets) ...)
           (list (cons arg-names arg-offsets) ...)
           body ...)
     (for ([name arg-names] [offset arg-offsets])
       (hash-set! vars name offset))
     (list-rest
      'lambda
      (build-assoc
       offsets
       (map (λ (x) (hash-ref vars x)) freevars))
      bound-offsets
      arg-offsets
      (unify-freevars body))]
    [(list 'var sym offset)
     (list 'var offset)]
    [(list fn args ...)
     (unify-freevars expr)]
    [other other]))


