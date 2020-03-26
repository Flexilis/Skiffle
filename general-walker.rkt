#lang racket

(provide apply-to-subexprs apply-to-expr apply-to-exprs merge-lists build-assoc list-rest)

(define (merge-lists l1 l2)
  (for/list ([x l1] [y l2])
    (cons x y)))

(define (build-assoc l1 l2)
  (for/list ([x l1] [y l2])
    (list x y)))

(define (list-rest arg1 . args)
  (match args
    ['() arg1]
    [(cons first rest)
     (cons
      arg1
      (apply list-rest (cons first rest)))]))

(define (apply-to-exprs fn code)
  (for/list ([expr code])
    (apply-to-expr fn expr)))

(define (apply-to-expr fn expr)
  (fn (apply-to-subexprs fn expr)))

(define (apply-to-subexprs fn expr)
  (match expr
    [(list expr args ...)
     (cons
      (apply-to-expr fn expr)
      (map (λ (e) (apply-to-expr fn e)) args))]
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc
       names
       (map (λ (e) (apply-to-expr fn e)) values))
      (map (λ (e) (apply-to-expr fn e)) body))]
    [(list 'begin body ...)
     (list-rest
      'begin
      (map (λ (e) (apply-to-expr fn e)) body))]
    [(list 'lambda (list args ...) body ...)
     (list-rest
      'lambda
      args
      (map (λ (e) (apply-to-expr fn e)) body))]
    [other other]))