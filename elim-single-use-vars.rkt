#lang racket

(require "general-walker.rkt")
(provide elim-single-use-vars)

(define vars (make-hash))

(define (elim-single-use-vars code)
  (map elim-single-use-vars-expr code))

(define (count-uses var expr)
  (define uses 0)
  (apply-to-expr
   (λ (e)
     (if (eq? e var)
         (set! uses (add1 uses))
         (void)))
   expr)
  uses)
   
(define (elim-single-use-vars-expr expr)
  (match expr
    [(? symbol?)
     (let ([val (hash-ref vars expr #f)])
       (if val val expr))]
    [(list 'begin exprs ...)
     (elim-single-use-vars exprs)]
    [(list 'lambda (list names ...) body ..1)
     (list-rest
      'lambda
      names
      (elim-single-use-vars body))]
    [(list 'let (list (list names values) ...) body ..1)
     (define elim-let #t)
     (for ([name names] [value values])
       (if (<= (count-uses name (list 'begin body)) 1)
           (hash-set! vars name value)
           (set! elim-let #f)))
     (if elim-let
         (if ((length body) . > . 1)
             (list-rest 'begin (elim-single-use-vars body))
             (car (elim-single-use-vars body)))
         (list-rest
          'let
          (for/list ([name names]
                     [value values]
                     #:unless (hash-ref vars name #f))
            (list name value))
          (elim-single-use-vars body)))]
    [(list expr1 args ...)
     (elim-single-use-vars expr)]
    [other other]))