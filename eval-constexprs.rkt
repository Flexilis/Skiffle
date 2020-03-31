#lang racket

(require "general-walker.rkt")
(provide eval-constexprs)

(define constexpr-vars (make-hash))

(define (eval-constexprs code)
  (collect-constexprs code)
  (map eval-constexprs-expr code))

(define (lit-eval expr)
  (match expr
    [(? symbol?)
     (lit-eval (hash-ref constexpr-vars expr))]
    [(list op lop rop)
     ((match op ['+ +] ['- -]) (lit-eval lop) (lit-eval rop))]
    [(list 'let _ body ..1)
     (lit-eval (last body))]
    [(? number?) expr]))
    
(define (eval-constexprs-expr expr)
  (if (constexpr? expr)
      (lit-eval
       (match expr
         [(? symbol?)
          expr]
         [(list 'begin body ...)
          (last body)]
         [(list 'let (list (list names values) ...) body ..1)
          (last body)]
         [(list (or '+ '-) lop rop)
          expr]
         [other other]))
      (match expr
        [(list 'let (list (list names values) ...) body ..1)
         (list-rest
          'let
          (for/list ([name names]
                     [value values]
                     #:unless (hash-ref constexpr-vars name #f))
            (list name value))
          body)]
        [else (apply-to-subexprs eval-constexprs-expr expr)])))

(define (all l)
  (if (null? l) #t (and (car l) (all (cdr l)))))

(define (collect-constexprs body)
  (let ([fn (λ (e)
              (match e
                [(list 'let (list (list names values) ...) _ ..1)
                 (for ([name names] [value values])
                   (unless (not (constexpr? value))
                     (hash-set! constexpr-vars name value)))]
                [_ (void)])
              e)])
    (map
     (λ (expr)
       (apply-to-subexprs fn (fn expr))
       expr)
     body)))

(define (constexpr? expr)
  (match expr
    [(? symbol?)
     (hash-ref constexpr-vars expr #f)]
    [(list 'begin body ..1)
     (all (map constexpr? body))]
    [(list 'lambda (list names ...) body ...)
     #f]
    [(list 'let (list (list names values) ...) body ..1)
     (all (map constexpr? body))]
    [(list (or '+ '-) lop rop)
     (and (constexpr? lop) (constexpr? rop))]
    [(list expr1 args ...)
     #f]
    [other #t]))
