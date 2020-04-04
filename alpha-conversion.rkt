#lang racket

(require "general-walker.rkt")
(provide alpha-convert)

;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) e*) | (e e*) | sym
;;                       --->>>
;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) e*) | (e e*) | sym

(define primitives '(((lambda . lambda) (if . if) (let . let) (begin . begin) (+ . +))))

(define-syntax-rule (if-true-ret expr1 expr2)
  (let ([e expr1])
    (if e e expr2)))

(define (lookup-in-scope name scope)
  (cond
    [(null? scope) #f]
    [(eq? name (caar scope)) (cdar scope)]
    [else (lookup-in-scope name (cdr scope))]))

(define (lookup name scopes)
  (if (null? scopes)
      (error (~a "Error: '" name "' not found."))
      (if-true-ret
       (lookup-in-scope name (car scopes))
       (lookup name (cdr scopes)))))

(define (alpha-convert code)
  (alpha-convert-block code primitives))

(define gen-name
  (let ([names 0])
    (λ ()
      (set! names (add1 names))
      (string->symbol (~a "v" names)))))

(define (alpha-convert-expr expr scopes)
  (match expr
    [(? symbol?) (lookup expr scopes)]
    [(list 'begin (list exprs ...))
     (alpha-convert-block exprs scopes)]
    [(list 'if cond if-branch else-branch)
     (list 'if
           (alpha-convert-expr cond scopes)
           (alpha-convert-expr if-branch scopes)
           (alpha-convert-expr else-branch scopes))]
    [(list 'lambda (list names ...) body ...)
     (let* ([new-names (map (λ e (gen-name)) names)]
            [new-scope (merge-lists names new-names)])
       (list-rest
        'lambda
        (map (λ (e) (lookup-in-scope e new-scope)) names)
        (alpha-convert-block body (cons new-scope scopes))))]
    [(list 'let (list (list names values) ...) body ..1)
     (let* ([new-names (map (λ e (gen-name)) names)]
            [new-scope (merge-lists names new-names)]
            [new-values (map (λ (e) (alpha-convert-expr e scopes)) values)]
            [new-namevals (build-assoc new-names new-values)])
       (list-rest
        'let
        new-namevals
        (alpha-convert-block body (cons new-scope scopes))))]
    [(list expr1 args ...)
     (cons
      (alpha-convert-expr expr1 scopes)
      (map (λ (e) (alpha-convert-expr e scopes)) args))]
    [other other]))

(define (alpha-convert-block block scopes)
  (if (null? block) '()
      (match (car block)
        [(list 'define name body)
         (let ([new-name (gen-name)])
           (cons
            (list 'define new-name (alpha-convert-expr body scopes))
            (alpha-convert-block
             (cdr block)
             (cons
              (list (cons name new-name))
              scopes))))]
        [other
         (cons
          (alpha-convert-expr other scopes)
          (alpha-convert-block (cdr block) scopes))])))
