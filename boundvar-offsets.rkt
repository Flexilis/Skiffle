#lang racket

;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) (sym*) e*) | (e e*) | sym
;;                       --->>>
;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) (sym*) e*) | (e e*) | sym | (boundvar sym int)

;; args ret prevenv boundvars locvars

(require "general-walker.rkt")
(provide find-boundvar-offsets)

(define (find-boundvar-offsets code)
  (for/list ([expr code])
    (find-boundvar-offsets-expr expr)))

(define (find-boundvar-offsets-expr expr)
  (match expr
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc names (map find-boundvar-offsets-expr values))
      (map find-boundvar-offsets-expr body))]
    [(list 'begin body)
     (list-rest
      'begin
      (map find-boundvar-offsets-expr body))]
    [(list 'lambda (list boundvar ...) (list boundvar ...) (list args ...) body ...)
     (list-rest
      'lambda
      boundvar
      args
      (if (null? boundvar)
       body
       (replace-boundvar
        (assign-offsets boundvar)
        body)))]
    [(list expr args ...)
     (map find-boundvar-offsets-expr (cons expr args))]
    [other other]))

(define (assign-offsets vars [acc 1])
  (if (null? vars) '()
      (cons
       (cons
        (car vars)
        acc)
       (assign-offsets (cdr vars) (add1 acc)))))

(define (lookup-offset var offsets)
  (cond
    [(null? offsets)                 #f]
    [(eq? (caar offsets) var)        (cdar offsets)]
    [else                            (lookup-offset var (cdr offsets))]))

(define (replace-boundvar offsets body)
  (for/list ([expr body])
    (replace-boundvar-expr offsets expr)))

(define (replace-boundvar-expr offsets expr)
  (match expr
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc names (map (λ (x) (replace-boundvar-expr offsets x)) values))
      (replace-boundvar offsets body))]
    [(list 'begin body)
     (list-rest
      'begin
      (replace-boundvar offsets body))]
    [(list 'lambda _ _ _ _ ...)
     expr]
    [(list expr args ...)
     (map (λ (x) (replace-boundvar-expr offsets x)) (cons expr args))]
    [(? symbol?)
     (let ([offset (lookup-offset expr offsets)])
       (if offset
           (list 'boundvar expr offset)
           expr))]
    [other other]))
