#lang racket

; Only works AFTER alpha conversion

;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) e*) | (e e*) | sym
;;                       --->>>
;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) (sym*) (sym*) e*) | (e e*) | sym

(require "general-walker.rkt")

(provide collect-vars)

(define (collect-vars code)
  (collect-vars-expr (list (list-rest 'lambda '() code))))
 
(define (collect-vars-p code)
  (for/list ([expr code])
    (collect-vars-expr expr)))

(define (collect-vars-expr expr)
  (match expr
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc names (map collect-vars-expr values))
      (collect-vars-p body))]
    [(list 'begin body ...)
     (cons
      'begin
      (for/list ([expr body])
        (collect-vars-expr expr)))]
    [(list 'lambda (list args ...) body ...)
     (let ([vars (find-vars body args)])
       (list-rest
        'lambda
        (car vars)
        (set-subtract (cdr vars) args)
        args
        (collect-vars-p body)))]
    [(list expr args ...)
     (collect-vars-p (cons expr args))]
    [other other]))

(define (find-vars code boundvars)
  (let ([vars (map
               (λ (x) (find-vars-expr x boundvars))
               code)])
    (cons
     (apply set-union
       (map car vars))
     (apply set-union
       (map cdr vars)))))

(define (find-vars-expr expr boundvars)
  (match expr
    [(list 'let (list (list names values) ...) body ...)
     (let ([body (find-vars body (set-union names boundvars))]
           [values (find-vars values boundvars)])
       (cons
        (set-union
         (car body)
         (car values))
        (set-union
         (cdr body)
         (cdr values))))]
    [(list 'begin body)
     (find-vars body boundvars)]
    [(list 'lambda (list args ...) body ...)
     (find-vars body (set-union boundvars args))]
    [(list _ args ...)
     (find-vars expr boundvars)]
    [(or '+ '-) (cons '() boundvars)]
    [(? symbol?)
     (cons
      (if (member expr boundvars)
          '()
          (list expr))
      boundvars)]
    [other (cons '() boundvars)]))












