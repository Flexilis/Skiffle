#lang racket

;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) (sym*) (sym*) e*) | (e e*) | sym
;;                       --->>>
;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda ((sym . int)*) ((sym . int)*) ((sym . int)*) e*) | (e e*) | (var sym int)

;; (lambda ((freevars . offsets) ...) ((boundvars . offsets) ...) ((args . offsets) ...) body ...)

;; args ret prevenv vars locvars

(require "general-walker.rkt")
(provide find-var-offsets)

(define (find-var-offsets code)
  (for/list ([expr code])
    (find-var-offsets-expr expr)))

(define (find-var-offsets-expr expr)
  (match expr23
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc names (map find-var-offsets-expr values))
      (map find-var-offsets-expr body))]
    [(list 'begin body)
     (list-rest
      'begin
      (map find-var-offsets-expr body))]
    [(list 'lambda (list var ...) (list boundvar ...) (list args ...) body ...)
     (let* ([var-offsets (assign-offsets var)]
            [boundvar-offsets (assign-offsets boundvar (add1 (length var)))]
            [arg-offsets (assign-offsets args (- 0 (length args) 1))])
       (list-rest
        'lambda
        var-offsets
        boundvar-offsets
        arg-offsets
        (if (null? var)
            body
            (replace-var
             (append
              var-offsets
              boundvar-offsets
              arg-offsets)
             body))))]
    [(list expr args ...)
     (map find-var-offsets-expr (cons expr args))]
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

(define (replace-var offsets body)
  (for/list ([expr body])
    (replace-var-expr offsets expr)))

(define (replace-var-expr offsets expr)
  (match expr
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc names (map (λ (x) (replace-var-expr offsets x)) values))
      (replace-var offsets body))]
    [(list 'begin body)
     (list-rest
      'begin
      (replace-var offsets body))]
    [(list 'lambda _ _ _ _ ...) ;; each lambda has it's own var binding
     expr]
    [(list expr args ...)
     (map (λ (x) (replace-var-expr offsets x)) (cons expr args))]
    [(? symbol?)
     (let ([offset (lookup-offset expr offsets)])
       (if offset
           (list 'var expr offset)
           expr))]
    [other other]))


