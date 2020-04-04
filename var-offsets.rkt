#lang racket

;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda (sym*) (sym*) (sym*) e*) | (e e*) | sym
;;                       --->>>
;; e ::= (begin e*) | (let ((sym e)*) e*) | (lambda ((sym . int)*) ((sym . int)*) ((sym . int)*) e*) | (e e*) | (var sym int)

;; (lambda ((freevars . offsets) ...) ((boundvars . offsets) ...) ((args . offsets) ...) body ...)

;; args ret prevenv vars locvars

(require "general-walker.rkt")
(provide find-var-offsets)

(define (find-var-offsets code)
  (map find-var-offsets-expr code))

(define (find-var-offsets-expr expr)
  (match expr
    [(list 'if cond if-branch else-branch)
     (list 'if
           (find-var-offsets-expr cond)
           (find-var-offsets-expr if-branch)
           (find-var-offsets-expr else-branch))]
    [(list 'let (list (list names values) ...) body ...)
     (list-rest
      'let
      (build-assoc names (map find-var-offsets-expr values))
      (map find-var-offsets-expr body))]
    [(list 'begin body ...)
     (list-rest
      'begin
      (map find-var-offsets-expr body))]
    [(list 'lambda (list var ...) (list boundvar ...) (list args ...) body ...)
     (let* ([var-offsets (assign-offsets var)]
            [boundvar-offsets (assign-offsets boundvar (add1 (length var)))]
            [arg-offsets (assign-offsets args (- (length args)))])
       (list-rest
        'lambda
        var-offsets
        boundvar-offsets
        arg-offsets
        (find-var-offsets
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
        (- acc))
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
      (build-assoc
       (merge-lists
        names
        (map (λ (x) (lookup-offset x offsets)) names))
       (map (λ (x) (replace-var-expr offsets x)) values))
      (replace-var offsets body))]
    [(list 'begin body ...)
     (list-rest
      'begin
      (replace-var offsets body))]
    [(list 'lambda _ _ _ _ ...) ;; each lambda has it's own var binding
     expr]
    [(list 'if cond if-branch else-branch)
     (list 'if
           (replace-var-expr offsets cond)
           (replace-var-expr offsets if-branch)
           (replace-var-expr offsets else-branch))]
    [(list expr args ...)
     (map (λ (x) (replace-var-expr offsets x)) (cons expr args))]
    [(? symbol?)
     (let ([offset (lookup-offset expr offsets)])
       (if offset
           (list 'var expr offset)
           expr))]
    [other other]))


