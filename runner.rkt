#lang racket

(require "alpha-conversion.rkt"
         "eval-constexprs.rkt"
         "elim-single-use-vars.rkt"
         "collect-vars.rkt"
         "var-offsets.rkt"
         "unify-freevars.rkt"
         ;"compile.rkt"
         "interp.rkt")

(define (run code passes)
  (if (null? passes) 
      code
      (run ((car passes) code) (cdr passes))))

(define (print-id x) (println x) x)

(println
 (run
  '((let ([y 2]) ((lambda (x) (+ x y)) 10)))
  (list alpha-convert
        ;eval-constexprs
        ;elim-single-use-vars
        collect-vars
        find-var-offsets
        unify-freevars
        ;compile
        ;interp
        )))