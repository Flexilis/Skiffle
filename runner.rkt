#lang racket

(require "alpha-conversion.rkt"
         "eval-constexprs.rkt"
         "elim-single-use-vars.rkt"
         "collect-vars.rkt"
         "var-offsets.rkt"
         "unify-freevars.rkt"
         "label-replacement.rkt"
         "find-labels.rkt"
         "compile-expr.rkt"
         "interp.rkt"
         )

(define (run code passes)
  (if (null? passes) 
      code
      (run (begin (println (car passes)) ((car passes) code)) (cdr passes))))

(define (print-id x) (println x) x)

(define code '())

(time
 (run
  '((if 3 2 1))
  (list alpha-convert
        ;eval-constexprs
        elim-single-use-vars
        collect-vars
        find-var-offsets
        unify-freevars
        compile-all
        find-labels
        replace-labels
        interp
        )))