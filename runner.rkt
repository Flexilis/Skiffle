#lang racket

(require "alpha-conversion.rkt" "collect-vars.rkt" "freevar-offsets.rkt")

(define (run code passes)
  (if (null? passes) 
      code
      (run ((car passes) code) (cdr passes))))

(println
 (run
  '((let ((x 4) (z 2)) (lambda (y) (let ([x 2]) (x y z)))))
  (list alpha-convert collect-vars find-freevar-offsets)))