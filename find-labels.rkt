#lang racket
(provide find-labels)

(define labels (make-hash))

(define (find-labels asm [ln 0])
  (if (null? asm)
      '(() . ())
      (cons
       labels
       (match (car asm)
         [(list ': name)
          (hash-set! labels name ln)
          (cdr (find-labels (cdr asm) ln))]
         [instr (cons instr (cdr (find-labels (cdr asm) (add1 ln))))]))))