#lang racket

(require "stack-tools.rkt")

(provide alloc free)

(define used-blocks (make-hash))
(define used-words (void))

(define (alloc mem size)
  (if (void? used-words)
      (set! used-words (make-vector (vector-length (cdr mem)) #f))
      (void))
  ;(push
  (allocate (cdr mem) size));mem)

(define (allocate memory size [index 0] [seq 0])
  (if (index . = . (vector-length used-words))
      (error "Out of memory.")
      (if (vector-ref used-words index)
          (allocate memory size (add1 index))
          (if (= size (add1 seq))
              (let ([start-ind (- index seq)])
                (hash-set! used-blocks start-ind index)
                (for ([i (in-range start-ind (add1 index))])
                  (vector-set! used-words i #t))
                start-ind)
              (allocate memory size (add1 index) (add1 seq))))))

(define (free memory ptr)
  (for ([i (in-range ptr (add1 (hash-ref used-blocks ptr)))])
    (vector-set! used-words i #f))
  (hash-remove! used-blocks ptr))

(define mem '(#() . #(0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define-syntax (print-all stx)
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ item rest ...)
     #'(begin
         (displayln (~a (quote item) " = " item))
         (print-all rest ...))]))
