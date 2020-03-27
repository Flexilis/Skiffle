#lang racket

(provide push pop pop-n)

(define (push value mem)
  (match mem
    [(cons registers memory)
     (let ([loc (sub1 (vector-ref registers 0))])
       (if (loc . < . 0)
           (error "Stack overflow.")
           (void))
       (vector-set! registers 0 loc)
       (vector-set! memory loc value))]))

(define (pop mem)
  (match mem
    [(cons registers memory)
     (let ([loc (vector-ref registers 0)])
       (vector-set! registers 0 (add1 loc))
       (vector-ref memory loc))]))

(define (pop-n n mem)
  (if (n . = . 0)
      '()
      (let ([popped (pop mem)]) ;; force evaluation order of pops
        (cons
         popped
         (pop-n (sub1 n) mem)))))
