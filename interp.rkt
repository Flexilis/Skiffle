#lang racket

(require "allocater.rkt" "stack-tools.rkt")

(provide interp MAX-ITERS)

; 2048 "word sizes". So 16 kilobytes of memory on a real machine.
(define mem-size 2048)

;                             top-of-stack reg
(define default-registers (vector mem-size 0 0 0 0 0 0 0 0))

(define ffi-fns (vector alloc free))

(define (interp opcodes)
  (interpret (list->vector opcodes) 0 (void) default-registers (make-vector mem-size)))

(define (calculate-addr op mem)
  (match op
    [(list expr1 ... '+ expr2 ...)
     (+ (calculate-addr expr1 mem)
        (calculate-addr expr2 mem))]
    [(list expr1 ... '- expr2 ...)
     (- (calculate-addr expr1 mem)
        (calculate-addr expr2 mem))]
    [(list expr1 '* (or 2 4 8))
     (* (calculate-addr expr1 mem)
        (third op))]
    [(list (list 'reg n)) (get-addr (list 'reg n) mem)]
    [(list (list n ...)) (vector-ref (cdr mem) (calculate-addr (car op) mem))]
    [(list n) n]))

(define (get-addr op mem)
  (match op
    [(list 'reg n)                         (vector-ref (car mem) n)]
    [(list 'int n)                         n]
    [(list _ ...)                       (vector-ref (cdr mem) (calculate-addr op mem))]))

(define (set-addr op value mem)
  (match op
    [(list 'reg n)                         (vector-set! (car mem) n value)]
    [(list 'int n)                         (error (~a "Cannot set integer in: " op "; did you mean " (list n) "?\n"))]
    [(list n ...)                       (vector-set! (cdr mem) (calculate-addr op mem) value)]))

(define (cmp num1 num2)
  (cond
    [(num1 . > . num2)       1]
    [(num1 . = . num2)       0]
    [else                   -1]))

(define (print-stack end mem)
  (if (= (vector-length mem) end)
      (void)
      (begin
        (print-stack (add1 end) mem)
        (display (~a " " (vector-ref mem end))))))
(define MAX-ITERS 500)

(define (interpret opcodes ip last-cmp registers memory)
  (if (MAX-ITERS . = . 0)
      (error "Too many iterations.")
      (set! MAX-ITERS (sub1 MAX-ITERS)))
  (let ([mem (cons registers memory)])
    (if (ip . >= . (vector-length opcodes))
        (begin
          (print-stack (vector-ref registers 0) memory)
          registers)
        (begin
          (set! ip (add1 ip))
          (match (vector-ref opcodes (sub1 ip))
            [(list 'debug)
             (println registers)
             (display "(")
             (print-stack (vector-ref registers 0) memory)
             (displayln ")")]
            [(list 'debug op)
             (displayln (~a "DBG: " (calculate-addr op mem)))]
            [(list 'stop)
             (set! ip (vector-length opcodes))]
            [(list 'nop) (void)]
            [(list 'mov op1 op2)
             (set-addr op1 (get-addr op2 mem) mem)]
            [(list 'lea op1 op2)
             (set-addr op1 (calculate-addr op2 mem) mem)]
            [(list 'push op)
             (push (get-addr op mem) mem)]
            [(list 'pop op)
             (set-addr op (pop mem) mem)]
            [(list 'jmp addr)
             (set! ip (get-addr addr mem))]
            [(list 'cmp op1 op2)
             (set! last-cmp (cmp (get-addr op1 mem) (get-addr op2 mem)))]
            [(list 'je addr)
             (if (last-cmp . = . 0)
                 (set! ip (get-addr addr mem))
                 (void))
             (set! last-cmp (void))]
            [(list 'jne addr)
             (unless (last-cmp . = . 0)
               (set! ip (get-addr addr mem)))
             (set! last-cmp (void))]
            [(list 'dec addr)
             (set-addr addr (sub1 (get-addr addr mem)) mem)]
            [(list 'iadd op1 op2)
             (set-addr op1
                       (+ (get-addr op1 mem)
                          (get-addr op2 mem))
                       mem)]
            [(list 'add op1 op2 op3)
             (set-addr op1
                       (+ (get-addr op2 mem)
                          (get-addr op3 mem))
                       mem)]
            [(list 'and op1 op2 op3)
             (set-addr op1
                       (bitwise-and (get-addr op2 mem)
                                    (get-addr op3 mem))
                       mem)]
            [(list 'iand op1 op2)
             (set-addr op1
                       (bitwise-and (get-addr op1 mem)
                                    (get-addr op2 mem))
                       mem)]
            [(list 'isub op1 op2)
             (set-addr op1
                       (- (get-addr op1 mem)
                          (get-addr op2 mem))
                       mem)]
            [(list 'ret)
             (set! ip (pop mem))]
            [(list 'call op)
             (push ip mem)
             (set!  ip (get-addr op mem))]
            [(list 'ffi n arg-cnt)
             (apply (vector-ref ffi-fns n) (cons mem (pop-n arg-cnt mem)))])
          (interpret opcodes ip last-cmp registers memory)))))

;push $8 ; [%s + 3]
;push $0 ; [%s + 2]
;push $1 ; [%s + 1]
;call :fib
;iadd %s $3
;stop
;
;fib:
;  cmp [%s + 3] $1
;  jne :rec
;  mov %a [%s + 1]
;  ret
;rec:
;  dec [%s + 3]
;  mov %b [%s + 1]
;  iadd [%s + 1] [%s + 2]
;  mov [%s + 2] %b
;  jmp :fib

;; pure racket: 100,000 = 516ms, 578ms if declarations included
;; interp:      18,000  = 531ms using stack
;;              100,000 = <1000ms using micro-optimised code 

