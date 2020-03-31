#lang racket

(require "allocater.rkt" "stack-tools.rkt")

(provide interp)

; 2048 "word sizes". So 16 kilobytes of memory on a real machine.
(define mem-size 2048)

;                             top-of-stack reg
(define default-registers (vector mem-size 0 0 0 0 0 0 0 0))

(define ffi-fns (vector alloc free))

(define (interp opcodes)
  (interpret (list->vector opcodes) 0 (void) default-registers (make-vector mem-size)))

(define (calculate-addr op mem)
  (match op
    [(cons 'deref expr)
     (match expr
       [(list expr1 ... '+ expr2 ...)
        (+ (calculate-addr (cons 'deref expr1) mem)
           (calculate-addr (cons 'deref expr2) mem))]
       [(list expr1 ... '- expr2 ...)
        (- (calculate-addr (cons 'deref expr1) mem)
           (calculate-addr (cons 'deref expr2) mem))]
       [(list expr1 '* (or 2 4 8))
        (* (calculate-addr (cons 'deref expr1) mem)
           (third expr))]
       [(list (list 'reg n)) (get-addr (list 'reg n) mem)]
       [(list (cons 'deref n)) (vector-ref (cdr mem) (calculate-addr (car expr) mem))]
       [(list n) n])]))

(define (get-addr op mem)
  (match op
    [(list 'reg n)                         (vector-ref (car mem) n)]
    [(list 'int n)                         n]
    [(cons 'deref _)                       (vector-ref (cdr mem) (calculate-addr op mem))]))

(define (set-addr op value mem)
  (match op
    [(list 'reg n)                         (vector-set! (car mem) n value)]
    [(list 'int n)                         (error (~a "Cannot set integer in: " op "; did you mean " (list 'deref n) "?\n"))]
    [(cons 'deref n)                       (vector-set! (cdr mem) (calculate-addr op mem) value)]))

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
(define MAX-ITERS 50)

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
          (print-stack (vector-ref registers 0) memory)
          (displayln "")
          (println registers)
          (println (~a "loc0: " (vector-ref memory 0)))
          (println (~a "loc1: " (vector-ref memory 1)))
          (println (vector-ref opcodes ip))
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

;; call :MAKE-FN
;; push %OLDFRAME
;; push $10
;; call [%RES]
;; END:
;; iadd %STACK $1
;; mov %FRAME %OLDFRAME
;; pop %OLDFRAME
;; stop

;; MAKE-FN:
;;   push $2
;;   ffi alloc 1
;;   mov %RES [%STACK]
;;   mov [%RES + 1] [%FRAME + 1]
;;   mov [%RES] :FN
;;   ret

;; FN:
;;   mov %OLDFRAME %FRAME
;;   mov %FRAME %STACK
;;   push [%RES - 1]
;;   push [%FRAME + 1]
;;   push [%RES - 1]
;;   iadd [%FRAME + 1] [%RES - 1]
;;   iadd %STACK $1
;;   mov %RES [%STACK]
;;   iadd %STACK $1
;;   ret

((λ (x) x)
 (time
  (interp
   '((lea (reg 1)  [deref (reg 0) - 2]) ;; 0
     (push (int 2))
     (call (int 10)) ;; 1
     (push (reg 2)) ;; 2
     (push (int 10)) ;; 3
     (call (deref (reg 3))) ;; 4
     (iadd (reg 0)  (int 1)) ;; 5
     (mov (reg 1)  (reg 2)) ;; 6
     (pop (reg 2)) ;; 7
     (stop) ;; 8
     (push (int 2)) ;; 9
     (ffi 0 1) ;; 10
     (mov (reg 3) (deref (reg 0) )) ;; 11
     (mov (deref (reg 3) + 1) (deref (reg 1) + 1)) ;; 12
     (mov (deref (reg 3)) (int 16)) ;; 13
     (ret) ;; 14
     (mov (reg 2) (reg 1) ) ;; 15
     (mov (reg 1)  (reg 0) ) ;; 16
     (push (deref (reg 3) - 1)) ;; 17
     (push (deref (reg 1) + 1)) ;; 18
     (push (deref (reg 3) - 1)) ;; 19
     (iadd [deref (reg 1) + 1] [deref (reg 3) + 1]) ;; 20
     (iadd (reg 0)  (int 1)) ;; 21
     (mov (reg 3) (deref (reg 0) )) ;; 22
     (iadd (reg 0)  (int 1)) ;; 23
     (ret))))) ;; 24



