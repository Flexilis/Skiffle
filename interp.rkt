#lang racket

; 2048 "word sizes". So 16 kilobytes of memory on a real machine.
(define mem-size 2048)

;                             top-of-stack reg
(define default-registers (vector mem-size 0 0 0 0 0 0 0 0))

(define (alloc mem size)
  (println "Hi!"))

(define ffi-fns (vector alloc))

(define (interp opcodes)
  (interpret (list->vector opcodes) 0 (void) default-registers (make-vector mem-size)))

(define (calculate-addr op mem)
  (match op
    [(cons 'deref expr)
     (match expr
       [(list expr1 ... '+ expr2 ...)
        (+ (calculate-addr (cons 'deref expr1) mem)
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

(define (interpret opcodes ip last-cmp registers memory)
  (let ([mem (cons registers memory)])
    (if (ip . >= . (vector-length opcodes)) registers
        (begin
          ;;(println (vector-ref opcodes ip))
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
            [(list 'ret)
             (set! ip (pop mem))]
            [(list 'call op)
             (push (add1 ip) mem)
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
;;              100,000 = 1000 using micro-optimised code 

(time
 (interp
  '((mov (reg 3) (int 100000)) ;; 0x0
    (mov (reg 4) (int 0)) ;; 0x1
    (mov (reg 1) (int 1)) ;; 0x2
    (jmp (int 4))  ;; 0x3

    ;; fibonacci function
    (cmp (reg 3) (int 1))      ;; 0x5
    (jne (int 7))                        ;; 0x6
    (stop)                                  ;; 0x7
    (dec (reg 3))              ;; 0x8
    (mov (reg 2) (reg 1))      ;; 0x9
    (iadd (reg 1) (reg 4)) ;; 0xA
    (mov (reg 4) (reg 2))      ;; 0xB
    (jmp (int 4)))))                     ;; 0xC