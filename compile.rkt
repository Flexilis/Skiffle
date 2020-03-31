#lang racket

(provide compile)

(define %STACK '(reg 0))
(define %FRAME '(reg 1))
(define %RES   '(reg 2))


;; args ret-id locvars freevars

((lambda (x) (+ x y)) 10)

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
;;   ffi alloc
;;   mov %RES [%STACK]
;;   mov [%RES + 1] [%FRAME - 1]
;;   mov [%RES] :FN
;;   ret

;; FN:
;;   mov %OLDFRAME %FRAME
;;   mov %FRAME %STACK
;;   push [%RES + 1]
;;   push [%FRAME - 1]
;;   push [%RES + 1]
;;   iadd [%FRAME - 1] [%RES + 1]
;;   iadd %STACK $1
;;   mov %RES [%STACK]
;;   iadd %STACK $1
;;   ret

;; mov FRAME STACK
;; isub STACK (n glob vars)
;; ~ prog ~
;; mov RES STACK

(define (compile code)
  (match code
    [(list (list-rest 'lambda _ (list boundvars ...) _ body))
     (append `((mov ,%FRAME ,%STACK)
               (isub ,%STACK (int ,(length boundvars))))
             (apply append
                    (map compile-expr body))
             `((mov ,%RES (deref ,%STACK))))]))

(define (compile-expr expr)
  (match expr
    [(? integer?)
     (list `(push (int ,expr)))]
    [(list 'var offset)
     (list `(push (deref ,%FRAME + ,offset)))]
    [(list '+ lop rop)
     (append (compile-expr lop)
             (compile-expr rop)
             `((iadd (deref ,%STACK + 1) (deref ,%STACK))
               (iadd ,%STACK (int 1))))]
    [(list 'let (list (list offsets values) ...) body ...)
     (append
      (apply append
             (map compile-expr values))
      (for/list ([offset (reverse offsets)]
                 [stack-offset (in-range (length offsets))])
        `(mov (deref ,%FRAME + ,offset) (deref ,%STACK + ,stack-offset)))
      `((iadd ,%STACK (int ,(length offsets))))
      (apply append
             (map compile-expr body)))]
    [(list fn args ...)
     (append
      (for/list ([arg args])
        (compile-expr arg))
      `((call )))]))







