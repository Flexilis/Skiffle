#lang racket

(provide compile)

(define gen-label
  (let ([labels 0])
    (λ ()
      (set! labels (add1 labels))
      (list ': labels))))

(define suffix '())

(define PROC-IND (list 'int -1)) ;; string eventually

(define (unencode loc)
  (let ([lbl1 (gen-label)]
        [lbl2 (gen-label)])
    `(,@(untag-int loc `((jmp ,lbl1)))
      (jmp ,lbl2)
      ,lbl1
      (push ,PROC-IND)
      ,lbl2)))

(define (compile code)
  (match code
    [(list (list 'lambda '() boundvars '() body ...))
     `((mov ,%FRAME ,%STACK)
       (isub ,%STACK (int ,(length boundvars)))
       ,@(apply append
                (for/list ([expr body])
                  (compile-expr expr '())))
       ,@(unencode `[,%STACK])
       (pop ,%RES)
       (stop)
       ,@(apply append suffix))]))

(define %STACK '(reg 0))
(define %FRAME '(reg 1))
(define %OLDFRAME '(reg 2))
(define %RES   '(reg 3))
(define %FN    '(reg 4))
(define ALLOC 0) ;; ffi index

(define (flatten-double-lists l)
  (match l
    ['() '()]
    [(cons (list (or 'ret 'debug 'stop)) r) (cons (car l) (flatten-double-lists r))]
    [(cons (or (list x) x) r) (cons x (flatten-double-lists r))]))

(define (tag-as-int n)
  (bitwise-ior n (expt 2 63)))

(define INT-UNMASK (list 'int (bitwise-not (expt 2 63))))

(define (untag-int loc [error '((mov (reg 9) -1) (stop))])
  (let ([lbl (gen-label)])
    `((and (reg 6) ,loc (int ,(expt 2 63)))
      (cmp (reg 6) (int 1))
      (je ,lbl)
      ,@error
      ,lbl
      (iand ,loc ,INT-UNMASK))))

(define (emit-fn-maker freevars args label)
  (match freevars
    [(list (list local-offsets global-offsets) ...)
     `((push (int ,(add1 (length freevars))))
       (ffi ,ALLOC 1)
       ,@(for/list ([loc-offset local-offsets]
                    [glob-offset global-offsets]
                    [ind (in-range (length local-offsets))])
           `(mov [,%RES + ,(- loc-offset)] [,%FRAME + ,glob-offset]))
     (mov [,%RES] ,label)
     (push ,%RES))]))

(define (copy-freevars freevars)
  (for/list ([var (map car freevars)])
    `(mov [,%FRAME + ,var] [,%FN + ,(- var)])))

(define (emit-fn-body freevars boundvars body label)
  `(,label
    (mov ,%OLDFRAME ,%FRAME)
    (mov ,%FRAME ,%STACK)
    (isub ,%STACK (int ,(+ (length freevars) (length boundvars))))
    ,@(copy-freevars freevars)
    ,@(apply append
             (for/list ([expr body])
               (compile-expr expr '())))
    (mov ,%RES [,%STACK])
    (mov ,%STACK ,%FRAME)
    (mov ,%FRAME ,%OLDFRAME)
    (ret)))
  
(define (compile-expr expr state)
  (println expr)
  (flatten-double-lists
   (match expr
     [(? number?) `((push (int ,(tag-as-int expr))))]
     [(list 'var offset) `((push [,%FRAME + ,offset]))]
     [(list (or '+ '-) lop rop)
      (println lop)
      `(,@(compile-expr lop state)
        ,@(compile-expr rop state)
        ,@(untag-int %STACK + 1)
        ,@(untag-int %STACK)
        (,(if (eq? (car expr) '+) 'iadd 'isub) [,%STACK + 1] [,%STACK])
        (iadd ,%STACK (int 1)))]
     [(list 'let (list (list offsets values) ...) body ...)
      (append (apply append
                     (for/list ([value values] [offset offsets])
                       (append (compile-expr value state)
                               `((mov [,%FRAME + ,offset] [,%STACK])))))
              `((iadd ,%STACK (int ,(length offsets)))
                ,@(apply append
                         (for/list ([expr body])
                           (compile-expr expr state)))))]
     [(list 'lambda (list freevars ...) (list boundvars ...) (list args ...) body ...)
      (let ([fn-body-label (gen-label)])
        (set! suffix
              (cons (emit-fn-body freevars boundvars body fn-body-label) suffix))
        (emit-fn-maker freevars args fn-body-label))]
     [(list fn args ...)
      `((push ,%OLDFRAME)
        (push ,%FN)
        (push (reg 5))
        (mov (reg 5) ,%STACK)
        ,@(compile-expr fn state)
        (pop ,%FN)
        (mov ,%STACK (reg 5))
        (pop (reg 5))
        ,@(apply append
                 (for/list ([arg args])
                   (compile-expr arg state)))
        (call (,%FN))
        (iadd ,%STACK (int ,(length args)))
        (pop ,%FN)
        (pop ,%OLDFRAME)
        (push ,%RES))])))

