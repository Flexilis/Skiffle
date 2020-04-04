#lang racket

(provide convert-instr convert)

(define (convert asm [ln 0])
  (if (null? asm) ""
      (~a
       (convert-instr (car asm) ln)
       "\n"
       (convert (cdr asm) (add1 ln)))))

(define (op->str op)
  (match op
    [(list 'reg n)
      (match n
        [0 "%STACK"]
        [1 "%FRAME"]
        [2 "%OLDFRAME"]
        [3 "%RES"]
        [4 "%FN"]
        [else (~a "%" n)])]
     [(list op) (~a #\[ (op->str op) #\])]
     [(list lop '+ rop) (~a #\[ (op->str lop) " + " (op->str rop) #\])]
     [(list lop '- rop) (~a #\[ (op->str lop) " - " (op->str rop) #\])]
     [(list 'int n) (~a "$" n)]
     [n (~a n)]))

(define (convert-instr instr [ln 0])
  (~a ln "\t"
      (match instr
        [(list ': label) (~a ":" label)]
        [(list 'iior op1 op2) (~a "iior " (op->str op1) #\space (op->str op2))]
        [(list 'xor op1 op2 op3) (~a "xor " (op->str op1) #\space (op->str op2) #\space (op->str op3))]
        [(list 'iand op1 op2) (~a "iand " (op->str op1) #\space (op->str op2))]
        [(list 'and op1 op2 op3)
         (~a "and " (op->str op1) #\space (op->str op2) #\space (op->str op3))]
        [(list 'debug) "debug"]
        [(list 'debug op) (~a "debug" (op->str op))]
        [(list 'stop) "stop"]
        [(list 'nop) "nop"]
        [(list 'mov op1 op2) (~a "mov " (op->str op1) #\space (op->str op2))]
        [(list 'lea op1 op2) (~a "lea " (op->str op1) #\space (op->str op2))]
        [(list 'push op) (~a "push " (op->str op))]
        [(list 'pop op) (~a "pop " (op->str op))]
        [(list 'jmp addr) (~a "jmp " (op->str addr))]
        [(list 'cmp op1 op2) (~a "cmp " (op->str op1) #\space (op->str op2))]
        [(list 'je addr) (~a "je " (op->str addr))]
        [(list 'jne addr) (~a "je " (op->str addr))]
        [(list 'dec addr) (~a "dec " (op->str addr))]
        [(list 'iadd op1 op2) (~a "iadd " (op->str op1) #\space (op->str op2))]
        [(list 'add op1 op2 op3)
         (~a "mov " (op->str op1) #\space (op->str op2) #\space (op->str op3))]
        [(list 'isub op1 op2) (~a "sub " (op->str op1) #\space (op->str op2))]
        [(list 'ret) "ret"]
        [(list 'call op) (~a "call " (op->str op))]
        [(list 'ffi n arg-cnt)
         (~a
          "ffi "
          (match n
            [0 "alloc"]
            [1 "free"])
          #\space
          arg-cnt)]
        [(list 'com str) (~a "; " str)])))