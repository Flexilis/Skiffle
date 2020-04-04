#lang racket

(provide replace-labels)

(define (replace-labels asm)
  (match asm
    [(cons labels asm)
     (for/list ([instr asm]) (convert-instr instr labels))]))

(define (convert-op op labels)
  (match op
    [(list ': name) (list 'int (hash-ref labels name))]
    [else op]))

(define (convert-instr instr labels)
  (match instr
    [(list 'com str) '(nop)]
    [(list 'and op1 op2 op3) (list 'and (convert-op op1 labels) (convert-op op2 labels) (convert-op op3 labels))]
    [(list 'debug) instr]
    [(list 'debug op) (list 'debug (convert-op op labels))]
    [(list 'stop) instr]
    [(list 'nop) instr]
    [(list 'mov op1 op2) (list 'mov (convert-op op1 labels) (convert-op op2 labels))]
    [(list 'lea op1 op2) (list 'lea (convert-op op1 labels) (convert-op op2 labels))]
    [(list 'push op) (list 'push (convert-op op labels))]
    [(list 'pop op) (list 'pop (convert-op op labels))]
    [(list 'jmp addr) (list 'jmp (convert-op addr labels))]
    [(list 'cmp op1 op2) (list 'cmp (convert-op op1 labels) (convert-op op2 labels))]
    [(list 'iand op1 op2) (list 'iand (convert-op op1 labels) (convert-op op2 labels))]
    [(list 'je addr) (list 'je (convert-op addr labels))]
    [(list 'jne addr) (list 'jne (convert-op addr labels))]
    [(list 'dec addr) (list 'dec (convert-op addr labels))]
    [(list 'iadd op1 op2) (list 'iadd (convert-op op1 labels) (convert-op op2 labels))]
    [(list 'add op1 op2 op3)
     (list 'mov (convert-op op1 labels) (convert-op op2 labels) (convert-op op3 labels))]
    [(list 'isub op1 op2) (list 'isub (convert-op op1 labels) (convert-op op2 labels))]
    [(list 'ret) instr]
    [(list 'call op) (list 'call (convert-op op labels))]
    [(list 'ffi n arg-cnt) instr]))