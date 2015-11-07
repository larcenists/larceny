; Copyright 2012 Lars T Hansen.
;
; Simple disassembler for ARM-32 code.
;
; Support only as much as we need to handle the ARM-32 code generator.
; Everything else disassembles as .word opcodes.
;
; (disassemble-codevector cv) -> (instr ...)
; (format-instruction instr) -> string
; (print-instructions (instr ...) [port]) -> unspecified

(define armdis.endian 'little)          ; 'big should also work

(define (disassemble-codevector cv)

  (define loc 0)

  (define (decode b3 b2 b1 b0)
    (let ((cnd (fxlogand b3 #xF0))
          (s   (fxlogand b2 #x10)))
      (if (= cnd 15)
          (data b3 b2 b1 b0)
          (case (fxlogand b3 #x0F)
            ((#b00000000)
             (case (fxlogand b2 #xE0)
               ((#b00000000) 
                (case (fxlogand b0 #xF0)
                  ((#b00000000)    (alu-1d2 "and" s cnd b2 b1 b0))
                  ((#b10010000)    (alu-d21 "mul" s cnd b2 b1 b0))
                  (else            (data b3 b2 b1 b0))))
               ((#b00100000)       (alu-1d2 "eor" s cnd b2 b1 b0))
               ((#b01000000)       (alu-1d2 "sub" s cnd b2 b1 b0))
               ((#b10000000)       (alu-1d2 "add" s cnd b2 b1 b0))
               ((#b11000000)       (alu-dd21 "smull" s cnd b2 b1 b0))
               (else               (data b3 b2 b1 b0))))
            ((#b00000001)
             (case (fxlogand b2 #xE0)
               ((#b00100000)
                (case s
                  ((0)             (alu-xxs "blx" 0 cnd b2 b1 b0))
                  (else            (data b3 b2 b1 b0))))
               ((#b01000000)
                (case s
                  ((#x10)          (alu-1x2 "cmp" 0 cnd b2 b1 b0))
                  (else            (data b3 b2 b1 b0))))
               ((#b10000000)       (alu-1d2 "orr" s cnd b2 b1 b0))
               ((#b10100000)
                (case (fxlogand b0 #xF0)
                  ((#b00000000) 
                   (case (fxlogand b1 #x0F)
                     ((#b00000000) (alu-xds "mov" s cnd b2 b1 b0))
                     (else         (shift-dis "lsli" s cnd b1 b0))))
                  ((#b00010000)    (shift-d21 "lsl" s cnd b1 b0))
                  ((#b00100000 #b10100000)
                                   (shift-dis "lsri" s cnd b1 b0))
                  ((#b00110000)    (shift-d21 "lsr" s cnd b1 b0))
                  ((#b01000000 #b11000000)
                                   (shift-dis "asri" s cnd b1 b0))
                  (else            (data b3 b2 b1 b0))))
               ((#b11100000)       (alu-xds "mvn" s cnd b2 b1 b0))
               (else               (data b3 b2 b1 b0))))
            ((#b00000010)
             (case (fxlogand b2 #xE0)
               ((#b00000000)       (alu-sdi "andi" s cnd b2 b1 b0))
               ((#b01000000)       (alu-sdi "subi" s cnd b2 b1 b0))
               ((#b10000000)       (alu-sdi "addi" s cnd b2 b1 b0))
               (else               (data b3 b2 b1 b0))))
            ((#b00000011)
             (case (fxlogand b2 #xE0)
               ((#b00000000)
                (case s
                  ((0)             (alu-idii "movwi" cnd b2 b1 b0))
                  ((#x10)          (alu-si "tsti" 0 cnd b2 b1 b0))))
               ((#b01000000)
                (case s
                  ((0)             (alu-idii "movti" cnd b2 b1 b0))
                  ((#x10)          (alu-si "cmpi" 0 cnd b2 b1 b0))))
               ((#b10100000)       (alu-di "movi" s cnd b1 b0))
               ((#b11000000)       (alu-sdi "bici" s cnd b2 b1 b0))
               ((#b11100000)       (alu-di "mvni" s cnd b1 b0))
               (else               (data b3 b2 b1 b0))))
            ((#b00000101)
             (case (fxlogand b2 #x70)
               ((#b00000000)       (mem-bsi "stri" cnd b2 b1 b0))
               ((#b00010000)       (mem-bsi "ldri" cnd b2 b1 b0))
               ((#b01000000)       (mem-bsi "strbi" cnd b2 b1 b0))
               ((#b01010000)       (mem-bsi "ldrbi" cnd b2 b1 b0))
               (else               (data b3 b2 b1 b0))))
            ((#b00000111)
             (case (fxlogand b2 #xF0)
               ((#b10000000)       (mem-bsx "str" cnd b2 b1 b0))
               ((#b10010000)       (mem-bsx "ldr" cnd b2 b1 b0))
               ((#b11000000)       (mem-bsx "strb" cnd b2 b1 b0))
               ((#b11010000)       (mem-bsx "ldrb" cnd b2 b1 b0))
               (else               (data b3 b2 b1 b0))))
            ((#b00001010)          (branch "b" cnd b2 b1 b0))
            ((#b00001101)
             (case (fxlogand b1 #x0F)
               ((#b00001011)
                (case (fxlogand b2 #b00110000)
                  ((#b00010000)    (fmem-bsi "vldri" cnd b2 b1 b0))
                  ((#b00000000)    (fmem-bsi "vstri" cnd b2 b1 b0))
                  (else            (data b3 b2 b1 b0))))
               (else               (data b3 b2 b1 b0))))
            ((#b00001110)
             (case (fxlogand b2 #b10110000)
               ((#b00110000)
                (case (fxlogand b1 #b00001110)
                  ((#b00001010)
                   (case (fxlogand b0 #b01010000)
                     ((#b00000000) (falu-1d2 "vadd" cnd b2 b1 b0))
                     ((#b01000000) (falu-1d2 "vsub" cnd b2 b1 b0))
                     (else         (data b3 b2 b1 b0))))
                  (else            (data b3 b2 b1 b0))))
               ((#b00100000)
                (case (fxlogand b1 #b00001110)
                  ((#b00001010)
                   (case (fxlogand b0 #b01010000)
                     ((#b00000000) (falu-1d2 "vmul" cnd b2 b1 b0))
                     (else         (data b3 b2 b1 b0))))
                  (else            (data b2 b1 b1 b0))))
               ((#b10000000)
                (case (fxlogand b1 #b00001110)
                  ((#b00001010)
                   (case (fxlogand b0 #b01010000)
                     ((#b00000000) (falu-1d2 "vdiv" cnd b2 b1 b0))
                     (else         (data b3 b2 b1 b0))))
                  (else            (data b3 b2 b1 b0))))
               ((#b10110000)
                (case b2
                  ((#b10110100
                    #b11110100)    (falu-x12 "vcmp" cnd b2 b1 b0))
                  ((#b11110001)
                   (case (fxlogand b1 #x0F)
                     ((#b00001010)
                      (case (fxlogand b0 #x10)
                        ((#b00010000)
                                   (if (= 15 (fxrshl b1 4))
                                       (list (op "vmrs" cnd) "APSR_nzcv")
                                       (list (op "vmrs" cnd)
                                             (reg (fxrshl b1 4)))))
                        (else      (data b3 b2 b1 b0))))
                     (else         (data b3 b2 b1 b0))))
                  (else            (data b3 b2 b1 b0))))
               (else               (data b3 b2 b1 b0))))
            (else                  (data b3 b2 b1 b0))))))

  (define (alu-1d2 name s cnd b2 b1 b0)     ; add etc
    (list (ops name s cnd)
          (reg (fxrshl b1 4))
          (reg (fxlogand b2 15))
          (reg (fxlogand b0 15))))

  (define (alu-1x2 name s cnd b2 b1 b0)     ; cmp
    (list (ops name s cnd)
          (reg (fxlogand b2 15))
          (reg (fxlogand b0 15))))

  (define (alu-d21 name s cnd b2 b1 b0)     ; mul
    (list (ops name s cnd)
          (reg (fxlogand b2 15))
          (reg (fxlogand b0 15))
          (reg (fxrshl b1 4))))

  (define (alu-xds name s cnd b2 b1 b0)     ; mov etc
    (list (ops name s cnd)
          (reg (fxrshl b1 4))
          (reg (fxlogand b0 15))))

  (define (alu-xxs name s cnd b2 b1 b0)     ; blx
    (list (ops name s cnd)
          (reg (fxlogand b0 15))))

  (define (alu-dd21 name s cnd b2 b1 b0)    ; smull
    (list (ops name s cnd)
          (reg (fxlogand b2 15))
          (reg (fxrshl b1 4))
          (reg (fxlogand b0 15))
          (reg (fxlogand b1 15))))

  (define (shift-d21 name s cnd b1 b0)      ; lsl etc
    (list (ops name s cnd)
          (reg (fxrshl b1 4))
          (reg (fxlogand b0 15))
          (reg (fxlogand b1 15))))

  (define (shift-dis name s cnd b1 b0)      ; lsli etc
    (list (ops name s cnd)
          (reg (fxrshl b1 4))
          (reg (fxlogand b0 15))
          (+ (* (fxlogand b1 15) 2)
             (fxrshl b0 7))))

  (define (alu-sdi name s cnd b2 b1 b0)     ; addi etc
    (list (ops name s cnd)
          (reg (fxrshl b1 4))
          (reg (fxlogand b2 15))
          (num (expand-imm (fxlogand b1 15) b0))))

  (define (alu-di name s cnd b1 b0)         ; movi etc
    (list (ops name s cnd)
          (reg (fxlogand b1 15))
          (num (expand-imm (fxlogand b1 15) b0))))

  (define (alu-si name s cnd b2 b1 b0)      ; cmpi etc
    (list (ops name s cnd)
          (reg (fxlogand b2 15))
          (num (expand-imm (fxlogand b1 15) b0))))

  (define (alu-idii name cnd b2 b1 b0)      ; movwi etc
    (list (op name cnd)
          (reg (fxrshl b1 4))
          (num (+ (* 4096 (fxlogand b2 15))
                  (* 256 (fxlogand b1 15)) b0))))

  (define (mem-bsi name cnd b2 b1 b0)       ; ldri etc
    (list (op name cnd)
          (reg (fxrshl b1 4)) 
          (list (reg (fxlogand b2 15))
                (* (if (zero? (fxlogand b2 #x80)) -1 1) 
                   (+ (* 256 (fxlogand b1 15)) b0)))))

  (define (mem-bsx name cnd b2 b1 b0)       ; ldr etc
    (list (op name cnd)
          (reg (fxrshl b1 4))
          (list (reg (fxlogand b2 15))
                (reg (fxlogand b0 15)))))

  (define (falu-1d2 name cnd b2 b1 b0)      ; vadd etc
    (list (op name cnd)
          (freg (+ (fxrshl (fxlogand b2 #x40) 2) (fxrshl b1 4)))      ; D:Vd
          (freg (+ (fxrshl (fxlogand b0 #x80) 3) (fxlogand b2 15)))   ; N:Vn
          (freg (+ (fxrshl (fxlogand b0 #x20) 1) (fxlogand b0 15))))) ; M:Vm

  (define (falu-x12 name cnd b2 b1 b0)      ; vcmp
    (list (op name cnd)
          (freg (+ (fxrshl (fxlogand b2 #x40) 2) (fxrshl b1 4)))      ; D:Vd
          (freg (+ (fxrshl (fxlogand b0 #x20) 1) (fxlogand b0 15))))) ; M:Vm

  (define (fmem-bsi name cnd b2 b1 b0)      ; vldri etc
    (list (op name cnd) 
          (freg (fxrshl b1 4))
          (list (reg (fxlogand b2 15))
                (* (if (zero? (fxlogand b2 #x80)) -1 1) b0 4))))

  (define (branch name cnd b2 b1 b0)        ; b
    (list (op name cnd)
          (num (+ 8
                  (- loc 4)
                  (* 4 (ext24 (+ (* b2 65536)
                                 (* b1 256)
                                 b0)))))))

  (define (data b3 b2 b1 b0)
    (list ".word" (num (+ (* 16777216 b3) (* 65536 b2) (* 256 b1) b0))))

  (define (freg n)
    (string-append "d" (number->string n)))

  (define (reg n)
    (cond ((= n 15) "pc")
          ((= n 14) "lr")
          ((= n 13) "sp")
          (else     (string-append "r" (number->string n)))))

  (define (num n)
    (string-append "#x" (number->string n 16)))

  (define (ext24 n)
    (if (zero? (fxlogand n #x800000))
        n
        (- (+ n #xFF000000) #x100000000)))

  (define (expand-imm rotation imm)
    (if (zero? rotation)
        imm
        (expand-imm (- rotation 1)
                    (+ (* (remainder imm 4) 1073741824)
                       (quotient imm 4)))))

  (define (op name cnd)
    (ops name 0 cnd))

  (define (ops name s cnd)
    (string-append 
     name
     (if (zero? s) "" "s")
     (vector-ref '#(".eq" ".ne" ".cs" ".cc" ".mi" ".pl" ".vs" ".vc"
                    ".hi" ".ls" ".ge" ".lt" ".gt" ".le" "") 
                 (quotient cnd 16))))

  (let ((o3 (if (eq? armdis.endian 'little) 3 0))
        (o2 (if (eq? armdis.endian 'little) 2 1))
        (o1 (if (eq? armdis.endian 'little) 1 2))
        (o0 (if (eq? armdis.endian 'little) 0 3)))
    (let loop ((is '()))
      (if (= loc (bytevector-length cv))
          (reverse is)

          ;; TODO: choose field width intelligently

          (let ((addr (armdis.hex loc 4))
                (b3   (bytevector-ref cv (+ loc o3)))
                (b2   (bytevector-ref cv (+ loc o2)))
                (b1   (bytevector-ref cv (+ loc o1)))
                (b0   (bytevector-ref cv (+ loc o0))))
            (set! loc (+ loc 4))
            (let ((x (decode b3 b2 b1 b0)))
              (loop (cons (cons addr (cons (list b3 b2 b1 b0) x)) is))))))))

(define print-instructions
  (case-lambda 
   ((ilist)
    (print-instructions ilist (current-output-port)))
   ((ilist out)
    (for-each (lambda (i) 
                (display (format-instruction i) out)
                (newline out))
              ilist)
    (unspecified))))

(define (format-instruction i)
  (with-output-to-string
    (lambda ()
      (define (comma-separated opds)
        (if (pair? (car opds))
            (begin
              (display "[")
              (comma-separated (car opds))
              (display "]"))
            (display (car opds)))
        (if (not (null? (cdr opds)))
            (begin
              (display ", ")
              (comma-separated (cdr opds)))))

      (display (car i))
      (display #\tab)
      (for-each (lambda (x) 
                  (display (armdis.hex x 2))
                  (display #\space))
                (cadr i))
      (display #\tab)
      (display (caddr i))
      (let ((opds (cdddr i)))
        (if (not (null? opds))
            (begin (display #\tab)
                   (comma-separated opds)))))))

(define (armdis.hex n digits)
  (let ((s (number->string (+ n (expt 16 digits)) 16)))
    (substring s 1 (string-length s))))
