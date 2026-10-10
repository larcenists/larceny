; Test for the x86-64 encoder and Cant primitives in
; src/Asm/Fence/pass5p2-x86-64.sch.
;
; Loading this file (from the Larceny root directory) writes two files
; into the current directory:
;
;   encoder-test-expected.s   the expected instructions, in Intel syntax
;   encoder-test-actual.s     the bytes our encoder produced, as .byte
;
; Each test case is in a section of its own, so its offsets start at 0
; in both files.  run-encoder-test.sh assembles both files with GNU as
; and compares their disassemblies: the encoder need not choose the
; same encodings as GNU as, only encodings of the same instructions.
;
; This file supplies a minimal stand-in for the assembler structure of
; src/Asm/Shared/pass5p1.sch, so the encoder can be exercised on its
; own.

(define $r.result -1)
(define $r.second -2)
(define $r.stkp -3)
(define $r.globals -4)
(define $r.tmp -5)
(define $r.reg0 0)
(define *nregs* 8)
(define *lastreg* 7)
(define $bytewidth.wordsize 8)
(define $bytewidth.code-align 4)
(define $tag.bytevector-tag 5)
(define $p.codevector 1)
(define $imm.true 6)
(define $imm.false 2)

(define x86-64.TMP1 0)
(define x86-64.TMP0 1)
(define x86-64.TMP2 2)
(define x86-64.GLOBALS 3)
(define x86-64.STKP 5)
(define x86-64.RESULT 6)
(define x86-64.SECOND 7)
(define x86-64.REG0 8)
(define x86-64.REG1 9)
(define x86-64.REG2 10)
(define x86-64.REG3 11)
(define x86-64.REG4 12)
(define x86-64.REG5 13)
(define x86-64.REG6 14)
(define x86-64.REG7 15)

(define fence.records-registers? #f)
(define (fence.note-register! r) #t)
(define (make-code-features . rest) rest)

; The stand-in assembler: a list of emitted bytevectors (in reverse),
; the location counter, and a list of fixup procedures.

(define (make-test-as) (vector '() 0 '()))

(define (emit! as bv)
  (vector-set! as 0 (cons bv (vector-ref as 0)))
  (vector-set! as 1 (+ (vector-ref as 1) (bytevector-length bv))))

(define (here as) (vector-ref as 1))

(define (label-value as L) (cdr L))

(define (emit-label! as L) (set-cdr! L (here as)))

(define (emit-fixup-proc! as proc)
  (vector-set! as 2 (cons (cons (here as) proc) (vector-ref as 2))))

(define (test-as-code as)
  (let* ((n  (here as))
         (cv (make-bytevector n 0)))
    (let loop ((bvs (reverse (vector-ref as 0))) (i 0))
      (if (pair? bvs)
          (let ((bv (car bvs)))
            (do ((j 0 (+ j 1)))
                ((= j (bytevector-length bv)))
              (bytevector-set! cv (+ i j) (bytevector-ref bv j)))
            (loop (cdr bvs) (+ i (bytevector-length bv))))))
    (for-each (lambda (f) ((cdr f) cv (car f)))
              (reverse (vector-ref as 2)))
    cv))

(load "src/Asm/Fence/pass5p2-x86-64.sch")

(define (L) (cons 'label #f))

; Test cases: the expected instructions (lines of Intel syntax for GNU
; as; "L:" defines the label L, and "{disp32}" forces a long branch,
; which the encoder uses for forward branches) and a procedure that
; emits them, given the assembler and a label.

(define test-cases
  (list
   ;; Loads and stores, with the base registers that need SIB bytes or
   ;; displacements, byte registers that need REX, and wide displacements.
   (list "mov rdi, [rsi+7]"
         (lambda (as L) (cant.ldi as $r.result 7 $r.second)))
   (list "mov rsi, [rbp+0]"
         (lambda (as L) (cant.ldi as $r.stkp 0 $r.result)))
   (list "mov r8, [r12+16]"
         (lambda (as L) (cant.ldi as 4 16 0)))
   (list "mov r9, [r13+0]"
         (lambda (as L) (cant.ldi as 5 0 1)))
   (list "mov r9, [r12]"
         (lambda (as L) (cant.ldi as 4 0 1)))
   (list "mov r15, [rbx+1000]"
         (lambda (as L) (cant.ldi as $r.globals 1000 7)))
   (list "mov rcx, [rsi-5]"
         (lambda (as L) (cant.ldi as $r.result -5 $r.tmp)))
   (list "movzx edi, BYTE PTR [rsi+3]"
         (lambda (as L) (cant.ldbiz as $r.result 3 $r.second)))
   (list "movzx r12d, BYTE PTR [r13+300]"
         (lambda (as L) (cant.ldbiz as 5 300 4)))
   (list "mov r8d, DWORD PTR [rsi-5]"
         (lambda (as L) (cant.ldwiz as $r.result -5 0)))
   (list "mov [rbp+8], r10"
         (lambda (as L) (cant.sti as 2 $r.stkp 8)))
   (list "mov [r12+8], rsi"
         (lambda (as L) (cant.sti as $r.result 4 8)))
   (list "mov QWORD PTR [rsi+3], 0"
         (lambda (as L) (cant.stzi as $r.result 3)))
   (list "mov [rsi+11], dil"
         (lambda (as L) (cant.stbi as $r.second $r.result 11)))
   (list "mov [rdi+11], sil"
         (lambda (as L) (cant.stbi as $r.result $r.second 11)))
   (list "mov [rsi+11], r9b"
         (lambda (as L) (cant.stbi as 1 $r.result 11)))
   (list "mov [rsi+11], cl"
         (lambda (as L) (cant.stbi as $r.tmp $r.result 11)))
   (list "mov [rsi+3], edi"
         (lambda (as L) (cant.stwi as $r.second $r.result 3)))
   (list "mov [rsi+3], r15d"
         (lambda (as L) (cant.stwi as 7 $r.result 3)))

   ;; Immediates and moves.
   (list "mov esi, 0"
         (lambda (as L) (cant.movi as 0 $r.result)))
   (list "mov r12d, 5"
         (lambda (as L) (cant.movi as 5 4)))
   (list "mov edi, 0xffffffff"
         (lambda (as L) (cant.movi as #xffffffff $r.second)))
   (list "mov rsi, -1"
         (lambda (as L) (cant.movi as -1 $r.result)))
   (list "mov r9, -2147483648"
         (lambda (as L) (cant.movi as -2147483648 1)))
   (list "movabs rsi, 0x123456789"
         (lambda (as L) (cant.movi as #x123456789 $r.result)))
   (list "movabs r15, 0x8000000000000000"
         (lambda (as L) (cant.movi as (- (expt 2 63)) 7)))
   (list "mov r8, rsi"
         (lambda (as L) (cant.mov as $r.result 0)))
   (list "mov rsi, r13"
         (lambda (as L) (cant.mov as 5 $r.result)))

   ;; Returns, millicode calls, jumps.
   (list "jmp QWORD PTR [rbp+8]"
         (lambda (as L) (cant.ldrtn/return as 8)))
   (list "nop\nnop\ncall QWORD PTR [rbx+1024]"
         (lambda (as L) (cant.trap as 1024 'foo)))
   (list "mov ecx, 13\ncall QWORD PTR [rbx+64]"
         (lambda (as L) (cant.trap as 64 'foo 13)))
   (list "lea rax, [rip+L]\nmov [rbp+8], rax\nL:"
         (lambda (as L) (cant.setrtn/strtn as L 8) (cant.label as L)))
   (list "L:\nlea rax, [rip+L]\nmov [rbp+16], rax"
         (lambda (as L) (cant.label as L) (cant.setrtn/strtn as L 16)))
   (list "mov rax, [r8+1]\nadd rax, 3\njmp rax"
         (lambda (as L) (cant.jump as #f)))
   (list "mov rax, [r8+1]\nadd rax, 16\njmp rax"
         (lambda (as L) (cant.jump as L) (cant.label as L)))
   (list "nop\nnop\nL:\nmov rax, [r8+1]\nadd rax, 5\njmp rax"
         (lambda (as L) (cant.nop as) (cant.nop as) (cant.label as L) (cant.jump as L)))
   (list "mov rax, [r8+1]\nadd rax, 3\nnop\nnop\nnop\nlea rdx, [rip+6]\nmov [rbp+8], rdx\njmp rax"
         (lambda (as L) (cant.setrtn/strtn/jump as #f 4 8)))
   (list "lea rdx, [rip+9]\nmov [rbp+8], rdx\n{disp32} jmp L\nL:"
         (lambda (as L) (cant.setrtn/strtn/branch as L 4 8) (cant.label as L)))
   (list "L:\nnop\nlea rdx, [rip+12]\nmov [rbp+1000], rdx\n{disp32} jmp L\nnop\nnop\nnop"
         (lambda (as L) (cant.label as L) (cant.setrtn/strtn/branch as L 4 1000)
                 (cant.nop as) (cant.nop as) (cant.nop as)))

   ;; Branches.
   (list "{disp32} jmp L\nL:"
         (lambda (as L) (cant.ba as L) (cant.label as L)))
   (list "L:\njmp L"
         (lambda (as L) (cant.label as L) (cant.ba as L)))
   (list "L:\n.rept 200\nnop\n.endr\njmp L"
         (lambda (as L) (cant.label as L)
                 (do ((i 0 (+ i 1))) ((= i 200)) (cant.nop as))
                 (cant.ba as L)))
   (list "test rsi, rsi\n{disp32} je L\nL:"
         (lambda (as L) (cant.bcci as 'equal $r.result 0 L) (cant.label as L)))
   (list "L:\ntest r12, r12\njne L"
         (lambda (as L) (cant.label as L) (cant.bcci as 'not-equal 4 0 L)))
   (list "L:\n.rept 200\nnop\n.endr\ncmp rsi, rdi\njl L"
         (lambda (as L) (cant.label as L)
                 (do ((i 0 (+ i 1))) ((= i 200)) (cant.nop as))
                 (cant.bcc as 'less $r.result $r.second L)))
   (list "test sil, 3\n{disp32} je L\nL:"
         (lambda (as L) (cant.bcci as 'low2-equal $r.result 0 L) (cant.label as L)))
   (list "test r9b, 1\n{disp32} jne L\nL:"
         (lambda (as L) (cant.bcci as 'low1-not-equal 1 0 L) (cant.label as L)))
   (list "mov rax, r9\nand rax, 7\ncmp rax, 5\n{disp32} jne L\nL:"
         (lambda (as L) (cant.bcci as 'low3-not-equal 1 5 L) (cant.label as L)))
   (list "mov rax, rsi\nand rax, 255\ncmp rax, rdi\n{disp32} je L\nL:"
         (lambda (as L) (cant.bcc as 'low8-equal $r.result $r.second L) (cant.label as L)))
   (list "cmp rsi, 100000\n{disp32} jg L\nL:"
         (lambda (as L) (cant.bcci as 'greater $r.result 100000 L) (cant.label as L)))
   (list "cmp rsi, -8\n{disp32} jle L\nL:"
         (lambda (as L) (cant.bcci as 'less-or-equal $r.result -8 L) (cant.label as L)))
   (list "cmp rsi, 0\n{disp32} jge L\nL:"
         (lambda (as L) (cant.bcci as 'greater-or-equal $r.result 0 L) (cant.label as L)))
   (list "movabs rax, 0x10000000000\ncmp rsi, rax\n{disp32} jb L\nL:"
         (lambda (as L) (cant.bcci as 'unsigned-less $r.result (expt 2 40) L) (cant.label as L)))
   (list "cmp r8, r15\n{disp32} jae L\nL:"
         (lambda (as L) (cant.bcc as 'unsigned-greater-or-equal 0 7 L) (cant.label as L)))

   ;; Conditions to booleans.
   (list "cmp rsi, rdi\nsetl al\nmovzx eax, al\nlea rdi, [rax*4+2]"
         (lambda (as L) (cant.setcc as 'less $r.result $r.second $r.second)))
   (list "test r9b, 7\nsete al\nmovzx eax, al\nlea r10, [rax*4+2]"
         (lambda (as L) (cant.setcci as 'low3-equal 1 0 2)))

   ;; Arithmetic.
   (list "add rsi, 8"
         (lambda (as L) (cant.addi as $r.result 8 $r.result)))
   (list "add rsi, 1000"
         (lambda (as L) (cant.addi as $r.result 1000 $r.result)))
   (list "lea rdi, [rsi+8]"
         (lambda (as L) (cant.addi as $r.result 8 $r.second)))
   (list "lea rdi, [rsi-1000]"
         (lambda (as L) (cant.subi as $r.result 1000 $r.second)))
   (list "movabs rax, 0x100000000\nlea rdi, [rsi+rax*1]"
         (lambda (as L) (cant.addi as $r.result (expt 2 32) $r.second)))
   (list "mov rax, rsi\nadd rax, 4\ncmovno rdi, rax\n{disp32} jno L\nL:"
         (lambda (as L) (cant.addi/bnv as $r.result 4 $r.second L) (cant.label as L)))
   (list "mov rax, rsi\nsub rax, 4\ncmovno rsi, rax\n{disp32} jno L\nL:"
         (lambda (as L) (cant.subi/bnv as $r.result 4 $r.result L) (cant.label as L)))
   (list "mov rax, rsi\nadd rax, rdi\ncmovno r8, rax\n{disp32} jno L\nL:"
         (lambda (as L) (cant.add/bnv as $r.result $r.second 0 L) (cant.label as L)))
   (list "mov rax, rsi\nsub rax, rdi\ncmovno rsi, rax\n{disp32} jno L\nL:"
         (lambda (as L) (cant.sub/bnv as $r.result $r.second $r.result L) (cant.label as L)))
   (list "mov r8, rsi\nadd r8, rdi\n{disp32} jo L\nL:"
         (lambda (as L) (cant.add/bov as $r.result $r.second 0 L) (cant.label as L)))
   (list "mov rax, rsi\nadd rax, rdi\ncmovno rdi, rax\n{disp32} jo L\nL:"
         (lambda (as L) (cant.add/bov as $r.result $r.second $r.second L) (cant.label as L)))
   (list "mov r8, rsi\nsub r8, rdi\n{disp32} jo L\nL:"
         (lambda (as L) (cant.sub/bov as $r.result $r.second 0 L) (cant.label as L)))
   (list "mov r8, rsi\nadd r8, 4\n{disp32} jo L\nL:"
         (lambda (as L) (cant.addi/bov as $r.result 4 0 L) (cant.label as L)))
   (list "mov rax, rsi\nsub rax, 4\ncmovno rsi, rax\n{disp32} jo L\nL:"
         (lambda (as L) (cant.subi/bov as $r.result 4 $r.result L) (cant.label as L)))
   (list "mov rdi, rsi\nshl rdi, 2"
         (lambda (as L) (cant.shli as $r.result 2 $r.second)))
   (list "shl rsi, 1"
         (lambda (as L) (cant.shli as $r.result 1 $r.result)))
   (list "mov rdi, rsi\nshr rdi, 8"
         (lambda (as L) (cant.shri as $r.result 8 $r.second)))
   (list "mov r13, rsi\nsar r13, 63"
         (lambda (as L) (cant.shrai as $r.result 63 5)))
   (list "mov rdi, rsi\nand rdi, -4"
         (lambda (as L) (cant.andni as $r.result 3 $r.second)))
   (list "movabs rax, 0xfffffff00000000\nand rsi, rax"
         (lambda (as L) (cant.andni as $r.result (- -1 #xfffffff00000000) $r.result)))
   (list "lea rdi, [rsi+r8*1]"
         (lambda (as L) (cant.add as $r.result 0 $r.second)))
   (list "add rsi, r8"
         (lambda (as L) (cant.add as $r.result 0 $r.result)))
   (list "add r8, rsi"
         (lambda (as L) (cant.add as $r.result 0 0)))
   (list "lea rdi, [rsi+r13*2]"
         (lambda (as L) (cant.addshli as $r.result 5 1 $r.second)))
   (list "lea rdi, [r13+rsi*8]"
         (lambda (as L) (cant.addshli as 5 $r.result 3 $r.second)))
   (list "sub rsi, rdi"
         (lambda (as L) (cant.sub as $r.result $r.second $r.result)))
   (list "mov rax, rsi\nsub rax, rdi\nmov rdi, rax"
         (lambda (as L) (cant.sub as $r.result $r.second $r.second)))
   (list "mov r8, rsi\nsub r8, rdi"
         (lambda (as L) (cant.sub as $r.result $r.second 0)))
   (list "and rsi, rdi"
         (lambda (as L) (cant.and as $r.result $r.second $r.result)))
   (list "or rdi, rsi"
         (lambda (as L) (cant.or as $r.result $r.second $r.second)))
   (list "mov r9, rsi\nxor r9, rdi"
         (lambda (as L) (cant.xor as $r.result $r.second 1)))
   (list "mov r8, rsi\nimul r8, rdi"
         (lambda (as L) (cant.mul as $r.result $r.second 0)))
   (list "imul rdi, rsi"
         (lambda (as L) (cant.mul as $r.result $r.second $r.second)))
   (list "mov rax, rsi\nimul rax, rcx\ncmovno rsi, rax\n{disp32} jno L\nL:"
         (lambda (as L) (cant.mul/bnv as $r.result $r.tmp $r.result L) (cant.label as L)))
   (list "mov rax, rsi\nimul rax, rcx\ncmovno rdi, rax\n{disp32} jo L\nL:"
         (lambda (as L) (cant.mul/bov as $r.result $r.tmp $r.second L) (cant.label as L)))
   (list (string-append "test r8, r8\n{disp32} je L\nmov rax, rsi\ncqo\nidiv r8\n"
                        "lea rdx, [rax*4+0]\nsar rdx, 2\ncmp rdx, rax\n{disp32} jne L\n"
                        "lea rdi, [rax*4+0]\nL:")
         (lambda (as L) (cant.fxquotient/bov as $r.result 0 $r.second L) (cant.label as L)))
   (list "test rdi, rdi\n{disp32} je L\nmov rax, rsi\ncqo\nidiv rdi\nmov r9, rdx\nL:"
         (lambda (as L) (cant.fxremainder/bov as $r.result $r.second 1 L) (cant.label as L)))
   (list "mov rax, rsi\nmov rdx, rdi\nxchg rcx, rdx\nshl rax, cl\nxchg rcx, rdx\nmov r8, rax"
         (lambda (as L) (cant.shl as $r.result $r.second 0)))
   (list "mov rax, rcx\nmov rdx, rsi\nxchg rcx, rdx\nshr rax, cl\nxchg rcx, rdx\nmov rcx, rax"
         (lambda (as L) (cant.shr as $r.tmp $r.result $r.tmp)))
   (list "mov rax, rsi\nmov rdx, rcx\nxchg rcx, rdx\nsar rax, cl\nxchg rcx, rdx\nmov rsi, rax"
         (lambda (as L) (cant.shra as $r.result $r.tmp $r.result)))
   (list "mov rdi, rsi\nnot rdi"
         (lambda (as L) (cant.not as $r.result $r.second)))

   ;; Floating point.
   (list "movsd xmm1, QWORD PTR [rsi+3]"
         (lambda (as L) (cant.fldi as $r.result 3 1)))
   (list "movsd QWORD PTR [r12+3], xmm3"
         (lambda (as L) (cant.fsti as 3 4 3)))
   (list "addsd xmm0, xmm1"
         (lambda (as L) (cant.fadd as 0 1 0)))
   (list "mulsd xmm1, xmm0"
         (lambda (as L) (cant.fmul as 0 1 1)))
   (list "movapd xmm15, xmm0\nsubsd xmm15, xmm1\nmovapd xmm1, xmm15"
         (lambda (as L) (cant.fsub as 0 1 1)))
   (list "movapd xmm2, xmm0\ndivsd xmm2, xmm1"
         (lambda (as L) (cant.fdiv as 0 1 2)))
   (list "mov rax, rsi\nsar rax, 2\ncvtsi2sd xmm2, rax"
         (lambda (as L) (cant.fixnum->flonum as $r.result 2)))
   (list "ucomisd xmm0, xmm1\nsete al\nsetnp dl\nand al, dl\nmovzx eax, al\nlea rdi, [rax*4+2]"
         (lambda (as L) (cant.fsetcc as 'equal 0 1 $r.second)))
   (list "ucomisd xmm0, xmm1\nsetne al\nsetp dl\nor al, dl\nmovzx eax, al\nlea rdi, [rax*4+2]"
         (lambda (as L) (cant.fsetcc as 'not-equal 0 1 $r.second)))
   (list "ucomisd xmm1, xmm0\nseta al\nmovzx eax, al\nlea rdi, [rax*4+2]"
         (lambda (as L) (cant.fsetcc as 'less 0 1 $r.second)))
   (list "ucomisd xmm1, xmm0\nsetae al\nmovzx eax, al\nlea r15, [rax*4+2]"
         (lambda (as L) (cant.fsetcc as 'less-or-equal 0 1 7)))
   (list "ucomisd xmm2, xmm3\nseta al\nmovzx eax, al\nlea rdi, [rax*4+2]"
         (lambda (as L) (cant.fsetcc as 'greater 2 3 $r.second)))
   (list "ucomisd xmm2, xmm3\nsetae al\nmovzx eax, al\nlea rdi, [rax*4+2]"
         (lambda (as L) (cant.fsetcc as 'greater-or-equal 2 3 $r.second)))
   ))

(define (hex2 n)
  (let ((s (number->string n 16)))
    (if (< n 16) (string-append "0" s) s)))

(define (write-test-files)
  (call-with-output-file "encoder-test-expected.s"
    (lambda (exp)
      (call-with-output-file "encoder-test-actual.s"
        (lambda (act)
          (display ".intel_syntax noprefix\n" exp)
          (let loop ((cases test-cases) (i 0))
            (if (pair? cases)
                (let* ((text (car (car cases)))
                       (proc (cadr (car cases)))
                       (as   (make-test-as))
                       (label (L)))
                  (proc as label)
                  (display (string-append ".section .text.case" (number->string i)
                                          ",\"ax\"\n")
                           exp)
                  (display (string-append ".section .text.case" (number->string i)
                                          ",\"ax\"\n")
                           act)
                  ;; Labels are local to the case.
                  (display (substitute-label text (string-append ".Lcase" (number->string i)))
                           exp)
                  (newline exp)
                  (let ((cv (test-as-code as)))
                    (do ((j 0 (+ j 1)))
                        ((= j (bytevector-length cv)))
                      (display ".byte 0x" act)
                      (display (hex2 (bytevector-ref cv j)) act)
                      (newline act)))
                  (loop (cdr cases) (+ i 1)))))))))
  (display (length test-cases))
  (display " cases written")
  (newline))

; Replace the label name L (a whole word) in text by name.

(define (substitute-label text name)
  (let ((n (string-length text)))
    (define (word-char? c)
      (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\.)))
    (let loop ((i 0) (acc '()))
      (cond ((= i n)
             (apply string-append (reverse acc)))
            ((and (char=? (string-ref text i) #\L)
                  (or (= i 0) (not (word-char? (string-ref text (- i 1)))))
                  (or (= (+ i 1) n) (not (word-char? (string-ref text (+ i 1))))))
             (loop (+ i 1) (cons name acc)))
            (else
             (loop (+ i 1) (cons (string (string-ref text i)) acc)))))))

(write-test-files)
