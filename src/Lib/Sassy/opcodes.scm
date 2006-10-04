; opcodes.scm - Sassy's opcode parsers and code generators
; Copyright (C) 2005 Jonathan Kraut

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Contact:
; Jonathan Kraut
; 4130 43 ST #C2
; Sunnyside, NY 11104
; jak76@columbia.edu

; see file COPYING in the top of Sassy's distribution directory


; module opcodes
; import operands numbers api text-block push-stacks
; export opcode? emit-direct2

 

;=============================================================
; Interfaces clients use

(define sassy-opcode-table (make-hash-table))

(define (opcode? x)
  (hash-table-ref sassy-opcode-table x (lambda () #f)))

(define (emit-direct2 name opcode args textb)
  (if (opcode textb args)
      (begin (t-addr-flag-set! textb #f)
	     (t-seg-flag-set! textb #f)
	     (push-stack-size (t-text textb)))
      (error "sassy: bad operands" (cons name args))))

(define-syntax def-sassy-op
  (syntax-rules ()
    ((_ name func)
     (hash-table-set! sassy-opcode-table 'name func))))


;=============================================================
; New parser generator (replaces old meta-lambda)

(define-syntax mini-meta
  (syntax-rules (or and lambda)
    ((_ (or x ...))
     (lambda (tt forms)
       (gen-parse forms (tt) (and (or x ...)))))
    
    ((_ (and x ...))
     (lambda (tt forms)
       (gen-parse forms (tt) (and x ...))))

    ((_ (lambda (t) x ...))
     (lambda (tt forms)
       (gen-parse forms (tt) (and (lambda (t) x ...)))))))
    
(define-syntax gen-parse
  (syntax-rules (and lambda or and begin quote ?* ?)
    
    ((_ forms (tt) (and (lambda (t1) bdy ...)))
     (and (null? forms)
	  (let ((t1 tt))
	    (begin bdy ...))))
    
    ((_ forms (m1 tt) (and (lambda (t1 a1) bdy ...)))
     (and (null? forms)
	  (let ((a1 m1)
		(t1 tt))
	    (begin bdy ...))))
    
    ((_ forms (m2 m1 tt) (and (lambda (t1 a1 a2) bdy ...)))
     (and (null? forms)
	  (let ((a1 m1) (a2 m2) (t1 tt))
	    (begin bdy ...))))

    ((_ forms (m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3) bdy ...)))
     (and (null? forms)
	  (let ((a1 m1) (a2 m2) (a3 m3) (t1 tt))
	    (begin bdy ...))))

    ((_ forms matches (and (begin bdy ...)))
     (and (null? forms) (begin bdy ...)))

    ((_ forms matches (and (quote blah) rst ...))
     (if (and (not (null? forms)) (equal? 'blah (car forms)))
	 (let ((next (cdr forms)))
	   (gen-parse next matches (and rst ...)))
	 #f))

    ((_ forms matches (and (?* (quote blah)) rst ...))
     (cond ((null? forms) (gen-parse forms matches (and rst ...)))
	   ((equal? (car forms) 'blah)
	    (let ((next (cdr forms)))
	      (gen-parse next matches (and rst ...))))
	   (else (gen-parse forms matches (and rst ...)))))

    ((_ forms matches (and (or c1 ...) . rst))
       (or (gen-parse forms matches (and c1 . rst))
	   ...))

    ((_ forms matches (and (and y ...)))
     (gen-parse forms matches (and y ...)))

    ((_ forms (x ...) (and ? rst ...))
     (let ((t (car forms)))
       (let ((next (cdr forms)))
	 (gen-parse next (t x ...) (and rst ...)))))

    ((_ forms (x ...) (and pred rst ...))
     (let ((t (and (not (null? forms))
		   (let ((g (pred (car forms))))
		     (and g (if (eq? g #t)
				(car forms)
				g))))))
       (if t
	   (let ((next (cdr forms)))
	     (gen-parse next (t x ...) (and rst ...)))
	   #f)))))




;==================================================================
; The following are short enough or rare enough that inlining seem
; approprate


; push a byte
(define-syntax outc
  (syntax-rules ()
    ((_ t itm) (begin (push-stack-push (t-text t) itm) #t))))


; push a list of bytes, but copy it first
(define-syntax outc-mq
  (syntax-rules ()
    ((_ t itm)
     (push-stack-push (t-text t) (if (pair? itm) (list-copy itm) itm)))))

; add appropriate address- and operand-size prefixes
(define-syntax outc-cs
  (syntax-rules ()
    ((_ t sizer)
     (let ((r (t-outp t))
	   (addr-flag (t-addr-flag t))
	   (seg-flag (t-seg-flag t)))
       (when addr-flag (outc t #x67))
       (when (and (not (= sizer 1))
		  (not (= sizer (/ (sassy-bits (t-outp t)) 8))))
	     (outc t #x66))
       (when seg-flag (outc t seg-flag))))))


; a common opcode template (eg: (add edx ecx) )
(define-syntax handle-r/r
  (syntax-rules ()
    ((_ t eff-add reg)
     (outc t (+ 192 eff-add (* 8 reg))))))



;===================================================================
; The "handle-FOO" procedures are the basic handlers that that
; everything eventually reaches to actually do the emitting



(define (text-symbol-setup t type target value)

  (define (setup-k x)
    (if (pair? x)
	(if (opcode? (car x))
	    (error "sassy: bad context for instruction as continuation" x)
	    (values #t (cadr x) (caddr x) (cadddr x)))
	(values #t type x value)))
  
  (case target
    (($win)  (setup-k (t-win t)))
    (($lose) (setup-k (t-lose t)))
    (($eip)  (values #t type (push-stack-size (t-text t)) value))
    (else (values #f type target value))))

  
; Handle a text symbol that will be an *absolute* relocation  
(define (handle-text-symbol t sizer type target value)
  (call-with-values
      (lambda ()
	(text-symbol-setup t type target value))
    (lambda (k? type target value)
      (let* ((t-text-t (t-text t))
	     (r (t-outp t))
	     (pnt   (push-stack-push->patcher
		     t-text-t
		     (integer->byte-list value sizer)))
	     (offs  (push-stack-size t-text-t))
	     (t-val (cond ((sassy-symbol-exists-env? r target)
			   => sassy-symbol-offset)
			  (else target)))
	     (a-reloc (make-sassy-reloc
		       (get-reloc-target target r)
		       'text offs type #f value sizer
		       (get-reloc-target-sect target r 'text)))
	     (patcher (lambda (new)
			(pnt (integer->byte-list new sizer))
			(sassy-reloc-value-set! a-reloc new))))
	(sassy-reloc-patcher-set! a-reloc patcher)
	(push-t-reloc! t a-reloc)
	(if (not k?)
	    (if (number? t-val)
		(patcher (+ t-val value))
		(sassy-symbol-set-unres!
		 r target (lambda (n sect)
			    (sassy-reloc-target-section-set!
			     a-reloc sect)
			    (patcher (+ n value)))))
	    (push-t-res!
	     t (cons t-val (lambda (n) (patcher (+ n value))))))))))


; Handle a text symbol that will be a relative relocation  
(define (handle-rel-symbol t sizer type target value)
  (call-with-values
      (lambda () (text-symbol-setup t type target value))
    (lambda (k? type target value)
      (let* ((t-text-t (t-text t))
	     (r (t-outp t))
	     (offs  (push-stack-size t-text-t))
	     (pnt   (push-stack-push->patcher
		     t-text-t (integer->byte-list value sizer)))
	     (t-val (cond ((sassy-symbol-exists-env? r target)
			   => sassy-symbol-offset)
			  (else target)))
	     (a-reloc (make-sassy-reloc
		       (get-reloc-target target r)
		       'text (+ offs sizer) type #f value sizer
		       (get-reloc-target-sect target r 'text)))
	     (patcher (lambda (new)
			(if (not ((case sizer
				    ((1) s-byte)
				    ((2) s-word)
				    ((4) s-dword))
				  new))
			    (error "sassy: out of range" (+ new sizer 1))
			    (begin (pnt (integer->byte-list new sizer))
				   (sassy-reloc-value-set! a-reloc new))))))
	(when (= 4 sizer)
	      (sassy-reloc-patcher-set! a-reloc patcher)
	      (push-t-reloc! t a-reloc))
	(when (and (= 2 sizer) (= 16 (sassy-bits r)))
	      (sassy-reloc-patcher-set! a-reloc patcher)
	      (push-t-reloc! t a-reloc))
	(if k?
	    (patcher (- offs t-val))
	    (if (and (number? t-val) (eqv? 'rel type))
		(push-t-res!
		 t (cons offs (lambda (n) (patcher (- t-val n)))))
		(push-t-unres!
		 t (list target offs (lambda (from)
				       (lambda (to sect)
					 (sassy-reloc-target-section-set!
					  a-reloc sect)
					 (patcher (- to from))))
			 (cond ((sassy-symbol-exists-env? r target) =>
				sassy-symbol-scope)
			       (else #f))))))))))


; "imm" means "immediate"
(define (handle-imm t sizer imm-value)
  (cond ((number? imm-value)
	 (outc t (number->byte-list imm-value sizer)))
	((symbol? imm-value)
	 (handle-text-symbol t sizer 'abs imm-value 0)); sizer))
	((eq? (cadr imm-value) 'rel)
	 (handle-rel-symbol
		t (/ (sassy-bits (t-outp t)) 8)
		(cadr imm-value)
		(caddr imm-value)
		(cadddr imm-value)))
	(else (handle-text-symbol
		     t
		     (/ (sassy-bits (t-outp t)) 8)
		     (cadr imm-value)
		     (caddr imm-value)
		     (cadddr imm-value)))))



; Handling memory references - This is in three parts
; handle-mem does the parsing
; fixup-mem  does the analysis
; finish-mem does the outputting
(define (handle-mem t ref reg)


  (define (skale x)
    (and (integer? x)
	 (cond ((= x 1) 0)
	       ((= x 2) 64)
	       ((= x 4) 128)
	       ((= x 8) 192)
	       (else #f))))

  
  (let iter ((rst (or (and (case (car ref)
			     ((cs) (t-seg-flag-set! t #x2e))
			     ((ss) (t-seg-flag-set! t #x36))
			     ((ds) (t-seg-flag-set! t #x3e))
			     ((es) (t-seg-flag-set! t #x26))
			     ((fs) (t-seg-flag-set! t #x64))
			     ((gs) (t-seg-flag-set! t #x65))
			     (else #f))
			   (cadr ref))
		      ref))
	     (base #f) (ix #f) (sk 0) (mod 0) (r/m #f) (symb #f)
	     (disp #f) (type 'abs))
    (if (null? rst)
	(fixup-mem t ref reg base ix sk  mod r/m symb disp type)
	(let ((next (car rst)))
	  (cond
	   ((r32 next) =>
	    (lambda (x)
	      (cond
	       ((not r/m)
		(iter (cdr rst) base ix sk mod x symb disp type))
	       ((not ix)
		(if (= x 4)
		    (iter (cdr rst) base (* 8 r/m) sk mod 4 symb disp type)
		    (iter (cdr rst) base (* 8 x) sk mod r/m symb disp type)))
	       (else (bad-mem ref)))))

	   ((integer? next)
	    (if disp
		(iter (cdr rst) base ix sk mod r/m symb (+ next disp) type)
		(iter (cdr rst) base ix sk mod r/m symb next type)))

	   ((symbol next) =>
	    (lambda (x)
	      (cond
	       (symb (bad-mem ref))
	       ((symbol? x)
		(iter (cdr rst) base ix sk mod r/m x disp type))
	       (disp
		(iter (cdr rst) base ix sk mod r/m (caddr x)
		      (+ disp (cadddr x)) (cadr x)))
	       (else
		(iter (cdr rst) base ix sk mod r/m (caddr x) (cadddr x)
		      (cadr x))))))

	   ((and (pair? next) (eq? '* (car next)) (null? (cdddr next)))
	    (let ((sk2 (skale (cadr next))))
	      (if sk2
		  (let ((y (r32-not-esp (caddr next))))
		    (if y
			(iter (cdr rst) base (* 8 y) sk2 mod r/m symb
			      disp type)
			(bad-mem ref)))
		  (let ((sk2 (skale (caddr next))))
		    (if sk2
			(let ((y (r32-not-esp (cadr next))))
			  (if y
			      (iter (cdr rst) base (* 8 y) sk2 mod r/m
				    symb disp type)
			      (bad-mem ref)))
			(bad-mem ref))))))
	   (else (bad-mem ref)))))))



(define (fixup-mem t ref reg base ix sk  mod r/m symb disp type)

  (define (rebuild-disp x)
    (if x `(dword ,x) '(dword 0)))

  (define (fix-disp ref disp symb) ;=>disp
    (if (or disp symb)
	(cond ((and (not symb) (s-byte disp)) =>
	       (lambda (x) `(byte ,x)))
	      (else
	       (let ((disp (u/s-dword disp)))
		 (if (or symb disp)
		     (rebuild-disp disp)
		     (bad-mem ref)))))
	disp))

  (define (comp-mod ref disp symb mod) ;=> mod
    (if (or disp symb)
	(cond ((and (s-byte disp) (not symb)) 64)
	      ((or (u/s-dword disp) symb)    128)
	      (else (bad-mem ref)))
	mod))
  
  (cond
   ((and (not r/m) (not ix))
    (if (and (not symb) (not (u/s-dword disp)))
	(bad-mem ref)
	(let ((disp (rebuild-disp disp)))
	  (finish-mem t ref reg base ix sk mod 5 symb disp type))))

   (r/m
    (if (and (= r/m 5) (not disp) (not symb))
	(if ix
	    (finish-mem t ref reg r/m ix sk 64 4 symb '(byte 0) type)
	    (finish-mem t ref reg r/m ix sk 64 r/m symb '(byte 0) type))
	(let ((mod  (comp-mod ref disp symb mod))
	      (disp (fix-disp ref disp symb)))
	  (cond
	   ((and (= r/m 4) (not ix))
	    (finish-mem t ref reg r/m 32 sk mod r/m symb disp type))

	   (ix (finish-mem t ref reg r/m ix sk mod 4 symb disp type))

	   (else (finish-mem t ref reg base ix sk mod r/m symb disp type))))))

   ((or (= sk 128) (= sk 192))
    (if (or symb (and disp (u/s-dword disp)))
	(let ((disp (rebuild-disp disp)))
	  (finish-mem t ref reg 5 ix sk mod 4 symb disp type))
	(finish-mem t ref reg 5 ix sk mod 4 symb '(dword 0) type)))

   (else
    (let* ((disp (if (and (not disp) (not symb) (= ix 40))
		     0
		     disp))
	   (mod (comp-mod ref disp symb mod))
	   (disp (fix-disp ref disp symb)))
      (if (zero? sk)
	  (finish-mem t ref reg base #f sk mod (/ ix 8) symb disp type)
	  (finish-mem t ref reg (/ ix 8) ix 0 mod 4 symb disp type))))))


(define (finish-mem t ref reg base ix sk mod r/m symb disp type)
  (when symb
	(handle-text-symbol t 4 type symb (or (and disp (cadr disp)) 0)))
  (when (and disp (not symb))
	(outc t (integer->byte-list (cadr disp) (case (car disp)
						 ((byte) 1)
						 ((dword) 4)))))
  (when (= 16 (sassy-bits (t-outp t))) (t-addr-flag-set! t #t))
  (when ix (outc t (+ sk ix base)))
  (outc t (+ mod r/m (* 8 reg))))


; helpers for handle-mem and friends
(define (bad-mem ref)
  (error "sassy: bad memory-ref" (cons '& ref)))




;================================================================
; Predicates that deal with the string/brnt prefixes

(define (rep-able x)
  (and (pair? x)
       (member (car x) '(insb insw insd outsb outsw outsd movsb
			      movsw movsd lodsb lodsw lodsd stosb
			      stosw stosd
			      ; same prefix, so allow these here -JK
			      cmpsb cmpsw cmpsd scasb scasw scasd))
       (null? (cdr x)) ;check for null due to ambiguity of movsd
       x))

(define (rep-e-able x)
  (and (pair? x)
       (member (car x) '(cmpsb cmpsw cmpsd scasb scasw scasd))
       (null? (cdr x)) ;check for null due to ambiguity of cmpsd
       x))

(define (lock-able x)
  (and (pair? x)
       (member (car x) '(add adc and btc btr bts cmpxchg cmpxchg8b
			     dec inc neg not or sbb sub xor xadd
			     xchg))
       (mem-any (cadr x))
       x))

(define (branch-predict-able x)
  (and (pair? x)
       (member (car x) '(jo jno jb jc jnae jnb jnc jae je jz jne
			    jnz jbe jna ja jnbe js jns jp jpe jnp
			    jpo jl jnge jge jnl jle jng jnle jg))
       x))



;====================================================================
;The opcode *OUTPUT* templates.
;
; All the opcode parsers call these with t (the <text-block> data
; structure) and various parameterizations particular to the opcode
; and its args, and the args needed.  Those that are farther down tend
; to call the ones higher up.

; Converting these to macros could gain about 5% in speed.
;
; These are called directly by the opcode parsers with appropriate
; parameterizations.

(let ()
  
(define (just-c t sizer opcode)
  (outc-mq t opcode)
  (outc-cs t sizer))

(define (just-i t sizer opcode imm-value . prefix?)
  (if (null? prefix?)
      (begin (handle-imm t sizer imm-value)
	     (just-c t sizer opcode))
      (begin (handle-imm t sizer imm-value)
	     (outc-mq t opcode))))

(define (just-i8 t sizer opcode imm-value)
  (handle-imm t 1 imm-value)
  (just-c t sizer opcode))

(define (just-i32 t sizer opcode imm-value) ; only used by mov mi
  (when (= 16 (sassy-bits (t-outp t))) (t-addr-flag-set! t #t))
  (handle-imm t 4 imm-value)
  (just-c t sizer opcode))

(define (just-r2 t sizer partial-code-a partial-code-b reg)
  (outc t (+ partial-code-b reg))
  (outc t partial-code-a)
  (outc-cs t sizer))

(define (just-r t sizer partial-code reg)
  (outc t (+ partial-code reg))
  (outc-cs t sizer))

(define (just-r-i t sizer partial-code reg imm-value)
  (handle-imm t sizer imm-value)
  (just-r t sizer partial-code reg))

(define (just-i-rel t sizer opcode rel-value)
  (cond ((or (symbol? rel-value) (number? rel-value))
	 (handle-rel-symbol t sizer 'rel rel-value 0))
	(else (handle-rel-symbol t sizer
				 (cadr rel-value)
				 (caddr rel-value)
				 (cadddr rel-value))))
  (just-c t sizer opcode))

(define (just-m t sizer opcode dest reg-field)
  (handle-mem t dest reg-field)
  (just-c t sizer opcode))

(define (r/m t sizer opcode dest reg-field)
  (if (number? dest)
      (handle-r/r t dest reg-field)
      (handle-mem t dest reg-field))
  (just-c t sizer opcode))

(define (r/m-i t sizer opcode dest reg-field imm-value)
  (handle-imm t sizer imm-value)
  (r/m t sizer opcode dest reg-field))

(define (r/m-i8 t sizer opcode dest reg-field imm-value)
  (handle-imm t 1 imm-value)
  (r/m t sizer opcode dest reg-field))

(define (r/m-r t sizer opcode dest src)
  (if (integer? dest)
      (handle-r/r t dest src)
      (handle-mem t dest src))
  (just-c t sizer opcode))

(define (r-r/m t sizer opcode dest src)
  (r/m-r t sizer opcode src dest))

(define (r/m-r-i t sizer opcode dest src imm)
  (handle-imm t sizer imm)
  (r/m-r t sizer opcode dest src))

(define (r-r/m-i t sizer opcode dest src imm)
  (r/m-r-i t sizer opcode src dest imm))

(define (r/m-r-i8 t sizer opcode dest src imm)
  (handle-imm t 1 imm)
  (r/m-r t sizer opcode dest src))

(define (r-r/m-i8 t sizer opcode dest src imm)
  (r/m-r-i8 t sizer opcode src dest imm))

(define (i16-i8 t sizer opcode imm1 imm2)
  (handle-imm t 1 imm2)
  (handle-imm t 2 imm1)
  (just-c t sizer opcode))

(define (i16-i16 t sizer opcode imm1 imm2)
  (handle-imm t 2 imm2)
  (handle-imm t 2 imm1)
  (just-c t sizer opcode))

(define (i16-i32 t sizer opcode imm1 imm2)
  (handle-imm t 2 imm2)
  (handle-imm t 4 imm1)
  (just-c t sizer opcode))

(define (lof-imms t sizer itms)
  (for-each
   (lambda (itm)
     (cond ((string? itm)
	    (let ((end-fill (- sizer (modulo (string-length itm) sizer))))
	      (push-stack-push (t-text t) (make-list end-fill 0))
	      (push-stack-push (t-text t) (map char->integer
					       (string->list itm)))))
	   (else (handle-imm t sizer itm))))
   (reverse itms)))



;====================================================================
;The opcode *GENERATOR* templates.
;
; There are lots of common patterns, but also some that don't really
; fit any (easily discernible) pattern.



;*********************
;** NOTE: NEW SCOPE **
;*********************
(let ()

  
(define (gen-non sizer opcode)
  (mini-meta (lambda (t) (just-c t sizer opcode))))


(define gen-alu
  (lambda (rm-op-b rm-op-dw rm/r-op-b rm/r-op-dw i-op-b i-op-dw reg-field)
    (mini-meta
     (or
      (and 'eax (or (and si8 (lambda (t y) (r/m-i8 t 4 131 0 reg-field y)))
		    (and i32 (lambda (t y) (just-i t 4 i-op-dw y)))))
      (and r32 (or (and r32 (lambda (t x y) (r/m-r t 4 rm-op-dw x y)))
	       (and si8 (lambda (t x y) (r/m-i8 t 4 131 x reg-field y)))
	       (and i32 (lambda (t x y) (r/m-i t 4 129 x reg-field y)))
	       (and m32 (lambda (t x y) (r-r/m t 4 rm/r-op-dw x y)))))
      (and 'al  i8 (lambda (t y) (just-i t 1 i-op-b  y)))
      (and r8 (or (and i8 (lambda (t x y) (r/m-i t 1 128 x reg-field y)))
		  (and r8 (lambda (t x y) (r/m-r t 1 rm-op-b x y)))
		  (and m8 (lambda (t x y) (r-r/m t 1 rm/r-op-b x y)))))
      (and 'ax  i16 (lambda (t y) (just-i t 2 i-op-dw y)))
      (and r16 (or (and r16 (lambda (t x y) (r/m-r t 2 rm-op-dw x y)))
		   (and si8 (lambda (t x y) (r/m-i8 t 2 131 x reg-field y)))
		   (and i16 (lambda (t x y) (r/m-i t 2 129 x reg-field y)))
		   (and m16 (lambda (t x y) (r-r/m t 2 rm/r-op-dw x y)))))
      (and m32 r32 (lambda (t x y) (r/m-r t 4 rm-op-dw x y)))
      (and m16 r16 (lambda (t x y) (r/m-r t 2 rm-op-dw x y)))
      (and m8  r8  (lambda (t x y) (r/m-r t 1 rm-op-b x y)))
      (and em8 i8 (lambda (t x y) (r/m-i t  1 #x80 x reg-field y)))
      (and em32
	   (or (and si8  (lambda (t x y) (r/m-i8 t 4 #x83 x reg-field y)))
	       (and i32 (lambda (t x y) (r/m-i t  4 #x81 x reg-field y)))))
      (and em16 (or (and si8 (lambda (t x y) (r/m-i8 t 2 #x83 x reg-field y)))
		    (and i16 (lambda (t x y)
			       (r/m-i t  2 #x81 x reg-field y)))))
      (and um32 (or (and si8 (lambda (t x y)
			       (if (= 16 (sassy-bits (t-outp t)))
				   (r/m-i8 t 2 #x83 x reg-field y)
				   (r/m-i8 t 4 #x83 x reg-field y))))
	       (and ei32 (lambda (t x y) (r/m-i t  4 #x81 x reg-field y)))
	       (and ei16 (lambda (t x y) (r/m-i t  2 #x81 x reg-field y)))
	       (and ui32 (lambda (t x y)
			   (if (and (ui16 y) (= 16 (sassy-bits (t-outp t))))
			       (r/m-i t  2 #x81 x reg-field y)
			       (r/m-i t  4 #x81 x reg-field y))))))))))


(define (gen-bt opcode1 opcode2 reg-field)
  (mini-meta
   (or (and (or r32 em32)
	    (or (and r32 (lambda (t x y) (r/m-r t 4 opcode1 x y)))
		(and i8  (lambda (t x y) (r/m-i8 t 4 opcode2 x reg-field y)))))
       (and (or r16 em16)
	    (or (and r16 (lambda (t x y) (r/m-r t 2 opcode1 x y)))
		(and i8 (lambda (t x y) (r/m-i8 t 2 opcode2 x reg-field y)))))
       (and um16 r16 (lambda (t x y) (r/m-r t 2 opcode1 x y)))
       (and um32 r32 (lambda (t x y) (r/m-r t 4 opcode1 x y)))
       (and um32 i8 (lambda (t x y)
		      (if (= 16 (sassy-bits (t-outp t)))
			  (r/m-i8 t 2 opcode2 x reg-field y)
			  (r/m-i8 t 4 opcode2 x reg-field y)))))))


(define (gen-shift reg-field)
  (mini-meta
   (or (and (or r32 em32)
	    (or (and '1  (lambda (t x) (r/m t 4 #xd1 x reg-field)))
		(and 'cl (lambda (t x) (r/m t 4 #xd3 x reg-field)))
		(and i8  (lambda (t x y) (r/m-i8 t 4 #xc1 x reg-field y)))))
    (and (or r8 em8)
	 (or (and '1  (lambda (t x) (r/m t 1 #xd0 x reg-field)))
	     (and 'cl (lambda (t x) (r/m t 1 #xd2 x reg-field)))
	     (and i8  (lambda (t x y) (r/m-i t  1 #xc0 x reg-field y)))))
    (and (or r16 em16)
	 (or (and '1  (lambda (t x) (r/m t 2 #xd1 x reg-field)))
	     (and 'cl (lambda (t x) (r/m t 2 #xd3 x reg-field)))
	     (and  i8 (lambda (t x y) (r/m-i8 t 2 #xc1 x reg-field y)))))
    (and um32 ? (lambda (t x y)
		  (let ((sizer (/ (sassy-bits (t-outp t)) 8)))
		    (cond ((eq? 1 y) (r/m t sizer #xd1 x reg-field))
			  ((eq? 'cl y) (r/m t sizer #xd3 x reg-field))
			  ((i8 y) => (lambda (y)
				       (r/m-i8 t sizer #xc1 x reg-field y)))
			  (else #f))))))))

(define (gen-jcc cc-code)
  (let* ((cc2 (+ #x70 cc-code))
	 (cc4 (+ #x80 cc-code))
	 (op4 (list #x0f cc4)))
    (mini-meta
     (or (and 'short (or rel32 rel16 rel8)
	      (lambda (t x) (just-i-rel t 1 cc2 x)))
	 (and (?* 'near)
	      (or (and erel32 (lambda (t x) (just-i-rel t 4 op4 x)))
		  (and erel16 (lambda (t x) (just-i-rel t 2 op4 x)))
		  (and urel32 (lambda (t x)
				(if (and (urel16 x)
					 (= 16 (sassy-bits (t-outp t))))
				    (just-i-rel t 2 op4 x)
				    (just-i-rel t 4 op4 x))))))))))

(define (gen-setcc cc-code)
  (let ((op (list #x0f (+ #x90 cc-code))))
    (mini-meta
     (and (or r8 m8) (lambda (t x) (r/m t 1 op x #b000))))))


(define (gen-cmovcc cc-code)
  (let ((op (list #x0f (+ #x40 cc-code))))
    (mini-meta
     (or (and r32 (or r32 m32) (lambda (t x y) (r-r/m t 4 op x y)))
	 (and r16 (or r16 m16) (lambda (t x y) (r-r/m t 2 op x y)))))))


(define (gen-decinc partial-code reg-field)
  (mini-meta
   (or (and r32 (lambda (t x) (just-r t 4 partial-code x)))
       (and r16 (lambda (t x) (just-r t 2 partial-code x)))
       (and em32 (lambda (t x) (r/m t 4 #xff x reg-field)))
       (and em16 (lambda (t x) (r/m t 2 #xff x reg-field)))
       (and um32 (lambda (t x)
		   (if (= 16 (sassy-bits (t-outp t)))
		       (r/m t 2 #xff x reg-field)
		       (r/m t 4 #xff x reg-field))))
       (and (or r8 em8) (lambda (t x) (r/m t 1 #xfe x reg-field))))))


(define (gen-plier reg-field)
  (mini-meta
   (or (and (or r32 em32) (lambda (t x) (r/m t 4 #xf7 x reg-field)))
       (and (or r16 em16) (lambda (t x) (r/m t 2 #xf7 x reg-field)))
       (and (or r8   em8) (lambda (t x) (r/m t 1 #xf6 x reg-field)))
       (and um32 (lambda (t x)
		   (if (= 16 (sassy-bits (t-outp t)))
		       (r/m t 2 #xf7 x reg-field)
		       (r/m t 4 #xf7 x reg-field)))))))

(define (gen-load opcode)
  (mini-meta
   (or (and r32 m32 (lambda (t x y) (r-r/m t 4 opcode x y)))
       (and r16 m16 (lambda (t x y) (r-r/m t 2 opcode x y))))))


(define (gen-movx opcode1 opcode2)
  (let ((op1 (list #x0f opcode1))
	(op2 (list #x0f opcode2)))
    (mini-meta
     (or (and r32 (or (and (or r8 em8)  (lambda (t x y) (r-r/m t 4 op1 x y)))
		      (and (or r16 m16) (lambda (t x y) (r-r/m t 4 op2 x y)))))
	 (and r16 (or r8 m8) (lambda (t x y) (r-r/m t 2 op1 x y)))))))


(define (gen-r/rm opcodes)
  (mini-meta
   (or (and r32 (or r32 m32) (lambda (t x y) (r-r/m t 4 opcodes x y)))
       (and r16 (or r16 m16) (lambda (t x y) (r-r/m t 2 opcodes x y))))))

(define (gen-rm opc reg-field)
  (let ((op (list #x0f opc)))
    (mini-meta
     (and m32 (lambda (t x) (r/m t 1 op x reg-field))))))

(define (gen-rm8 opc reg-field)
  (let ((op (list #x0f opc)))
    (mini-meta
     (and m8 (lambda (t x) (r/m t 1 op x reg-field))))))


(define (gen-rm2 opcodes reg-field)
  (mini-meta
   (and (or r16 m16) (lambda (t x) (r/m t 1 opcodes x reg-field)))))


(define (gen-aa opcodes-none opcodes-1)
  (mini-meta
   (or (lambda (t) (just-c t 1 opcodes-none))
       (and i8 (lambda (t x) (just-i t 1 opcodes-1 x))))))


(define (gen-ret opcodes-none opcodes-1)
  (mini-meta
   (or (and (lambda (t) (just-c t 1 opcodes-none)))
       (and i16 (lambda (t x) (just-i t 2 opcodes-1 x #f))))))


(define (gen-doub-shift code1 code2)
  (let ((op1 (list #x0f code1))
	(op2 (list #x0f code2)))
    (mini-meta
     (or (and (or r32 m32) r32
	      (or (and i8 (lambda (t x y z) (r/m-r-i8 t 4 op1 x y z)))
		  (and 'cl (lambda (t x y) (r/m-r t 4 op2 x y)))))
	 (and (or r16 m16) r16
	      (or (and i8 (lambda (t x y z) (r/m-r-i8 t 2 op1 x y z)))
		  (and 'cl (lambda (t x y) (r/m-r t 2 op2 x y)))))))))

	
(define (gen-loop opcode)
  (let ((op (list #x67 opcode)))
    (mini-meta
     (and rel8 (or (lambda (t x) (just-i-rel t 1 opcode x))
		   (and 'cx (lambda (t x)
			      (if (= 16 (sassy-bits (t-outp t)))
				  (just-i-rel t 1 opcode x)
				  (just-i-rel t 1 op x))))
		   (and 'ecx (lambda (t x)
			       (if (= 16 (sassy-bits (t-outp t)))
				   (just-i-rel t 1 op x)
				   (just-i-rel t 1 opcode x)))))))))
	
(define (gen-cmpx opcode1 opcode2)
  (mini-meta
   (or (and (or r8 m8)    r8 (lambda (t x y) (r/m-r t 1 opcode1 x y)))
       (and (or r16 m16) r16 (lambda (t x y) (r/m-r t 2 opcode2 x y)))
       (and (or r32 m32) r32 (lambda (t x y) (r/m-r t 4 opcode2 x y))))))


(define (gen-fpmath-1 reg-field to-0-c from-0-c)
  (mini-meta
   (or (and em64    (lambda (t x) (r/m t 1 #xdc x reg-field)))
       (and m32     (lambda (t x) (r/m t 1 #xd8 x reg-field)))
       (and 'st0 st (lambda (t x) (just-r2 t 1 #xd8 to-0-c x)))
       (and st 'st0 (lambda (t x) (just-r2 t 1 #xdc from-0-c x))))))


(define (gen-fpmath-2 with w/o)
  (let ((op (list #xde w/o)))
    (mini-meta
     (or (lambda (t) (just-c t 1 op))
	 (and st 'st0 (lambda (t x) (just-r2 t 1 #xde with x)))))))


(define (gen-fpmath-3 reg-field)
  (mini-meta
   (or (and em16 (lambda (t x) (r/m t 1 #xde x reg-field)))
       (and m32 (lambda (t x) (r/m t 1 #xda x reg-field))))))


(define (gen-fcmovcc p-code-1 p-code-2)
  (mini-meta
   (and 'st0 st (lambda (t x) (just-r2 t 1 p-code-1 p-code-2 x)))))


(define (gen-fp-reg/non op1-a op1-b opcode2)
  (mini-meta
   (or (lambda (t) (just-c t 1 opcode2))
       (and st (lambda (t x) (just-r2 t 1 op1-a op1-b x))))))


(define (gen-fp-3m/st reg-field1 reg-field2 st-opcode-a st-opcode-b)
  (mini-meta
   (or (and em64 (lambda (t x) (r/m t 1 #xdd x reg-field1)))
       (and em80 (lambda (t x) (r/m t 1 #xdb x reg-field2)))
       (and m32 (lambda (t x) (r/m t 1 #xd9 x reg-field1)))
       (and st  (lambda (t x) (just-r2 t 1 st-opcode-a st-opcode-b x))))))


(define (gen-fp-3int reg-field1 reg-field2)
  (mini-meta
   (or (and em64 (lambda (t x) (r/m t 1 '#xdf x reg-field2)))
       (and em16 (lambda (t x) (r/m t 1 '#xdf x reg-field1)))
       (and m32 (lambda (t x) (r/m t 1 '#xdb x reg-field1))))))


(define (gen-fp-2int opcode1 opcode2 reg-field)
  (mini-meta
   (or (and em16 (lambda (t x) (r/m t 1 opcode1 x reg-field)))
       (and m32 (lambda (t x) (r/m t 1 opcode2 x reg-field))))))


(define (gen-fp-com pcode1 pcode2 reg-field)
  (let ((op (list #xd8 pcode2)))
    (mini-meta
     (or (lambda (t) (just-c t 1 op))
	 (and em64 (lambda (t x) (r/m t 1 #xdc x reg-field)))
	 (and m32  (lambda (t x) (r/m t 1 #xd8 x reg-field)))
	 (and st   (lambda (t x) (just-r2 t 1 #xd8 pcode1 x)))))))


(define (gen-mmx-log opcode)
  (let ((op1 (list #x0f opcode))
	(op2 (list #x66 #x0f opcode)))
    (mini-meta
     (or (and mm (or mm m64) (lambda (t x y) (r-r/m t 1 op1 x y)))
	 (and xmm (or xmm m128) (lambda (t x y) (r-r/m t 1 op2 x y)))))))


(define (gen-mmx-unplow opcode)
  (let ((op1 (list #x0f opcode))
	(op2 (list #x66 #x0f opcode)))
    (mini-meta
     (or (and mm  (or mm m32) (lambda (t x y) (r-r/m t 1 op1 x y)))
	 (and xmm (or xmm m128) (lambda (t x y) (r-r/m t 1 op2 x y)))))))


(define (gen-mmx-shr opcode1 opcode2 reg-field)
  (let* ((op1 (list #x0f opcode1))
	 (op2 (list #x0f opcode2))
	 (op3 (cons #x66 op1))
	 (op4 (cons #x66 op2)))
    (mini-meta
     (or (and mm (or (and (or mm  m64) (lambda (t x y) (r-r/m t 1 op1 x y)))
		     (and i8
			  (lambda (t x y) (r/m-i8 t 1 op2 x reg-field y)))))
	 (and xmm (or (and (or xmm m128)
			   (lambda (t x y) (r-r/m t 1 op3 x y)))
		      (and i8
			   (lambda (t x y)
			     (r/m-i8 t 1 op4 x reg-field y)))))))))


(define (gen-sse1-mov opcode1 opcode2)
  (mini-meta
   (or (and xmm (or xmm m128) (lambda (t x y) (r-r/m t 1 opcode1 x y)))
       (and m128 xmm (lambda (t x y) (r/m-r t 1 opcode2 x y))))))


(define (gen-sse-ps/pd opcode)
  (mini-meta
   (and xmm (or xmm m128) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse1-ss opcode)
  (mini-meta
   (and xmm (or xmm m32) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse2-sd opcode)
  (mini-meta
   (and xmm (or xmm m64) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse1-mov2 opcode1 opcode2)
  (mini-meta
   (or (and xmm m64 (lambda (t x y) (r-r/m t 1 opcode1 x y)))
       (and m64 xmm (lambda (t x y) (r/m-r t 1 opcode2 x y))))))


(define (gen-xmm-r/r opcode)
  (mini-meta
   (and xmm xmm (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse-cmp opcode)
  (mini-meta
   (and xmm (or xmm m128) i8 (lambda (t x y z) (r-r/m-i t 1 opcode x y z)))))


(define (gen-sse1-ps2pi opc)
  (let ((op (list #x0f opc)))
    (mini-meta
     (and mm (or xmm m64) (lambda (t x y) (r-r/m t 1 op x y))))))


(define (gen-sse1-ss2si opc)
  (let ((op (list #xf3 #x0f opc)))
    (mini-meta
     (and r32 (or xmm m32) (lambda (t x y) (r-r/m t 1 op x y))))))


(define (gen-sse2-sd2si opcode)
  (mini-meta
   (and r32 (or xmm m64) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse-movmsk opcode)
  (mini-meta
   (and r32 xmm (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse-pd2pi opcode)
  (mini-meta
   (and mm (or xmm m128) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse-pi2pds opcode)
  (mini-meta
   (and xmm (or mm m64) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse-si2sssd opcode)
  (mini-meta
   (and xmm (or r32 m32) (lambda (t x y) (r-r/m t 1 opcode x y)))))


(define (gen-sse2-sr reg-field)
  (mini-meta
   (and xmm i8 (lambda (t x y) (r/m-i t 1 '(#x66 #x0f #x73) x reg-field y)))))


(define (gen-sse-movnt opcode)
  (mini-meta
   (and m128 xmm (lambda (t x y) (r/m-r t 1 opcode x y)))))

(define (gen-prefix pred code)
  (mini-meta
   (and pred (lambda (t x)
	       (emit-direct2 (car x) (opcode? (car x)) (cdr x) t)
	       (outc t code)))))


(define (raw-bytes? lst p)
  (let iter ((lst lst) (res '()))
    (cond ((null? lst) (reverse res))
	  ((string? (car lst)) (iter (cdr lst) (cons (car lst) res)))
	  ((p (car lst)) => (lambda (n) (iter (cdr lst) (cons n res))))
	  (else #f))))


;===================================================================
; Now we actually generate the opcodes - each is a function stored in
; the hashtable "sassy-opcode-table" which accepts a <text-block> and
; the list of operands, and either side affects the <text-block> or
; returns #f. One obtains these procedures by calling (opcode? FOO),
; where foo is hopefully the mnemonic of an instructions name. Either
; the function or #f is returned.


; raw data in the text section
(def-sassy-op bytes  (lambda (t lst)
		      (cond ((raw-bytes? lst i8) =>
			     (lambda (good) (lof-imms t 1 good)))
			    (else #f))))
(def-sassy-op words  (lambda (t lst)
		      (cond ((raw-bytes? lst i16) =>
			     (lambda (good) (lof-imms t 2 good)))
			    (else #f))))
	 
(def-sassy-op dwords  (lambda (t lst)
		       (cond ((raw-bytes? lst i32) =>
			      (lambda (good) (lof-imms t 4 good)))
			     (else #f))))
	 
(def-sassy-op rep   (gen-prefix rep-able #xf3))
(def-sassy-op repe  (gen-prefix rep-e-able #xf3))
(def-sassy-op repz  (gen-prefix rep-e-able #xf3))
(def-sassy-op repne (gen-prefix rep-e-able #xf2))
(def-sassy-op repnz (gen-prefix rep-e-able #xf2))
(def-sassy-op lock  (gen-prefix lock-able  #xf0))
(def-sassy-op brt   (gen-prefix branch-predict-able #x3e))
(def-sassy-op brnt  (gen-prefix branch-predict-able #x2e))
	 
(def-sassy-op aaa      (gen-non 1 #x37))
(def-sassy-op aas      (gen-non 1 #x3f))
(def-sassy-op cbw      (gen-non 2 #x98))
(def-sassy-op cdq      (gen-non 4 #x99))
(def-sassy-op clc      (gen-non 1 #xf8))
(def-sassy-op cld      (gen-non 1 #xfc))
(def-sassy-op cli      (gen-non 1 #xfa))
(def-sassy-op clts     (gen-non 1 '(#x0f #x06)))
(def-sassy-op cmc      (gen-non 1 #xf5))
(def-sassy-op cmpsb    (gen-non 1 #xa6))
(def-sassy-op cmpsw    (gen-non 2 #xa7)) ;cmpsd in sse2
(def-sassy-op cpuid    (gen-non 1 '(#x0f #xa2)))
(def-sassy-op cwde     (gen-non 4 #x98))
(def-sassy-op cwd      (gen-non 2 #x99))
(def-sassy-op daa      (gen-non 1 #x27))
(def-sassy-op das      (gen-non 1 #x2f))
(def-sassy-op hlt      (gen-non 1 #xf4))
(def-sassy-op insb     (gen-non 1 #x6c))
(def-sassy-op insw     (gen-non 2 #x6d))
(def-sassy-op insd     (gen-non 4 #x6d))
(def-sassy-op int3     (gen-non 1 #xcc))
(def-sassy-op into     (gen-non 1 #xce))
(def-sassy-op invd     (gen-non 1 '(#x0f #x08)))
(def-sassy-op iret     (gen-non 1 #xcf))
(def-sassy-op iretw    (gen-non 2 #xcf))
(def-sassy-op iretd    (gen-non 4 #xcf))
(def-sassy-op lahf     (gen-non 1 #x9f))
(def-sassy-op leave    (gen-non 1 #xc9))
(def-sassy-op lodsb    (gen-non 1 #xac))
(def-sassy-op lodsw    (gen-non 2 #xad))
(def-sassy-op lodsd    (gen-non 4 #xad))
(def-sassy-op movsb    (gen-non 1 #xa4))
(def-sassy-op movsw    (gen-non 2 #xa5)) ;movsd in sse2
(def-sassy-op nop      (gen-non 1 #x90))
(def-sassy-op outsb    (gen-non 1 #x6e))
(def-sassy-op outsw    (gen-non 2 #x6f))
(def-sassy-op outsd    (gen-non 4 #x6f))
(def-sassy-op popa     (gen-non 1 #x61))
(def-sassy-op popaw    (gen-non 2 #x61))
(def-sassy-op popad    (gen-non 4 #x61))
(def-sassy-op popf     (gen-non 1 #x9d))
(def-sassy-op popfw    (gen-non 2 #x9d))
(def-sassy-op popfd    (gen-non 4 #x9d))
(def-sassy-op pusha    (gen-non 1 #x60))
(def-sassy-op pushaw   (gen-non 2 #x60))
(def-sassy-op pushad   (gen-non 4 #x60))
(def-sassy-op pushf    (gen-non 1 #x9c))
(def-sassy-op pushfw   (gen-non 2 #x9c))
(def-sassy-op pushfd   (gen-non 4 #x9c))
(def-sassy-op rdmsr    (gen-non 1 '(#x0f #x32)))
(def-sassy-op rdpmc    (gen-non 1 '(#x0f #x33)))
(def-sassy-op rdtsc    (gen-non 1 '(#x0f #x31)))
(def-sassy-op rsm      (gen-non 1 '(#x0f #xaa)))
(def-sassy-op sahf     (gen-non 1 #x9e))
(def-sassy-op scasb    (gen-non 1 #xae))
(def-sassy-op scasw    (gen-non 2 #xaf))
(def-sassy-op scasd    (gen-non 4 #xaf))
(def-sassy-op stc      (gen-non 1 #xf9))
(def-sassy-op std      (gen-non 1 #xfd))
(def-sassy-op sti      (gen-non 1 #xfb))
(def-sassy-op stosb    (gen-non 1 #xaa))
(def-sassy-op stosw    (gen-non 2 #xab))
(def-sassy-op stosd    (gen-non 4 #xab))
(def-sassy-op sysenter (gen-non 1 '(#x0f #x34)))
(def-sassy-op sysexit  (gen-non 1 '(#x0f #x35)))
(def-sassy-op ud2      (gen-non 1 '(#x0f #x0b)))
(def-sassy-op wbinvd   (gen-non 1 '(#x0f #x09)))
(def-sassy-op wrmsr    (gen-non 1 '(#x0f #x30)))
(def-sassy-op xlat     (gen-non 1 #xd7))
(def-sassy-op xlatb    (gen-non 1 #xd7))
(def-sassy-op fld1     (gen-non 1 '(#xd9 #xe8)))
(def-sassy-op fldl2t   (gen-non 1 '(#xd9 #xe9)))
(def-sassy-op fldl2e   (gen-non 1 '(#xd9 #xea)))
(def-sassy-op fldpi    (gen-non 1 '(#xd9 #xeb)))
(def-sassy-op fldlg2   (gen-non 1 '(#xd9 #xec)))
(def-sassy-op fldln2   (gen-non 1 '(#xd9 #xed)))
(def-sassy-op fldz     (gen-non 1 '(#xd9 #xee)))
(def-sassy-op fsin     (gen-non 1 '(#xd9 #xfe)))
(def-sassy-op fcos     (gen-non 1 '(#xd9 #xff)))
(def-sassy-op fsincos  (gen-non 1 '(#xd9 #xfb)))
(def-sassy-op fptan    (gen-non 1 '(#xd9 #xf2)))
(def-sassy-op fpatan   (gen-non 1 '(#xd9 #xf3)))
(def-sassy-op f2xm1    (gen-non 1 '(#xd9 #xf0)))
(def-sassy-op fyl2x    (gen-non 1 '(#xd9 #xf1)))
(def-sassy-op fyl2xp1  (gen-non 1 '(#xd9 #xf9)))
(def-sassy-op fincstp  (gen-non 1 '(#xd9 #xf7)))
(def-sassy-op fdecstp  (gen-non 1 '(#xd9 #xf6)))
(def-sassy-op finit    (gen-non 1 '(#x9b #xdb #xe3)))
(def-sassy-op fninit   (gen-non 1 '(#xdb #xe3)))
(def-sassy-op fclex    (gen-non 1 '(#x9b #xdb #xe2)))
(def-sassy-op fnclex   (gen-non 1 '(#xdb #xe2)))
(def-sassy-op fnop     (gen-non 1 '(#xd9 #xd0)))
(def-sassy-op fcompp   (gen-non 1 '(#xde #xd9)))
(def-sassy-op fucompp  (gen-non 1 '(#xda #xe9)))
(def-sassy-op ftst     (gen-non 1 '(#xd9 #xe4)))
(def-sassy-op fxam     (gen-non 1 '(#xd9 #xe5)))
(def-sassy-op fprem    (gen-non 1 '(#xd9 #xf8)))
(def-sassy-op fprem1   (gen-non 1 '(#xd9 #xf5)))
(def-sassy-op fabs     (gen-non 1 '(#xd9 #xe1)))
(def-sassy-op fchs     (gen-non 1 '(#xd9 #xe0)))
(def-sassy-op frndint  (gen-non 1 '(#xd9 #xfc)))
(def-sassy-op fscale   (gen-non 1 '(#xd9 #xfd)))
(def-sassy-op fsqrt    (gen-non 1 '(#xd9 #xfa)))
(def-sassy-op fxtract  (gen-non 1 '(#xd9 #xf4)))
(def-sassy-op fwait    (gen-non 1 #x9b))
(def-sassy-op wait     (gen-non 1 #x9b))
(def-sassy-op emms     (gen-non 1 '(#x0f #x77)))
(def-sassy-op adc (gen-alu #x10 #x11 #x12 #x13 #x14 #x15 #b010))
(def-sassy-op add (gen-alu #x00 #x01 #x02 #x03 #x04 #x05 #b000))
(def-sassy-op and (gen-alu #x20 #x21 #x22 #x23 #x24 #x25 #b100))
(def-sassy-op cmp (gen-alu #x38 #x39 #x3a #x3b #x3c #x3d #b111))
(def-sassy-op or  (gen-alu #x08 #x09 #x0a #x0b #x0c #x0d #b001))
(def-sassy-op sbb (gen-alu #x18 #x19 #x1a #x1b #x1c #x1d #b011))
(def-sassy-op sub (gen-alu #x28 #x29 #x2a #x2b #x2c #x2d #b101))
(def-sassy-op xor (gen-alu #x30 #x31 #x32 #x33 #x34 #x35 #b110))
(def-sassy-op bt  (gen-bt '(#x0f #xa3) '(#x0f #xba) #b100))
(def-sassy-op btc (gen-bt '(#x0f #xbb) '(#x0f #xba) #b111))
(def-sassy-op btr (gen-bt '(#x0f #xb3) '(#x0f #xba) #b110))
(def-sassy-op bts (gen-bt '(#x0f #xab) '(#x0f #xba) #b101))
(def-sassy-op rcl (gen-shift #b010))
(def-sassy-op rcr (gen-shift #b011))
(def-sassy-op rol (gen-shift #b000))
(def-sassy-op ror (gen-shift #b001))
(def-sassy-op sal (gen-shift #b100))
(def-sassy-op sar (gen-shift #b111))
(def-sassy-op shl (gen-shift #b100))
(def-sassy-op shr (gen-shift #b101))
(def-sassy-op jo   (gen-jcc #x00))
(def-sassy-op jno  (gen-jcc #x01))
(def-sassy-op jb   (gen-jcc #x02))
(def-sassy-op jc   (gen-jcc #x02))
(def-sassy-op jnae (gen-jcc #x02))
(def-sassy-op jnb  (gen-jcc #x03))
(def-sassy-op jnc  (gen-jcc #x03))
(def-sassy-op jae  (gen-jcc #x03))
(def-sassy-op je   (gen-jcc #x04))
(def-sassy-op jz   (gen-jcc #x04))
(def-sassy-op jne  (gen-jcc #x05))
(def-sassy-op jnz  (gen-jcc #x05))
(def-sassy-op jbe  (gen-jcc #x06))
(def-sassy-op jna  (gen-jcc #x06))
(def-sassy-op ja   (gen-jcc #x07))
(def-sassy-op jnbe (gen-jcc #x07))
(def-sassy-op js   (gen-jcc #x08))
(def-sassy-op jns  (gen-jcc #x09))
(def-sassy-op jp   (gen-jcc #x0a))  
(def-sassy-op jpe  (gen-jcc #x0a))  
(def-sassy-op jnp  (gen-jcc #x0b))  
(def-sassy-op jpo  (gen-jcc #x0b))  
(def-sassy-op jl   (gen-jcc #x0c))
(def-sassy-op jnge (gen-jcc #x0c))
(def-sassy-op jge  (gen-jcc #x0d))
(def-sassy-op jnl  (gen-jcc #x0d))
(def-sassy-op jle  (gen-jcc #x0e))
(def-sassy-op jng  (gen-jcc #x0e))
(def-sassy-op jnle (gen-jcc #x0f))
(def-sassy-op jg   (gen-jcc #x0f))
(def-sassy-op cmovo   (gen-cmovcc #x00))
(def-sassy-op cmovno  (gen-cmovcc #x01))
(def-sassy-op cmovb   (gen-cmovcc #x02))
(def-sassy-op cmovc   (gen-cmovcc #x02))
(def-sassy-op cmovnae (gen-cmovcc #x02))
(def-sassy-op cmovnb  (gen-cmovcc #x03))
(def-sassy-op cmovnc  (gen-cmovcc #x03))
(def-sassy-op cmovae  (gen-cmovcc #x03))
(def-sassy-op cmove   (gen-cmovcc #x04))
(def-sassy-op cmovz   (gen-cmovcc #x04))
(def-sassy-op cmovne  (gen-cmovcc #x05))
(def-sassy-op cmovnz  (gen-cmovcc #x05))
(def-sassy-op cmovbe  (gen-cmovcc #x06))
(def-sassy-op cmovna  (gen-cmovcc #x06))
(def-sassy-op cmova   (gen-cmovcc #x07))
(def-sassy-op cmovnbe (gen-cmovcc #x07))
(def-sassy-op cmovs   (gen-cmovcc #x08))
(def-sassy-op cmovns  (gen-cmovcc #x09))
(def-sassy-op cmovp   (gen-cmovcc #x0a))  
(def-sassy-op cmovpe  (gen-cmovcc #x0a))  
(def-sassy-op cmovnp  (gen-cmovcc #x0b))  
(def-sassy-op cmovpo  (gen-cmovcc #x0b))  
(def-sassy-op cmovl   (gen-cmovcc #x0c))
(def-sassy-op cmovnge (gen-cmovcc #x0c))
(def-sassy-op cmovge  (gen-cmovcc #x0d))
(def-sassy-op cmovnl  (gen-cmovcc #x0d))
(def-sassy-op cmovle  (gen-cmovcc #x0e))
(def-sassy-op cmovng  (gen-cmovcc #x0e))
(def-sassy-op cmovnle (gen-cmovcc #x0f))
(def-sassy-op cmovg   (gen-cmovcc #x0f))
(def-sassy-op seto   (gen-setcc #x00))
(def-sassy-op setno  (gen-setcc #x01))
(def-sassy-op setb   (gen-setcc #x02))
(def-sassy-op setc   (gen-setcc #x02))
(def-sassy-op setnae (gen-setcc #x02))
(def-sassy-op setnb  (gen-setcc #x03))
(def-sassy-op setnc  (gen-setcc #x03))
(def-sassy-op setae  (gen-setcc #x03))
(def-sassy-op sete   (gen-setcc #x04))
(def-sassy-op setz   (gen-setcc #x04))
(def-sassy-op setne  (gen-setcc #x05))
(def-sassy-op setnz  (gen-setcc #x05))
(def-sassy-op setbe  (gen-setcc #x06))
(def-sassy-op setna  (gen-setcc #x06))
(def-sassy-op seta   (gen-setcc #x07))
(def-sassy-op setnbe (gen-setcc #x07))
(def-sassy-op sets   (gen-setcc #x08))
(def-sassy-op setns  (gen-setcc #x09))
(def-sassy-op setp   (gen-setcc #x0a))  
(def-sassy-op setpe  (gen-setcc #x0a))  
(def-sassy-op setnp  (gen-setcc #x0b))  
(def-sassy-op setpo  (gen-setcc #x0b))  
(def-sassy-op setl   (gen-setcc #x0c))
(def-sassy-op setnge (gen-setcc #x0c))
(def-sassy-op setge  (gen-setcc #x0d))
(def-sassy-op setnl  (gen-setcc #x0d))
(def-sassy-op setle  (gen-setcc #x0e))
(def-sassy-op setng  (gen-setcc #x0e))
(def-sassy-op setnle (gen-setcc #x0f))
(def-sassy-op setg   (gen-setcc #x0f))
(def-sassy-op dec (gen-decinc #x48 #b001))
(def-sassy-op inc (gen-decinc #x40 #b000))
(def-sassy-op div  (gen-plier #b110))
(def-sassy-op idiv (gen-plier #b111))
(def-sassy-op mul  (gen-plier #b100))
(def-sassy-op neg  (gen-plier #b011))
(def-sassy-op not  (gen-plier #b010))
(def-sassy-op lds (gen-load #xc5))
(def-sassy-op les (gen-load #xc4))
(def-sassy-op lea (gen-load #x8d))
(def-sassy-op lfs (gen-load '(#x0f #xb4)))
(def-sassy-op lgs (gen-load '(#x0f #xb5)))
(def-sassy-op lss (gen-load '(#x0f #xb2)))
(def-sassy-op movsx (gen-movx #xbe #xbf))
(def-sassy-op movzx (gen-movx #xb6 #xb7))
(def-sassy-op lldt (gen-rm2 '(#x0f #x00) #b010))
(def-sassy-op sldt (gen-rm2 '(#x0f #x00) #b000))
(def-sassy-op lmsw (gen-rm2 '(#x0f #x01) #b110))
(def-sassy-op smsw (gen-rm2 '(#x0f #x01) #b100))
(def-sassy-op ltr  (gen-rm2 '(#x0f #x00) #b011))
(def-sassy-op str  (gen-rm2 '(#x0f #x00) #b001))
(def-sassy-op verr (gen-rm2 '(#x0f #x00) #b100))
(def-sassy-op verw (gen-rm2 '(#x0f #x00) #b101))
(def-sassy-op invlpg (gen-rm  #x01 #b111))
(def-sassy-op lgdt   (gen-rm  #x01 #b010))
(def-sassy-op sgdt   (gen-rm  #x01 #b000))
(def-sassy-op lidt   (gen-rm  #x01 #b011))
(def-sassy-op sidt   (gen-rm  #x01 #b001))
(def-sassy-op bsf (gen-r/rm '(#x0f #xbc)))
(def-sassy-op bsr (gen-r/rm '(#x0f #xbd)))
(def-sassy-op lar (gen-r/rm '(#x0f #x02)))
(def-sassy-op lsl (gen-r/rm '(#x0f #x03)))
(def-sassy-op aad (gen-aa '(#xd5 #x0a) #xd5))
(def-sassy-op aam (gen-aa '(#xd4 #x0a) #xd4))
(def-sassy-op ret  (gen-ret #xc3 #xc2))
(def-sassy-op retn (gen-ret #xc3 #xc2))
(def-sassy-op retf (gen-ret #xcb #xca))
(def-sassy-op shld (gen-doub-shift #xa4 #xa5))
(def-sassy-op shrd (gen-doub-shift #xac #xad))
(def-sassy-op loop   (gen-loop #xe2))
(def-sassy-op loope  (gen-loop #xe1))
(def-sassy-op loopz  (gen-loop #xe1))
(def-sassy-op loopne (gen-loop #xe0))
(def-sassy-op loopnz (gen-loop #xe0))
(def-sassy-op cmpxchg (gen-cmpx '(#x0f #xb0) '(#x0f #xb1)))
(def-sassy-op xadd    (gen-cmpx '(#x0f #xc0) '(#x0f #xc1)))

(def-sassy-op arpl
  (mini-meta
   (and (or r16 m16) r16 (lambda (t x y) (r/m-r t 1 #x63 x y)))))

(def-sassy-op bound
  (mini-meta
   (or (and r16 m16 (lambda (t x y) (r-r/m t 2 #x62 x y)))
       (and r32 m32 (lambda (t x y) (r-r/m t 4 #x62 x y))))))

(def-sassy-op bswap
  (mini-meta
   (and r32 (lambda (t x) (just-r2 t 4 #x0f #xc8 x)))))

(def-sassy-op cmpxchg8b
  (mini-meta
   (and m64 (lambda (t x) (just-m t 1 '(#x0f #xc7) x #b001)))))

(def-sassy-op xchg
  (mini-meta
   (or (and 'eax (or (and r32 (lambda (t y) (just-r t 4 #x90 y)))))
       (and r32 (or (and 'eax (lambda (t y) (just-r t 4 #x90 y)))
		    (and (or r32 m32) (lambda (t x y) (r-r/m t 4 #x87 x y)))))
       (and 'ax r16 (lambda (t y) (just-r t 2 #x90 y)))
       (and r16 (or (and 'ax (lambda (t y) (just-r t 2 #x90 y)))
		    (and (or r16 m16) (lambda (t x y) (r-r/m t 2 #x87 x y)))))
       (and r8 (or r8 m8) (lambda (t x y) (r-r/m t 1 #x86 x y)))
       (and m8 r8 (lambda (t x y) (r/m-r t 1 #x86 x y)))
       (and m16 r16 (lambda (t x y) (r/m-r t 2 #x87 x y)))
       (and m32 r32 (lambda (t x y) (r/m-r t 4 #x87 x y))))))

(def-sassy-op enter
  (mini-meta
   (and i16 i8 (lambda (t x y) (i16-i8 t 1 #xc8 x y)))))

(def-sassy-op jcxz
  (mini-meta
   (and rel8 (lambda (t x)
	       (if (= 16 (sassy-bits (t-outp t)))
		   (just-i-rel t 1 #xe3 x)
		   (just-i-rel t 1 '(#x67 #xe3) x))))))

(def-sassy-op jecxz
  (mini-meta
   (and rel8 (lambda (t x)
	       (if (= 16 (sassy-bits (t-outp t)))
		   (just-i-rel t 1 '(#x67 #xe3) x)
		   (just-i-rel t 1 #xe3 x))))))

(def-sassy-op in
  (mini-meta
   (or (and 'al (or (and i8 (lambda (t y) (just-i8 t 1 #xe4 y)))
		    (and 'dx (lambda (t) (just-c t 1 #xec)))))
       (and 'ax (or (and i8 (lambda (t y) (just-i8 t 2 #xe5 y)))
		    (and 'dx (lambda (t) (just-c t 2 #xed)))))
       (and 'eax (or (and i8 (lambda (t y) (just-i8 t 4 #xe5 y)))
		     (and 'dx (lambda (t) (just-c t 4 #xed))))))))

(def-sassy-op out
  (mini-meta
   (or (and i8 (or (and 'al (lambda (t x) (just-i8 t 1 #xe6 x)))
		   (and 'ax (lambda (t x) (just-i8 t 2 #xe7 x)))
		   (and 'eax (lambda (t x) (just-i8 t 4 #xe7 x)))))
       (and 'dx (or (and 'al (lambda (t) (just-c t 1 #xee)))
		    (and 'ax (lambda (t) (just-c t 2 #xef)))
		    (and 'eax (lambda (t) (just-c t 4 #xef))))))))

(def-sassy-op int
  (mini-meta
   (and i8 (lambda (t x) (just-i8 t 1 #xcd x)))))

(def-sassy-op pop
  (mini-meta
   (or (and r16 (lambda (t x) (just-r t 2 #x58 x)))
       (and r32 (lambda (t x) (just-r t 4 #x58 x)))
       (and em32 (lambda (t x) (just-m t 4 #x8f x #b000)))
       (and em16 (lambda (t x) (just-m t 2 #x8f x #b000)))
       (and um32 (lambda (t x)
		   (if (= 16 (sassy-bits (t-outp t)))
		       (just-m t 2 #x8f x #b000)
		       (just-m t 4 #x8f x #b000))))
       (and 'ds (lambda (t) (just-c t 1 #x1f)))
       (and 'es (lambda (t) (just-c t 1 #x07)))
       (and 'ss (lambda (t) (just-c t 1 #x17)))
       (and 'fs (lambda (t) (just-c t 1 '(#x0f #xa1))))
       (and 'gs (lambda (t) (just-c t 1 '(#x0f #xa9)))))))

(def-sassy-op push
  (mini-meta
   (or (and r32 (lambda (t x) (just-r t 4 #x50 x)))
       (and r16 (lambda (t x) (just-r t 2 #x50 x)))
       (and i8  (lambda (t x) (just-i t 1 #x6a x)))
       (and ei32 (lambda (t x) (just-i t 4 #x68 x)))
       (and ei16 (lambda (t x) (just-i t 2 #x68 x)))
       (and ui32 (lambda (t x)
		   (if (and (ui16 x) (= 16 (sassy-bits (t-outp t))))
		       (just-i t 2 #x68 x)
		       (just-i t 4 #x68 x))))
       (and em32 (lambda (t x) (just-m t 4 #xff x #b110)))
       (and em16 (lambda (t x) (just-m t 2 #xff x #b110)))
       (and um32 (lambda (t x)
		   (if (= 16 (sassy-bits (t-outp t)))
		       (just-m t 2 #xff x #b110)
		       (just-m t 4 #xff x #b110))))
       (and 'cs (lambda (t) (just-c t 1 #x0e)))
       (and 'ds (lambda (t) (just-c t 1 #x1e)))
       (and 'es (lambda (t) (just-c t 1 #x06)))
       (and 'ss (lambda (t) (just-c t 1 #x16)))
       (and 'fs (lambda (t) (just-c t 1 '(#x0f #xa0))))
       (and 'gs (lambda (t) (just-c t 1 '(#x0f #xa8)))))))

(def-sassy-op imul
  (mini-meta
   (or
    (and r32
	 (or (lambda (t x) (r/m t 4 247 x 5))
	     (and (or r32 m32)
		  (or (lambda (t x y) (r-r/m t 4 '(15 175) x y))
		      (and si8 (lambda (t x y z) (r-r/m-i8 t 4 107 x y z)))
		      (and i32 (lambda (t x y z) (r-r/m-i t 4 105 x y z)))))
	     (and si8 (lambda (t x y) (r-r/m-i8 t 4 107 x x y)))
	     (and i32 (lambda (t x y) (r-r/m-i t 4 105 x x y)))))
    (and r16
	 (or (lambda (t x) (r/m t 2 247 x 5))
	     (and (or r16 m16)
		  (or (lambda (t x y) (r-r/m t 2 '(15 175) x y))
		      (and si8 (lambda (t x y z) (r-r/m-i8 t 2 107 x y z)))
		      (and i16 (lambda (t x y z) (r-r/m-i t 2 105 x y z)))))
	     (and si8 (lambda (t x y) (r-r/m-i8 t 2 107 x x y)))
	     (and i16 (lambda (t x y) (r-r/m-i t 2 105 x x y)))))
    (and em32 (lambda (t x) (r/m t 4 #xf7 x #b101)))
    (and em16 (lambda (t x) (r/m t 2 #xf7 x #b101)))
    (and um32 (lambda (t x)
		(if (= 16 (sassy-bits (t-outp t)))
		    (r/m t 2 #xf7 x #b101)
		    (r/m t 4 #xf7 x #b101))))
    (and (or r8 m8) (lambda (t x) (r/m t 1 #xf6 x #b101))))))

(def-sassy-op test
  (mini-meta
   (or
    (and 'eax i32 (lambda (t y) (just-i t 4 #xa9 y)))
    (and 'al  i8  (lambda (t y) (just-i t 1 #xa8 y)))
    (and 'ax  i16 (lambda (t y) (just-i t 2 #xa9 y)))
    (and (or r8  em8)
	 (or (and r8 (lambda (t x y) (r/m-r t 1 #x84 x y)))
	     (and i8 (lambda (t x y) (r/m-i t 1 #xf6 x #b000 y)))))
    (and um8 r8 (lambda (t x y) (r/m-r t 1 #x84 x y)))
    (and (or r32 em32) (or (and r32 (lambda (t x y) (r/m-r t 4 #x85 x y)))
			   (and i32 (lambda (t x y)
				      (r/m-i t 4 #xf7 x #b000 y)))))
    (and (or r16 em16)
	 (or (and r16 (lambda (t x y) (r/m-r t 2 #x85 x y)))
	     (and i16 (lambda (t x y) (r/m-i t 2 #xf7 x #b000 y)))))
    (and um32 r32 (lambda (t x y) (r/m-r t 4 #x85 x y)))
    (and um16 r16 (lambda (t x y) (r/m-r t 2 #x85 x y)))
    (and um32 ei32 (lambda (t x y) (r/m-i t 4 #xf7 x #b000 y)))
    (and um16 ei16 (lambda (t x y) (r/m-i t 2 #xf7 x #b000 y)))
    (and um32 ui32 (lambda (t x y)
		     (if (and (ui16 y) (= 16 (sassy-bits (t-outp t))))
			 (r/m-i t 2 #xf7 x #b000 y)
			 (r/m-i t 4 #xf7 x #b000 y)))))))

(def-sassy-op mov
  (mini-meta
   (or
    (and 'eax mi32 (lambda (t y) (just-i32 t 4 #xa1 y)))
    (and mi32 'eax (lambda (t x) (just-i32 t 4 #xa3 x)))
    (and 'al mi8   (lambda (t y) (just-i32 t 1 #xa0 y)))
    (and mi8 'al   (lambda (t x) (just-i32 t 1 #xa2 x)))
    (and 'ax mi16  (lambda (t y) (just-i32 t 2 #xa1 y)))
    (and mi16 'ax  (lambda (t x) (just-i32 t 2 #xa3 x)))
    (and r32 (or (and r32 (lambda (t x y) (r/m-r t 4 137 x y)))
		 (and m32 (lambda (t x y) (r-r/m t 4 139 x y)))
		 (and i32 (lambda (t x y) (just-r-i t 4 184 x y)))
		 (and sreg-not-cs (lambda (t x y) (r/m-r t 4 140 x y)))
		 (and creg (lambda (t x y) (r/m-r t 1 '(15 32) x y)))
		 (and dreg (lambda (t x y) (r/m-r t 1 '(15 33) x y)))
		 (and treg (lambda (t x y) (r/m-r t 1 '(15 #x24) x y)))))
    (and r16 (or (and r16 (lambda (t x y) (r/m-r t 2 137 x y)))
		 (and m16 (lambda (t x y) (r-r/m t 2 139 x y)))
		 (and i16 (lambda (t x y) (just-r-i t 2 184 x y)))
		 (and sreg-not-cs (lambda (t x y) (r/m-r t 2 140 x y)))))
    (and r8 (or (and r8 (lambda (t x y) (r/m-r t 1 136 x y)))
		(and m8 (lambda (t x y) (r-r/m t 1 138 x y)))
		(and i8 (lambda (t x y) (just-r-i t 1 176 x y)))))
    (and creg r32 (lambda (t x y) (r/m-r t 1 '(#x0f #x22) y x)))
    (and dreg r32 (lambda (t x y) (r/m-r t 1 '(#x0f #x23) y x)))
    (and treg r32 (lambda (t x y) (r/m-r t 1 '(#x0f #x26) y x)))
    (and m32 (or (and r32 (lambda (t x y) (r/m-r t 4 #x89 x y)))
		 (and i32 (lambda (t x y) (r/m-i t 4 #xc7 x #b000 y)))))
    (and m8 (or (and r8 (lambda (t x y) (r/m-r t 1 #x88 x y)))
		(and i8 (lambda (t x y) (r/m-i t 1 #xc6 x #b000 y)))))
    (and m16 (or (and r16 (lambda (t x y) (r/m-r t 2 #x89 x y)))
		 (and i16 (lambda (t x y) (r/m-i t 2 #xc7 x #b000 y)))))
    (and sreg-not-cs (or (and (or r32 r16 em32 em16)
			      (lambda (t x y) (r-r/m t 1 #x8e x y)))
			 (and um32 (lambda (t x y)
				     (if (= 16 (sassy-bits (t-outp t)))
					 (r-r/m t 2 #x8e x y)
					 (r-r/m t 4 #x8e x y))))))
    (and em32 sreg-not-cs (lambda (t x y) (r/m-r t 4 #x8c x y)))
    (and em16 sreg-not-cs (lambda (t x y) (r/m-r t 2 #x8c x y)))
    (and um32 sreg-not-cs (lambda (t x y)
			    (if (= 16 (sassy-bits (t-outp t)))
				(r/m-r t 2 #x8c x y)
				(r/m-r t 4 #x8c x y)))))))

(def-sassy-op jmp
  (mini-meta
   (or
; (or erel8 (and 'short (or rel32 rel16 rel8)))
    (and 'short (or rel32 rel16 rel8) (lambda (t x) (just-i-rel t 1 #xeb x)))
    (and (?* 'near)
	 (or (and erel32 (lambda (t x) (just-i-rel t 4 #xe9 x)))
	     (and erel16 (lambda (t x) (just-i-rel t 2 #xe9 x)))
	     (and urel32 (lambda (t x)
			   (if (and (urel16 x) (= 16 (sassy-bits (t-outp t))))
			       (just-i-rel t 2 #xe9 x)
			       (just-i-rel t 4 #xe9 x))))))
    (and r32 (lambda (t x) (r/m t 4 #xff x #b100)))
    (and r16 (lambda (t x) (r/m t 2 #xff x #b100)))
    (and em32 (lambda (t x) (r/m t 4 #xff x #b100)))
    (and em16 (lambda (t x) (r/m t 2 #xff x #b100)))
    (and m32 (lambda (t x)
	       (if (= 16 (sassy-bits (t-outp t)))
		   (r/m t 2 #xff x #b100)
		   (r/m t 4 #xff x #b100))))
    (and i16 (or (and ei32 (lambda (t x y) (i16-i32 t 4 #xea y x)))
		 (and ei16 (lambda (t x y) (i16-i16 t 2 #xea y x)))
		 (and ui32 (lambda (t x y)
			     (if (and (ui16 y) (= 16 (sassy-bits (t-outp t))))
				 (i16-i16 t 2 #xea y x)
				 (i16-i32 t 4 #xea y x))))))
    (and 'far (or (and em32 (lambda (t x) (just-m t 4 #xff x #b101)))
		  (and em16 (lambda (t x) (just-m t 2 #xff x #b101)))
		  (and um32 (lambda (t x)
			      (if (= 16 (sassy-bits (t-outp t)))
				  (just-m t 2 #xff x #b101)
				  (just-m t 4 #xff x #b101)))))))))

(def-sassy-op call
  (mini-meta
   (or
    (and erel32 (lambda (t x) (just-i-rel t 4 #xe8 x)))
    (and erel16 (lambda (t x) (just-i-rel t 2 #xe8 x)))
    (and urel32 (lambda (t x) 
		  (if (and (urel16 x)
			   (= 16 (sassy-bits (t-outp t))))
		      (just-i-rel t 2 #xe8 x)
		      (just-i-rel t 4 #xe8 x))))
    (and i16 (or (and ei32 (lambda (t x y) (i16-i32 t 4 #x9a y x)))
		 (and ei16 (lambda (t x y) (i16-i16 t 2 #x9a y x)))
		 (and ui32 (lambda (t x y)
			     (if (and (ui16 y) (= 16 (sassy-bits (t-outp t))))
				 (i16-i16 t 2 #x9a y x)
				 (i16-i32 t 4 #x9a y x))))))
    (and 'far (or (and em32 (lambda (t x) (just-m t 4 #xff x #b011)))
		  (and em16 (lambda (t x) (just-m t 2 #xff x #b011)))
		  (and um32 (lambda (t x)
			      (if (= 16 (sassy-bits (t-outp t)))
				  (just-m t 2 #xff x #b011)
				  (just-m t 4 #xff x #b011))))))
    (and r32 (lambda (t x) (r/m t 4 #xff x #b010)))
    (and r16 (lambda (t x) (r/m t 2 #xff x #b010)))
    (and em32 (lambda (t x) (r/m t 4 #xff x #b010)))
    (and em16 (lambda (t x) (r/m t 2 #xff x #b010)))
    (and um32 (lambda (t x)
		(if (= 16 (sassy-bits (t-outp t)))
		    (r/m t 2 #xff x #b010)
		    (r/m t 4 #xff x #b010)))))))

(def-sassy-op fadd  (gen-fpmath-1 #b000 #xc0 #xc0))
(def-sassy-op fsub  (gen-fpmath-1 #b100 #xe0 #xe8))
(def-sassy-op fsubr (gen-fpmath-1 #b101 #xe8 #xe0))
(def-sassy-op fmul  (gen-fpmath-1 #b001 #xc8 #xc8))
(def-sassy-op fdiv  (gen-fpmath-1 #b110 #xf0 #xf8))
(def-sassy-op fdivr (gen-fpmath-1 #b111 #xf8 #xf0))
(def-sassy-op fdivrp (gen-fpmath-2 #xf0 #xf1))
(def-sassy-op fdivp  (gen-fpmath-2 #xf8 #xf9))
(def-sassy-op fmulp  (gen-fpmath-2 #xc8 #xc9))
(def-sassy-op fsubp  (gen-fpmath-2 #xe8 #xe9))
(def-sassy-op fsubrp (gen-fpmath-2 #xe0 #xe1))
(def-sassy-op faddp  (gen-fpmath-2 #xc0 #xc1))
(def-sassy-op fimul  (gen-fpmath-3 #b001))
(def-sassy-op fiadd  (gen-fpmath-3 #b000))
(def-sassy-op fidiv  (gen-fpmath-3 #b110))
(def-sassy-op fidivr (gen-fpmath-3 #b111))
(def-sassy-op fisub  (gen-fpmath-3 #b100))
(def-sassy-op fisubr (gen-fpmath-3 #b101))
(def-sassy-op fcmovb   (gen-fcmovcc #xda #xc0))
(def-sassy-op fcmove   (gen-fcmovcc #xda #xc8))
(def-sassy-op fcmovbe  (gen-fcmovcc #xda #xd0))
(def-sassy-op fcmovu   (gen-fcmovcc #xda #xd8))
(def-sassy-op fcmovnb  (gen-fcmovcc #xdb #xc0))
(def-sassy-op fcmovne  (gen-fcmovcc #xdb #xc8))
(def-sassy-op fcmovnbe (gen-fcmovcc #xdb #xd0))
(def-sassy-op fcmovnu  (gen-fcmovcc #xdb #xd8))
(def-sassy-op fxch   (gen-fp-reg/non #xd9 #xc8 '(#xd9 #xc9)))
(def-sassy-op fucom  (gen-fp-reg/non #xdd #xe0 '(#xdd #xe1)))
(def-sassy-op fucomp (gen-fp-reg/non #xdd #xe8 '(#xdd #xe9)))
(def-sassy-op fld  (gen-fp-3m/st #b000 #b101 #xd9 #xc0))
(def-sassy-op fstp (gen-fp-3m/st #b011 #b111 #xdd #xd8))

(def-sassy-op fst
  (mini-meta
   (or (and em64 (lambda (t x) (r/m t 1 #xdd x #b010)))
       (and m32 (lambda (t x) (r/m t 1 #xd9 x #b010)))
       (and st  (lambda (t x) (just-r2 t 1 #xdd #xd0 x))))))

(def-sassy-op fild  (gen-fp-3int #b000 #b101))
(def-sassy-op fistp (gen-fp-3int #b011 #b111))
(def-sassy-op fist   (gen-fp-2int #xdf #xdb #b010))
(def-sassy-op ficom  (gen-fp-2int #xde #xda #b010))
(def-sassy-op ficomp (gen-fp-2int #xde #xda #b011))
(def-sassy-op fcom  (gen-fp-com #xd0 #xd1 #b010))
(def-sassy-op fcomp (gen-fp-com #xd8 #xd9 #b011))

(def-sassy-op fcomi
  (mini-meta
   (and 'st0 st (lambda (t x) (just-r2 t 1 #xdb #xf0 x)))))

(def-sassy-op fcomip
  (mini-meta
   (and 'st0 st (lambda (t x) (just-r2 t 1 #xdf #xf0 x)))))

(def-sassy-op fucomi
  (mini-meta
   (and 'st0 st (lambda (t x) (just-r2 t 1 #xdb #xe8 x)))))

(def-sassy-op fucomip
  (mini-meta
   (and 'st0 st (lambda (t x) (just-r2 t 1 #xdf #xe8 x)))))

(def-sassy-op fbld
  (mini-meta
   (and m80 (lambda (t x) (r/m t 1 #xdf x #b100)))))

(def-sassy-op fbstp
  (mini-meta
   (and m80 (lambda (t x) (r/m t 1 #xdf x #b110)))))

(def-sassy-op ffree
  (mini-meta
   (and st (lambda (t x) (just-r2 t 1 #xdd #xc0 x)))))

(def-sassy-op fstcw
  (mini-meta
   (and m16 (lambda (t x) (r/m t 1 '(#x9b #xd9) x #b111)))))

(def-sassy-op fnstcw
  (mini-meta
   (and m16 (lambda (t x) (r/m t 1 #xd9 x #b111)))))

(def-sassy-op fldcw
  (mini-meta
   (and m16 (lambda (t x) (r/m t 1 #xd9 x #b101)))))

(def-sassy-op fstenv
  (mini-meta
   (and (or em16 m32) (lambda (t x) (r/m t 1 '(#x9b #xd9) x #b110)))))

(def-sassy-op fnstenv
  (mini-meta
   (and (or em16 m32) (lambda (t x) (r/m t 1 #xd9 x #b110)))))

(def-sassy-op fldenv
  (mini-meta
   (and (or em16 m32) (lambda (t x) (r/m t 1 #xd9 x #b100)))))

(def-sassy-op fsave
  (mini-meta
   (and mem-any
	(lambda (t x) (r/m t 1 '(#x9b #xdd) x #b110)))))

(def-sassy-op fnsave
  (mini-meta
   (and mem-any (lambda (t x) (r/m t 1 #xdd x #b110)))))

(def-sassy-op frstor
  (mini-meta
   (and mem-any (lambda (t x) (r/m t 1 #xdd x #b100)))))

(def-sassy-op fxsave
  (mini-meta
   (and mem-any (lambda (t x) (r/m t 1 '(#x0f #xae) x #b000)))))

(def-sassy-op fxrstor
  (mini-meta
   (and mem-any (lambda (t x) (r/m t 1 '(#x0f #xae) x #b001)))))

(def-sassy-op fstsw
  (mini-meta
   (or (and 'ax (lambda (t) (just-c t 1 '(#x9b #xdf #xe0))))
       (and m16 (lambda (t x) (r/m t 1 '(#x9b #xdd) x #b111))))))

(def-sassy-op fnstsw
  (mini-meta
   (or (and 'ax (lambda (t) (just-c t 1 '(#xdf #xe0))))
       (and m16 (lambda (t x) (r/m t 1 '#xdd x #b111))))))

(def-sassy-op movd
  (mini-meta
   (or (and mm (or r32 m32) (lambda (t x y) (r-r/m t 1 '(#x0f #x6e) x y)))
       (and xmm (or r32 m32)
	    (lambda (t x y) (r-r/m t 1 '(#x66 #x0f #x6e) x y)))
       (and
	(or r32 m32) (or (and mm
			      (lambda (t x y) (r/m-r t 1 '(#x0f #x7e) x y)))
			 (and xmm
			      (lambda (t x y)
				(r/m-r t 1 '(#x66 #x0f #x7e) x y))))))))

(def-sassy-op movq
  (mini-meta
   (or (and mm  (or mm m64) (lambda (t x y) (r-r/m t 1 '(#x0f #x6f) x y)))
       (and xmm (or xmm m64)
	    (lambda (t x y) (r-r/m t 1 '(#xf3 #x0f #x7e) x y)))
       (and m64 (or (and mm (lambda (t x y) (r/m-r t 1 '(#x0f #x7f) x y)))
		    (and xmm (lambda (t x y)
			       (r/m-r t 1 '(#x66 #x0f #xd6) x y))))))))

(def-sassy-op pand      (gen-mmx-log #xdb))
(def-sassy-op pandn     (gen-mmx-log #xdf))
(def-sassy-op por       (gen-mmx-log #xeb))
(def-sassy-op pxor      (gen-mmx-log #xef))
(def-sassy-op packsswb  (gen-mmx-log #x63))
(def-sassy-op packssdw  (gen-mmx-log #x6b))
(def-sassy-op packuswb  (gen-mmx-log #x67))
(def-sassy-op punpckhbw (gen-mmx-log #x68))
(def-sassy-op punpckhwd (gen-mmx-log #x69))
(def-sassy-op punpckhdq (gen-mmx-log #x6a))
(def-sassy-op paddb     (gen-mmx-log #xfc))
(def-sassy-op paddw     (gen-mmx-log #xfd))
(def-sassy-op paddd     (gen-mmx-log #xfe))
(def-sassy-op paddsb    (gen-mmx-log #xec))
(def-sassy-op paddsw    (gen-mmx-log #xed))
(def-sassy-op paddusb   (gen-mmx-log #xdc))
(def-sassy-op paddusw   (gen-mmx-log #xdd))
(def-sassy-op psubb     (gen-mmx-log #xf8))
(def-sassy-op psubw     (gen-mmx-log #xf9))
(def-sassy-op psubd     (gen-mmx-log #xfa))
(def-sassy-op psubsb    (gen-mmx-log #xe8))
(def-sassy-op psubsw    (gen-mmx-log #xe9))
(def-sassy-op psubusb   (gen-mmx-log #xd8))
(def-sassy-op psubusw   (gen-mmx-log #xd9))
(def-sassy-op pmullw    (gen-mmx-log #xd5))
(def-sassy-op pmulhw    (gen-mmx-log #xe5))
(def-sassy-op pmaddwd   (gen-mmx-log #xf5))
(def-sassy-op pcmpeqb   (gen-mmx-log #x74))
(def-sassy-op pcmpeqw   (gen-mmx-log #x75))
(def-sassy-op pcmpeqd   (gen-mmx-log #x76))
(def-sassy-op pcmpgtb   (gen-mmx-log #x64))
(def-sassy-op pcmpgtw   (gen-mmx-log #x65))
(def-sassy-op pcmpgtd   (gen-mmx-log #x66))
(def-sassy-op pavgb     (gen-mmx-log #xe0))
(def-sassy-op pavgw     (gen-mmx-log #xe3))
(def-sassy-op pmaxub    (gen-mmx-log #xde))
(def-sassy-op pmaxsw    (gen-mmx-log #xee))
(def-sassy-op pminub    (gen-mmx-log #xda))
(def-sassy-op pminsw    (gen-mmx-log #xea))
(def-sassy-op pmulhuw   (gen-mmx-log #xe4))
(def-sassy-op psadbw    (gen-mmx-log #xf6))
(def-sassy-op punpcklbw (gen-mmx-unplow #x60))
(def-sassy-op punpcklwd (gen-mmx-unplow #x61))
(def-sassy-op punpckldq (gen-mmx-unplow #x62))
(def-sassy-op psrlw (gen-mmx-shr #xd1 #x71 #b010))
(def-sassy-op psrld (gen-mmx-shr #xd2 #x72 #b010))
(def-sassy-op psrlq (gen-mmx-shr #xd3 #x73 #b010))
(def-sassy-op psllw (gen-mmx-shr #xf1 #x71 #b110))
(def-sassy-op pslld (gen-mmx-shr #xf2 #x72 #b110))
(def-sassy-op psllq (gen-mmx-shr #xf3 #x73 #b110))
(def-sassy-op psraw (gen-mmx-shr #xe1 #x71 #b100))
(def-sassy-op psrad (gen-mmx-shr #xe2 #x72 #b100))
(def-sassy-op movaps (gen-sse1-mov '(#x0f #x28) '(#x0f #x29)))
(def-sassy-op movups (gen-sse1-mov '(#x0f #x10) '(#x0f #x11)))
(def-sassy-op addps      (gen-sse-ps/pd '(#x0f #x58)))
(def-sassy-op subps      (gen-sse-ps/pd '(#x0f #x5c)))
(def-sassy-op mulps      (gen-sse-ps/pd '(#x0f #x59)))
(def-sassy-op divps      (gen-sse-ps/pd '(#x0f #x5e)))
(def-sassy-op rcpps      (gen-sse-ps/pd '(#x0f #x53)))
(def-sassy-op sqrtps     (gen-sse-ps/pd '(#x0f #x51)))
(def-sassy-op rsqrtps    (gen-sse-ps/pd '(#x0f #x52)))
(def-sassy-op maxps      (gen-sse-ps/pd '(#x0f #x5f)))
(def-sassy-op minps      (gen-sse-ps/pd '(#x0f #x5d)))
(def-sassy-op andps      (gen-sse-ps/pd '(#x0f #x54)))
(def-sassy-op andnps     (gen-sse-ps/pd '(#x0f #x55)))
(def-sassy-op orps       (gen-sse-ps/pd '(#x0f #x56)))
(def-sassy-op xorps      (gen-sse-ps/pd '(#x0f #x57)))
(def-sassy-op unpckhps   (gen-sse-ps/pd '(#x0f #x15)))
(def-sassy-op unpcklps   (gen-sse-ps/pd '(#x0f #x14)))
(def-sassy-op addpd      (gen-sse-ps/pd '(#x66 #x0f #x58)))
(def-sassy-op subpd      (gen-sse-ps/pd '(#x66 #x0f #x5c)))
(def-sassy-op mulpd      (gen-sse-ps/pd '(#x66 #x0f #x59)))
(def-sassy-op divpd      (gen-sse-ps/pd '(#x66 #x0f #x5e)))
(def-sassy-op sqrtpd     (gen-sse-ps/pd '(#x66 #x0f #x51)))
(def-sassy-op maxpd      (gen-sse-ps/pd '(#x66 #x0f #x5f)))
(def-sassy-op minpd      (gen-sse-ps/pd '(#x66 #x0f #x5d)))
(def-sassy-op andpd      (gen-sse-ps/pd '(#x66 #x0f #x54)))
(def-sassy-op andnpd     (gen-sse-ps/pd '(#x66 #x0f #x55)))
(def-sassy-op orpd       (gen-sse-ps/pd '(#x66 #x0f #x56)))
(def-sassy-op xorpd      (gen-sse-ps/pd '(#x66 #x0f #x57)))
(def-sassy-op unpckhpd   (gen-sse-ps/pd '(#x66 #x0f #x15)))
(def-sassy-op unpcklpd   (gen-sse-ps/pd '(#x66 #x0f #x14)))
(def-sassy-op cvtpd2dq   (gen-sse-ps/pd '(#xf2 #x0f #xe6)))
(def-sassy-op cvttpd2dq  (gen-sse-ps/pd '(#x66 #x0f #xe6)))
(def-sassy-op cvtdq2ps   (gen-sse-ps/pd '(#x0f #x5b)))
(def-sassy-op cvtps2dq   (gen-sse-ps/pd '(#x66 #x0f #x5b)))
(def-sassy-op cvttps2dq  (gen-sse-ps/pd '(#xf3 #x0f #x5b)))
(def-sassy-op cvtpd2ps   (gen-sse-ps/pd '(#x66 #x0f #x5a)))
(def-sassy-op punpckhqdq (gen-sse-ps/pd '(#x66 #x0f #x6d)))
(def-sassy-op punpcklqdq (gen-sse-ps/pd '(#x66 #x0f #x6c)))
(def-sassy-op addsubps   (gen-sse-ps/pd '(#xf2 #x0f #xd0)))
(def-sassy-op addsubpd   (gen-sse-ps/pd '(#x66 #x0f #xd0)))
(def-sassy-op haddps     (gen-sse-ps/pd '(#xf2 #x0f #x7c)))
(def-sassy-op hsubps     (gen-sse-ps/pd '(#xf2 #x0f #x7d)))
(def-sassy-op haddpd     (gen-sse-ps/pd '(#x66 #x0f #x7c)))
(def-sassy-op hsubpd     (gen-sse-ps/pd '(#x66 #x0f #x7d)))
(def-sassy-op movshdup   (gen-sse-ps/pd '(#xf3 #x0f #x16)))
(def-sassy-op movsldup   (gen-sse-ps/pd '(#xf3 #x0f #x12)))
(def-sassy-op addss   (gen-sse1-ss '(#xf3 #x0f #x58)))
(def-sassy-op subss   (gen-sse1-ss '(#xf3 #x0f #x5c)))
(def-sassy-op mulss   (gen-sse1-ss '(#xf3 #x0f #x59)))
(def-sassy-op divss   (gen-sse1-ss '(#xf3 #x0f #x5e)))
(def-sassy-op rcpss   (gen-sse1-ss '(#xf3 #x0f #x53)))
(def-sassy-op sqrtss  (gen-sse1-ss '(#xf3 #x0f #x51)))
(def-sassy-op rsqrtss (gen-sse1-ss '(#xf3 #x0f #x52)))
(def-sassy-op maxss   (gen-sse1-ss '(#xf3 #x0f #x5f)))
(def-sassy-op minss   (gen-sse1-ss '(#xf3 #x0f #x5d)))
(def-sassy-op comiss  (gen-sse1-ss '(#x0f #x2f)))
(def-sassy-op ucomiss (gen-sse1-ss '(#x0f #x2e)))
(def-sassy-op movhps (gen-sse1-mov2 '(#x0f #x16) '(#x0f #x17)))
(def-sassy-op movlps (gen-sse1-mov2 '(#x0f #x12) '(#x0f #x13)))
(def-sassy-op movhlps (gen-xmm-r/r '(#x0f #x12)))
(def-sassy-op movlhps (gen-xmm-r/r '(#x0f #x16)))

(def-sassy-op cmpss
  (mini-meta
   (and xmm (or xmm m32) i8
	(lambda (t x y z) (r-r/m-i t 1 '(#xf3 #x0f #xc2) x y z)))))

(def-sassy-op shufps (gen-sse-cmp '(#x0f #xc6)))
(def-sassy-op cmpps  (gen-sse-cmp '(#x0f #xc2)))
(def-sassy-op movmskps (gen-sse-movmsk '(#x0f #x50)))

(def-sassy-op movss
  (mini-meta
   (or (and xmm (or xmm m32)
	    (lambda (t x y) (r-r/m t 1 '(#xf3 #x0f #x10) x y)))
       (and m32 xmm (lambda (t x y) (r/m-r t 1 '(#xf3 #x0f #x11) x y))))))

(def-sassy-op cvtpi2ps (gen-sse-pi2pds '(#x0f #x2a)))
(def-sassy-op cvtsi2ss (gen-sse-si2sssd '(#xf3 #x0f #x2a)))
(def-sassy-op cvtps2pi  (gen-sse1-ps2pi #x2d))
(def-sassy-op cvttps2pi (gen-sse1-ps2pi #x2c))
(def-sassy-op cvttss2si (gen-sse1-ss2si #x2c))
(def-sassy-op cvtss2si  (gen-sse1-ss2si #x2d))
(def-sassy-op ldmxcsr (gen-rm #xae #b010))
(def-sassy-op stmxcsr (gen-rm #xae #b011))

(def-sassy-op pextrw
  (mini-meta
   (and r32
	(or (and mm i8 (lambda (t x y z) (r-r/m-i t 1 '(#x0f #xc5) x y z)))
	    (and xmm i8
		 (lambda (t x y z) (r-r/m-i t 1 '(#x66 #x0f #xc5) x y z)))))))

(def-sassy-op pinsrw
  (mini-meta
   (or (and mm (or r32 m16) i8
	    (lambda (t x y z) (r-r/m-i t 1 '(#x0f #xc4) x y z)))
       (and xmm (or r32 m16) i8
	    (lambda (t x y z) (r-r/m-i t 1 '(#x66 #x0f #xc4) x y z))))))

(def-sassy-op pmovmskb
  (mini-meta
   (and r32
	(or (and mm (lambda (t x y) (r-r/m t 1 '(#x0f #xd7) x y)))
	    (and xmm (lambda (t x y) (r-r/m t 1 '(#x66 #x0f #xd7) x y)))))))

(def-sassy-op pshufw
  (mini-meta
   (and mm (or mm m64) i8
	(lambda (t x y z) (r-r/m-i t 1 '(#x0f #x70) x y z)))))

(def-sassy-op maskmovq
  (mini-meta
   (and mm mm (lambda (t x y) (r-r/m t 1 '(#x0f #xf7) x y)))))

(def-sassy-op movntq
  (mini-meta
   (and m64 mm (lambda (t x y) (r/m-r t 1 '(#x0f #xe7) x y)))))

(def-sassy-op movntps (gen-sse-movnt '(#x0f #x2b)))
(def-sassy-op prefetcht0  (gen-rm8 #x18 #b001))
(def-sassy-op prefetcht1  (gen-rm8 #x18 #b010))
(def-sassy-op prefetcht2  (gen-rm8 #x18 #b011))
(def-sassy-op prefetchnta (gen-rm8 #x18 #b000))
(def-sassy-op sfence (gen-non 1 '(#x0f #xae #xf8)))
(def-sassy-op movapd (gen-sse1-mov  '(#x66 #x0f #x28) '(#x66 #x0f #x29)))
(def-sassy-op movupd (gen-sse1-mov  '(#x66 #x0f #x10) '(#x66 #x0f #x11)))
(def-sassy-op movdqa (gen-sse1-mov '(#x66 #x0f #x6f) '(#x66 #x0f #x7f)))
(def-sassy-op movdqu (gen-sse1-mov '(#xf3 #x0f #x6f) '(#xf3 #x0f #x7f)))
(def-sassy-op movhpd (gen-sse1-mov2 '(#x66 #x0f #x16) '(#x66 #x0f #x17)))
(def-sassy-op movlpd (gen-sse1-mov2 '(#x66 #x0f #x12) '(#x66 #x0f #x13)))
(def-sassy-op movmskpd (gen-sse-movmsk '(#x66 #x0f #x50)))

(def-sassy-op movsd
  (mini-meta
   (or (lambda (t) (just-c t 4 #xa5)) ;string instruction
       (and xmm
	    (or xmm m64)
	    (lambda (t x y) (r-r/m t 1 '(#xf2 #x0f #x10) x y)))
       (and m64 xmm (lambda (t x y) (r/m-r t 1 '(#xf2 #x0f #x11) x y))))))

(def-sassy-op addsd   (gen-sse2-sd '(#xf2 #x0f #x58)))
(def-sassy-op subsd   (gen-sse2-sd '(#xf2 #x0f #x5c)))
(def-sassy-op mulsd   (gen-sse2-sd '(#xf2 #x0f #x59)))
(def-sassy-op divsd   (gen-sse2-sd '(#xf2 #x0f #x5e)))
(def-sassy-op maxsd   (gen-sse2-sd '(#xf2 #x0f #x5f)))
(def-sassy-op minsd   (gen-sse2-sd '(#xf2 #x0f #x5d)))
(def-sassy-op sqrtsd  (gen-sse2-sd '(#xf2 #x0f #x51)))
(def-sassy-op comisd  (gen-sse2-sd '(#x66 #x0f #x2f)))
(def-sassy-op ucomisd (gen-sse2-sd '(#x66 #x0f #x2e)))
(def-sassy-op cvtdq2pd  (gen-sse2-sd '(#xf3 #x0f #xe6)))
(def-sassy-op cvtps2pd  (gen-sse2-sd '(#x0f #x5a)))
(def-sassy-op cvtsd2ss  (gen-sse2-sd '(#xf2 #x0f #x5a)))
(def-sassy-op cmppd   (gen-sse-cmp '(#x66 #x0f #xc2)))
(def-sassy-op shufpd  (gen-sse-cmp '(#x66 #x0f #xc6)))
(def-sassy-op pshuflw (gen-sse-cmp '(#xf2 #x0f #x70)))
(def-sassy-op pshufhw (gen-sse-cmp '(#xf3 #x0f #x70)))
(def-sassy-op pshufd  (gen-sse-cmp '(#x66 #x0f #x70)))

(def-sassy-op cmpsd
  (mini-meta
   (or (lambda (t) (just-c t 4 #xa7)) ;string version
       (and xmm (or xmm m64) i8
	    (lambda (t x y z) (r-r/m-i t 1 '(#xf2 #x0f #xc2) x y z))))))

(def-sassy-op cvttpd2pi (gen-sse-pd2pi '(#x66 #x0f #x2c)))
(def-sassy-op cvtpd2pi  (gen-sse-pd2pi '(#x66 #x0f #x2d)))
(def-sassy-op cvtpi2pd  (gen-sse-pi2pds '(#x66 #x0f #x2a)))
(def-sassy-op cvtss2sd  (gen-sse1-ss '(#xf3 #x0f #x5a)))
(def-sassy-op cvtsd2si  (gen-sse2-sd2si '(#xf2 #x0f #x2d)))
(def-sassy-op cvttsd2si (gen-sse2-sd2si '(#xf2 #x0f #x2c)))
(def-sassy-op cvtsi2sd  (gen-sse-si2sssd '(#xf2 #x0f #x2a)))

(def-sassy-op movq2dq
  (mini-meta
   (and xmm mm (lambda (t x y) (r-r/m t 1 '(#xf3 #x0f #xd6) x y)))))

(def-sassy-op movdq2q
  (mini-meta
   (and mm xmm (lambda (t x y) (r-r/m t 1 '(#xf2 #x0f #xd6) x y)))))

(def-sassy-op pmuludq (gen-mmx-log #xf4))
(def-sassy-op paddq   (gen-mmx-log #xd4))
(def-sassy-op psubq   (gen-mmx-log #xfb))
(def-sassy-op pslldq (gen-sse2-sr #b111))
(def-sassy-op psrldq (gen-sse2-sr #b011))
(def-sassy-op pause (gen-non 1 '(#xf3 #x90)))
(def-sassy-op lfence (gen-non 1 '(#x0f #xae #xe8)))
(def-sassy-op mfence (gen-non 1 '(#x0f #xae #xf0)))
(def-sassy-op clflush (gen-rm8 #xae #b111))
(def-sassy-op maskmovdqu (gen-xmm-r/r '(#x66 #x0f #xf7)))
(def-sassy-op movntpd (gen-sse-movnt '(#x66 #x0f #x2b)))
(def-sassy-op movntdq (gen-sse-movnt '(#x66 #x0f #xe7)))

(def-sassy-op movnti
  (mini-meta
   (and m32 r32 (lambda (t x y) (r/m-r t 1 '(#x0f #xc3) x y)))))

(def-sassy-op fisttp
  (mini-meta
   (or (and m32 (lambda (t x) (r/m t 1 #xdb x #b001)))
       (and m64 (lambda (t x) (r/m t 1 #xdd x #b001)))
       (and m16 (lambda (t x) (r/m t 1 #xdf x #b001))))))

(def-sassy-op lddqu
  (mini-meta
   (and xmm mem-any (lambda (t x y) (r-r/m t 1 '(#xf2 #x0f #xf0) x y)))))

(def-sassy-op movddup (gen-sse2-sd '(#xf2 #x0f #x12)))
(def-sassy-op monitor (gen-non 1 '(#x0f #x01 #xc8)))
(def-sassy-op mwait   (gen-non 1 '(#x0f #x01 #xc9)))

)) ; end (let () ... let () ...

