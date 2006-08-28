	;=========================;
        ; 			  ;
	; Sassy test suite	  ;
	; 			  ;
        ;=========================;

; The three files generate-nasm.scm, generate-prims.scm, and
; generate-direc.scm generate the baselines for these tests. Of the
; three, only generate-nasm.scm should really ever be run again unless
; Sassy's language actually changes, since the corectness of the output
; for generate-prims and generate-direc has to be verified by hand.

; (generate-nasm generates nasm versions of the Sassy code and assembles
; them by calling nasm -f bin ??. For mzscheme)

(define %%%include-test '((entry foo)))

(define (sassy-run-tests . args)

  (define print #t)

  (define (sassy-symbol-get r n)
    (hash-table-ref (sassy-symbol-table r) n))

  (define (sassy-reloc->list reloc)
    (list (sassy-reloc-name reloc)
	  (sassy-reloc-section reloc)
	  (sassy-reloc-offset reloc)
	  (sassy-reloc-type reloc)))

  (define (file-chars->list file)
    (with-input-from-file file
      (lambda ()
	(let iter ((new (read-byte)))
	  (if (eof-object? new)
	      '()
	      (cons new (iter (read-byte))))))))

  (define (l->hex lst)
    (map (lambda (x)
	   (number->string x 16))
	 lst))

  (define (match-lists subl longl)
    (let iter ((rs subl)
	       (rl longl))
      (cond ((null? rs) rl)
	    ((= (car rs) (car rl))
	     (iter (cdr rs) (cdr rl)))
	    (else (newline)
		  (display (l->hex subl))
		  (newline)
		  (display (l->hex longl))
		  (newline)
		  #f))))

  (define (match-opcodes l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  ((= (car l1) (car l2)) (match-opcodes (cdr l1) (cdr l2)))
	    ;be forgiving about prefixes
	  ((and (not (null? (cdr l1)))
		(not (null? (cdr l2)))
		(or (and (= (car l1) #x66) (= (cadr l1) #x67)
			 (= (car l2) #x67) (= (cadr l2) #x66))
		    (and (= (car l1) #x67) (= (cadr l1) #x66)
			 (= (car l2) #x66) (= (cadr l2) #x67))))
	   (match-opcodes (cddr l1) (cddr l2)))
	  (else #f)))
	  

  (define (sassy-raw c bits32?)
    (sassy-text-list
     (sassy (if bits32?
		`((text (label foo)
			(begin ,@c)))
		`((bits 16)
		  (text (label foo)
			(begin ,@c)))))))

  (define (succeed x)
    (if print
	(begin (display "test passed: ")
	       (display x)
	       (newline))
	#t))

  (define (fail x)
    (if print
	(begin (display "TEST FAILED: ")
	       (display x)
	       (newline))
	#t))


;============================================================================;
; opcode-test								     ;
; ================							     ;
; The opcode tests are fairly exhaustive. Every opcode is tested, and	     ;
; every possible successful parse, whether the opcode(s) use a "gen-"	     ;
; template or has its own. The output is compared to NASM's.		     ;
;============================================================================;
  (define (opcode-test bits32?)
    (for-each
     (lambda (x)
       (let* ((the-codes (with-input-from-file x (lambda () (read))))
	      (sassy-res (sassy-raw the-codes bits32?))
	      (nasm-res (file-chars->list
			 (string-append
			  (substring x 0 (- (string-length x) 4))
			  (if bits32? "" "16")))))
	 (if (match-opcodes nasm-res sassy-res)
	     (succeed (if bits32? x (string-append "(16 bit) " x)))
	     (begin (fail (if bits32? x (string-append "(16 bit) " x)))
		    (let iter ((codes the-codes))
		      (let ((foo (match-lists
				  (sassy-raw (list (car codes)) bits32?)
				  nasm-res)))
			(if foo
			    (begin (set! nasm-res foo)
				   (iter (cdr codes)))
			    (begin (display (car codes))
				   (newline)))))
		    (error "")))))
     (if bits32?
	 the-opcode-files
	 the-opcode16-files)))

  (define the-opcode-files
    (list "tests/mem-ref.scm"
	  "tests/non.scm"
	  "tests/alu.scm"
	  "tests/bt.scm"
	  "tests/shift.scm"
	  "tests/setcc.scm"
	  "tests/cmovcc.scm"
	  "tests/decinc.scm"
	  "tests/plier.scm"
	  "tests/load.scm"
	  "tests/movx.scm"
	  "tests/r-rm.scm"
	  "tests/rm.scm"
	  "tests/rm2.scm"
	  "tests/aa.scm"
	  "tests/ret.scm"
	  "tests/doub-shift.scm"
	  "tests/cmpx.scm"
	  "tests/misc1.scm"
	  "tests/misc2.scm"
	  "tests/misc3.scm"
	  "tests/jcc.scm"
 	  "tests/jumps.scm"
	  "tests/prefix.scm"
	  "tests/fp0.scm"
	  "tests/fp1.scm"
	  "tests/fp2.scm"
	  "tests/fp3.scm"
	  "tests/mmx.scm"
	  "tests/sse1.scm"
	  "tests/sse2.scm"
	  "tests/sse3.scm"
	  "tests/seg.scm"
	  "tests/brt.scm"
	  ))

  (define the-opcode16-files
    (list
     "tests/alu.scm"
     "tests/bt.scm"
     "tests/cmpx.scm"
     "tests/decinc.scm"
     "tests/doub-shift.scm"
     "tests/jcc.scm"
     "tests/jumps.scm"
     "tests/load.scm"
     "tests/mem-ref.scm"
     "tests/misc1.scm"
     "tests/misc2.scm"
     "tests/movx.scm"
     "tests/non.scm"
     "tests/plier.scm"
     "tests/prefix.scm"
     "tests/ret.scm"
     "tests/rm2.scm"
     "tests/rm.scm"
     "tests/r-rm.scm"
     "tests/setcc.scm"
     "tests/shift.scm"
     "tests/seg.scm"))


;================================;
; direc-test			 ;
; ==========			 ;
; The basic functionality	 ;
;================================;

  (define (direc-test)
    (test-export)
    (test-import)
    (test-heap)
    (test-misc)
    (test-data)
    (test-data-locals)
    (test-data-reloc))

  (define (test-export)
    (let ((o (sassy '((export foo quux)
		      (text (label bar (pop eax))
			    (label wizo (push eax)))
		      (export wizo)))))
      (or (and (eqv? 'export (sassy-symbol-scope (sassy-symbol-get o 'foo)))
	       (eqv? 'export (sassy-symbol-scope (sassy-symbol-get o 'quux)))
	       (eqv? 'export (sassy-symbol-scope (sassy-symbol-get o 'wizo)))
	       (eqv? 'local  (sassy-symbol-scope (sassy-symbol-get o 'bar)))
	       (succeed "exports"))
	  (fail "exports"))))

  (define (test-import)
    (let ((o (sassy '((export bar)
		      (import wizo)
		      (text (label bar (pop eax))
			    (label foo (push edx)))
		      (import qadr)))))
      (or (and (eqv? 'export (sassy-symbol-scope (sassy-symbol-get o 'bar)))
	       (eqv? 'import (sassy-symbol-scope (sassy-symbol-get o 'wizo)))
	       (eqv? 'import (sassy-symbol-scope (sassy-symbol-get o 'qadr)))
	       (eqv? 'local  (sassy-symbol-scope (sassy-symbol-get o 'foo)))
	       (succeed "imports"))
	  (fail "imports"))))

  (define (test-heap)
    (let ((o (sassy '((heap (align 128)
			    (label foo (bytes 5))
			    (words 32)
			    (align 16)
			    (label bar (dwords 100)))))))
      (or (and (= 480 (sassy-heap-size o))
	       (= 128 (sassy-heap-align o))
	       (let ((foo-s (sassy-symbol-get o 'foo)))
		 (and (= 0 (sassy-symbol-offset foo-s))
		      (= 5 (sassy-symbol-size   foo-s))))
	       (let ((bar-s (sassy-symbol-get o 'bar)))
		 (and (= 80 (sassy-symbol-offset bar-s))
		      (= 400 (sassy-symbol-size bar-s))))
	       (succeed "heap"))
	  (fail "heap"))))

  (define (test-misc)
    (let ((o (sassy '((org 1000)
		      (include %%%include-test "tests/include.scm")))))
      (succeed "include")
      (and (or (and (eqv? 'foo (sassy-entry-point o))
		    (succeed "entry"))
	       (fail "entry"))
	   (or (and (= 1000 (sassy-text-org o))
		    (= 1024 (sassy-symbol-offset (sassy-symbol-get o 'foo)))
		    (succeed "org"))
	       (fail "org"))
	   (or (and (= 32 (sassy-text-align o))
		    (equal? (sassy-text-list o)
			    (append (make-list 24 #x90) (list 80)))
		    (succeed "text-align"))
	       (fail "text-align")))))

  (define (test-data)
    (let ((o (sassy '((data (label foo (dwords "ab"))
			    (align 8)
			    (label bar (dwords 100 quux))
			    (dwords -3242.52)
			    (qwords -84930284902.48392048)
			    (label quux (dwords -1 #\A bar)))))))
      (and (or (and (= 40 (sassy-data-size o))
		    (= 8  (sassy-data-align o))
		    (succeed "data-align"))
	       (fail "data-align"))
	   (or (and (= 4  (sassy-symbol-size (sassy-symbol-get o 'foo)))
		    (= 0  (sassy-symbol-offset (sassy-symbol-get o 'foo)))
		    (= 8  (sassy-symbol-size (sassy-symbol-get o 'bar)))
		    (= 8  (sassy-symbol-offset (sassy-symbol-get o 'bar)))
		    (= 12 (sassy-symbol-size (sassy-symbol-get o 'quux)))
		    (= 28 (sassy-symbol-offset (sassy-symbol-get o 'quux)))
		    (equal? (sassy-data-list o)
			    '(97 98 0 0 0 0 0 0 100 0 0 0 28 0 0 0 82
				 168 74 197 226 123 102 77 61 198 51
				 194 255 255 255 255 65 0 0 0 8 0 0 0))
		    (succeed "data"))
	       (fail "data")))))

  (define (test-data-locals)
    (let ((o (sassy '((data
		       (dwords 0)
		       (label foo (dwords "abcd" "efgh"))
		       (locals (foo)
			       (label foo
				      (dwords #xeeeeeeee #xffffffff)
				      (dwords foo)))
		       (dwords foo))))))
      (if (equal? (sassy-data-list o)
		  '(0 0 0 0 97 98 99 100 101 102 103 104 238 238 238 238 255
		      255 255 255 12 0 0 0 4 0 0 0))
	  (succeed "data-locals")
	  (fail "data-locals"))))

  (define (test-data-reloc)
    (let ((o (sassy '((data (dwords 100 (reloc abs $here 8))
			    (label foo (dwords (reloc abs $here)
					       (reloc blah quux))))
		      (text
		       (begin (push eax)
			      (nop)
			      (nop)
			      (nop))
		       (label quux (push edx)))))))
      (or (and (equal? (sassy-data-list o)
		       '(100 0 0 0 12 0 0 0 8 0 0 0 4 0 0 0))
	       (equal? '(quux data 12 blah)
		       (sassy-reloc->list (car (sassy-reloc-list o))))
	       (equal? '(#f data 8 abs)
		       (sassy-reloc->list (cadr (sassy-reloc-list o))))
	       (equal? '(#f data 4 abs)
		       (sassy-reloc->list (caddr (sassy-reloc-list o))))
	       (succeed "data-reloc"))
	  (fail "data-reloc"))))

;============================================================================;
; prim-test								     ;
; =========								     ;
; A series of several short tests of some probably (hopefully) common	     ;
; usage idioms of the primitives.					     ;
;============================================================================;
  (define (prim-test)
    (for-each
     (lambda (x)
       (let* ((goal (file-chars->list
		     (substring x 0 (- (string-length x) 4))))
	      (source (sassy-text-list (sassy x))))
	 (if (equal? goal source)
	     (succeed x)
	     (begin (fail x)
		    (error "")))))
     the-prim-files))

  (define the-prim-files
    (list "tests/prims/seq1.scm"
	  "tests/prims/seq2.scm"
	  "tests/prims/seq3.scm"
	  "tests/prims/alt1.scm"
	  "tests/prims/alt2.scm"
	  "tests/prims/alt3.scm"
	  "tests/prims/alt4.scm"
	  "tests/prims/begin1.scm"
	  "tests/prims/begin2.scm"
	  "tests/prims/begin3.scm"
	  "tests/prims/begin4.scm"
	  "tests/prims/begin5.scm"
	  "tests/prims/if1.scm"
	  "tests/prims/if2.scm"
	  "tests/prims/if3.scm"
	  "tests/prims/if4.scm"
	  "tests/prims/inv1.scm"
	  "tests/prims/inv2.scm"
	  "tests/prims/inv3.scm"
	  "tests/prims/inv4.scm"
	  "tests/prims/inv5.scm"
	  "tests/prims/inv6.scm"
	  "tests/prims/iter1.scm"
	  "tests/prims/iter2.scm"
	  "tests/prims/iter3.scm"
	  "tests/prims/iter4.scm"
	  "tests/prims/iter5.scm"
	  "tests/prims/iter6.scm"
	  "tests/prims/leap-mark1.scm"
	  "tests/prims/leap-mark2.scm"
	  "tests/prims/leap-mark3.scm"
	  "tests/prims/while1.scm"
	  "tests/prims/while2.scm"
	  "tests/prims/while3.scm"
	  "tests/prims/with-win1.scm"
	  "tests/prims/with-win2.scm"
	  "tests/prims/with-win3.scm"
	  "tests/prims/with-win4.scm"
	  "tests/prims/with-win5.scm"
	  "tests/prims/with-lose1.scm"
	  "tests/prims/with-lose2.scm"
	  "tests/prims/with-lose3.scm"
	  "tests/prims/with-win-lose1.scm"
	  "tests/prims/with-win-lose2.scm"
	  "tests/prims/with-win-lose3.scm"
	  "tests/prims/with-win-lose4.scm"
	  "tests/prims/with-win-lose5.scm"
	  "tests/prims/exp-k1.scm"
	  "tests/prims/exp-k2.scm"
	  "tests/prims/exp-k3.scm"
	  "tests/prims/exp-k4.scm"
	  "tests/prims/esc1.scm"
	  "tests/prims/esc2.scm"
	  "tests/prims/esc3.scm"
	  "tests/prims/esc4.scm"
	  "tests/prims/esc5.scm"
	  "tests/prims/esc6.scm"
	  "tests/prims/esc7.scm"
	  "tests/prims/label1.scm"
	  "tests/prims/label2.scm"
	  "tests/prims/label3.scm"
	  "tests/prims/label4.scm"
	  "tests/prims/locals1.scm"
	  "tests/prims/locals2.scm"
	  "tests/prims/locals3.scm"
	  "tests/prims/locals4.scm"
	  "tests/prims/locals5.scm"
	  "tests/prims/locals6.scm"
	  "tests/prims/locals7.scm"
	  "tests/prims/locals8.scm"


	  "tests/prims16/16alt1.scm"
	  "tests/prims16/16alt2.scm"
	  "tests/prims16/16alt3.scm"
	  "tests/prims16/16alt4.scm"
	  "tests/prims16/16begin1.scm"
	  "tests/prims16/16begin2.scm"
	  "tests/prims16/16begin3.scm"
	  "tests/prims16/16begin4.scm"
	  "tests/prims16/16begin5.scm"
	  "tests/prims16/16if1.scm"
	  "tests/prims16/16if2.scm"
	  "tests/prims16/16if3.scm"
	  "tests/prims16/16if4.scm"
	  "tests/prims16/16inv1.scm"
	  "tests/prims16/16inv2.scm"
	  "tests/prims16/16inv3.scm"
	  "tests/prims16/16inv4.scm"
	  "tests/prims16/16inv5.scm"
	  "tests/prims16/16inv6.scm"
	  "tests/prims16/16iter1.scm"
	  "tests/prims16/16iter2.scm"
	  "tests/prims16/16iter3.scm"
	  "tests/prims16/16iter4.scm"
	  "tests/prims16/16iter5.scm"
	  "tests/prims16/16iter6.scm"
	  "tests/prims16/16exp-k1.scm"
	  "tests/prims16/16exp-k2.scm"
	  "tests/prims16/16exp-k3.scm"
	  "tests/prims16/16exp-k4.scm"
	  "tests/prims16/16label1.scm"
	  "tests/prims16/16label2.scm"
	  "tests/prims16/16label3.scm"
	  "tests/prims16/16label4.scm"
	  "tests/prims16/16leap-mark1.scm"
	  "tests/prims16/16leap-mark2.scm"
	  "tests/prims16/16leap-mark3.scm"
	  "tests/prims16/16locals1.scm"
	  "tests/prims16/16locals2.scm"
	  "tests/prims16/16locals3.scm"
	  "tests/prims16/16locals4.scm"
	  "tests/prims16/16locals5.scm"
	  "tests/prims16/16locals6.scm"
	  "tests/prims16/16locals7.scm"
	  "tests/prims16/16locals8.scm"
	  "tests/prims16/16seq1.scm"
	  "tests/prims16/16seq2.scm"
	  "tests/prims16/16seq3.scm"
	  "tests/prims16/16while1.scm"
	  "tests/prims16/16while2.scm"
	  "tests/prims16/16while3.scm"
	  "tests/prims16/16with-lose1.scm"
	  "tests/prims16/16with-lose2.scm"
	  "tests/prims16/16with-lose3.scm"
	  "tests/prims16/16with-win1.scm"
	  "tests/prims16/16with-win2.scm"
	  "tests/prims16/16with-win3.scm"
	  "tests/prims16/16with-win4.scm"
	  "tests/prims16/16with-win5.scm"
	  "tests/prims16/16with-win-lose1.scm"
	  "tests/prims16/16with-win-lose2.scm"
	  "tests/prims16/16with-win-lose3.scm"
	  "tests/prims16/16with-win-lose4.scm"
	  "tests/prims16/16with-win-lose5.scm"

	  ))

;============================================================================;
; elf-test								     ;
; =========								     ;
; A series of several short tests of some probably (hopefully) common	     ;
; usage idioms of the primitives.					     ;
;============================================================================;
  (define (elf-test)
    (for-each
     (lambda (x)
       (let* ((source-name
	      (string-append
	       (substring x 0 (- (string-length x) 4))
	       ".new.o"))
	      (goal (file-chars->list
		     (string-append
		      (substring x 0 (- (string-length x) 4))
		      ".o")))
	      (source (begin (sassy-make-elf source-name
					     (sassy x))
			     (file-chars->list source-name))))
	 (if (equal? goal source)
	     (succeed x)
	     (begin (fail x)
		    (error "")))))
     the-elf-files))

  (define the-elf-files
    (list "tests/sysexit.scm"
	  "tests/fac5.scm"
	  "tests/cell.scm"

          "tests/sysexit2.scm" ; static linking
          "tests/count.scm"

	  "tests/libhello.scm" ; dynamic linking
	  "tests/libgoodbye.scm"
	  "tests/hello.scm"
	  "tests/bye.scm"

	  "tests/localdata1.scm"
	  "tests/localdata2.scm"
	  "tests/localdata3.scm"
	  "tests/localdata4.scm"

	  "tests/sect.scm")) ; sections of anon relocs

  (define (build-opcodes)
    (let iter ((rst the-opcode-files))
      (if (null? rst)
	  '()
	  (let ((next (with-input-from-file (car rst) (lambda () (read)))))
	    (cons `(text ,@next) (iter (cdr rst)))))))

  (define result #t)


  (if (memq 'silent args) (set! print #f))
  (do ((r args (cdr r)))
      ((null? r) result)
    (case (car r)
      ((all) (begin (opcode-test #t)
		    (opcode-test #f)
		    (prim-test)
		    (direc-test)
		    (elf-test)))
      ((opcodes)   (opcode-test #t))
      ((opcodes16) (opcode-test #f))
      ((prims)   (prim-test))
      ((direcs)  (direc-test))
      ((silent)  #t)
      ((elf)     (elf-test))
      ((build-opcodes) (set! result (build-opcodes))))))



