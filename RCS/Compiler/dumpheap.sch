; -*- Scheme -*-
;
; Scheme 313 Compiler
; Code to dump a bootstrap heap image from un-encoded scheme object files.
;
; $Id: dumpheap.scm,v 1.1 91/08/16 02:33:04 lth Exp Locker: lth $
;
; Each input file consists of a pair, the car of which is a code vector and the
; cdr of which is a constant vector. The code vector is a regular vector,
; although it is treated as a byte vector rather than a word vector. The
; constant vector has all tagged entries (represented using lists), where the
; tags are `data', `codevector', `constantvector', `global', or `bits'.
;
; `Dump-heap' loads its file arguments into the heap, creates thunks from the
; code and constant vectors, and generates an initialization procedure which 
; calls each thunk in turn by traversing the list of thunks. This procedure is
; then installed in the SCHEME_ENTRY root pointer. The thunks are called in
; the order they are loaded.
;
; The Scheme assembler must be co-resident, since it is used by this
; procedure to assemble the final startup code. This could be avoided
; by pre-assembling the code and patching it here, but the way it is now,
; this procedure is entirely portable -- no target dependencies.
;
; Usage: (dump-heap outputfile inputfile ... )

(define dump-heap

  (let ()

    ; Neat constants.

    (define twofiftysix^3 (* 256 256 256))
    (define twofiftysix^2 (* 256 256))
    (define twofiftysix   256)                ; don't ask

    (define largest-fixnum (- (expt 2 29) 1))
    (define smallest-fixnum (- (expt 2 29)))

    (define heap-version 0)

    (define roots
      '(reg0 reg1 reg2 reg3 reg3 reg5 reg6 reg7 reg8 reg9 reg10 reg11 reg12
	reg13 reg14 reg15 reg16 reg17 reg18 reg19 reg20 reg21 reg22 reg23
	reg24 reg25 reg26 reg27 reg28 reg29 reg30 reg31 aregreg2 argreg3
	result continuation saved-result startproc symtab))
    
    ; A heap is represented internally as a vector of three elements,
    ; denoted the `bytes', `globals', and `top'. `Bytes' is a list
    ; of the bytes in the heap (in reverse order). `Globals' is an
    ; assoc list of values to be inserted into the root slots of the
    ; heap. `Top' is the address of the next byte in the heap.

    (define (make-new-heap)
      (vector '() '() 0))

    (define (heap.bytes h) (vector-ref h 0))
    (define (heap.globals h) (vector-ref h 1))
    (define (heap.top h) (vector-ref h 2))

    (define (heap.bytes! h b) (vector-set! h 0 b))
    (define (heap.globals! h g) (vector-set! h 1 g))
    (define (heap.top! h t) (vector-set! h 2 t))

    ; Get the value of a global.

    (define (heap.global h g)
      (let ((x (assq g (heap.globals h))))
	(if x
	    (cdr x)
	    '())))

    ; Set the value of a global.

    (define (heap.global! h g v)
      (let ((x (assq g (heap.globals h))))
	(if x
	    (set-cdr! x v)
	    (heap.globals! h (cons (cons g v) (heap.globals h))))))

    ; Put a byte on the heap.

    (define (heap.byte! h b)
      (heap.bytes! h (cons b (heap.bytes h)))
      (heap.top! h (+ 1 (heap.top h))))

    ; Adjust the heap up to an 8-byte boundary.

    (define (heap.adjust! h)
      (let ((p (heap.top h)))
	(let loop ((i (- (* 8 (quotient (+ p 7) 8)) p)))
	  (if (zero? i)
	      '()
	      (begin (heap.byte! h 0)
		     (loop (- i 1)))))))

    ; Put a word on the heap. Always big-endian.

    (define (heap.word! h w)
      (heap.byte! h (quotient w twofiftysix^3))
      (heap.byte! h (quotient (remainder w twofiftysix^3) twofiftysix^2))
      (heap.byte! h (quotient (remainder w twofiftysix^2) twofiftysix))
      (heap.byte! h (remainder w twofiftysix)))

    ; Procedures for dumping various kinds of data.

    (define (dump-header-word! h immediate length)
      (heap.word! h (+ (* length 256) immediate)))

    ; All data dumpers return tagged pointers (in the form of integers).

    (define (dump-item! h item)
      (case (car item)
	((codevector)
	 (dump-bytevector! h (cadr item) $tag.bytevec-hdrtag))
	((constantvector)
	 (dump-constantvector! h (cadr item)))
	((data)
	 (dump-data! h (cadr item)))
	((global)
	 (dump-global! h (cadr item)))
	((bits)
	 (cadr item))
	(else
	 (error 'dump-item! "Unknown item ~a" item))))

    (define (dump-constantvector! h cv)
      (dump-vector-like! h cv dump-item! $tag.vector-hdrtag))

    ; Only a subset of the data types have been accounted for here.

    (define (dump-data! h datum)
      (cond ((fixnum? datum)
	     (make-fixnum datum))
	    ((integer? datum)
	     (dump-bignum! h datum))
	    ((real? datum)
	     (dump-flonum! h datum))
	    ((char? datum)
	     (make-char datum))
	    ((null? datum)
	     $imm.null)
	    ((eq? datum #t)
	     $imm.true)
	    ((eq? datum #f)
	     $imm.false)
	    ((vector? datum)
	     (dump-vector! h datum $tag.vector-hdrtag))
	    ((bytevector? datum)
	     (dump-bytevector! h datum $tag.bytevec-hdrtag))
	    ((pair? datum)
	     (dump-pair! h datum))
	    ((string? datum)
	     (dump-string! h datum))
	    ((symbol? datum)
	     (dump-symbol! h datum))
	    (else
	     (error 'dump-data! "Unsupported type of datum ~a" datum))))

    (define (fixnum? x)
      (and (integer? x)
	   (<= x largest-fixnum)
	   (>= x smallest-fixnum)))

    (define (make-fixnum f)
      (* 4 f))

    (define (make-char c)
      (+ (* (char->integer c) twofiftysix^2) $imm.character))

    ; misc->bytevector must be provided externally.

    (define (dump-bignum! h b)
      (dump-bytevector! h (bignum->bytevector b) $tag.bignum-hdrtag))

    (define (dump-flonum! h f)
      (dump-bytevector! h (flonum->bytevector b) $tag.flonum-hdrtag))

    (define (dump-string! h s)
      (dump-bytevector! h (string->bytevector b) $tag.string-hdrtag))

    (define (dump-pair! h p)
      (let ((the-car (dump-data! h (car p)))
	    (the-cdr (dump-data! h (cdr p))))
	(let ((base (heap.top h)))
	  (heap.word! h the-car)
	  (heap.word! h the-cdr)
	  (+ base $tag.pair))))

    (define (dump-bytevector! h bv variation)
      (let ((base (heap.top h))
	    (l    (bytevector-length bv)))
	(dump-header-word! h (+ $imm.bytevector-header variation) l)
	(let loop ((i 0))
	  (if (< i l)
	      (begin (heap.byte! h (bytevector-ref bv i))
		     (loop (+ i 1)))
	      (begin (heap.adjust! h)
		     (+ base $tag.bytevector))))))

    (define (dump-vector! h v variation)
      (dump-vector-like! h v dump-data! variation))

    (define (dump-vector-like! h cv recur! variation)
      (let* ((l (vector-length cv))
	     (v (make-vector l '())))
	(let loop ((i 0))
	  (if (< i l)
	      (begin (vector-set! v i (recur! h (vector-ref cv i)))
		     (loop (+ i 1)))
	      (let ((base (heap.top h)))
		(dump-header-word! h (+ $imm.vector-header variation) (* l 4))
		(let loop ((i 0))
		  (if (< i l)
		      (begin (heap.word! h (vector-ref v i))
			     (loop (+ i 1)))
		      (begin (heap.adjust! h)
			     (+ base $tag.vector)))))))))

    ; Symbols and globals have an awful lot in common.
    ;
    ; Currently, we simply maintain a list of the locations of symbols
    ; and value cells in the heap -- no fancy hash table (yet).
    ; The symbol table is a table of triples: symbol, symbol location, and
    ; value cell location. Both of the latter may be null.

    (define symbol-table '())

    (define (symbol-cell s)
      (let ((x (assq s symbol-table)))
	(if (null? x)
	    (let ((p (list s '() '())))
	      (set! symbol-table (cons p symbol-table))
	      p)
	    x)))

    (define (create-symbol! h s)
      (dump-bytevector! h (symbol->bytevector s) $tag.symbol-hdrtag))

    (define (create-cell! h)
      (dump-pair! h (cons '() '())))

    (define (dump-symbol! h s)
      (let ((x (symbol-cell s)))
	(if (null? (cadr x))
	    (set-car! (cdr x) (create-symbol! h s)))
	(cadr x)))

    (define (dump-global! h g)
      (let ((x (symbol-cell g)))
	(if (null? (caddr x))
	    (set-car! (cddr x) (create-cell! h)))
	(caddr x)))

    ; Given a pair of code vector and constant vector, dump a thunk.

    (define (dump-segment! h segment)
      (let* ((the-code   (dump-bytevector! h
					  (car segment)
					  $tag.bytevec-hdrtag))
	     (the-consts (dump-constantvector! h (cdr segment))))
	(let ((base (heap.top h)))
	  (dump-header-word! h $imm.procedure-header 8)
	  (heap.word! h the-code)
	  (heap.word! h the-consts)
	  (heap.adjust! h)
	  (+ base $tag.procedure))))

    ; Given a file name and a heap, load the file into the heap, create a
    ; thunk in the heap of the code and constant vector, and return the
    ; heap pointer to that thunk.

    (define (load-file-into-heap! h filename)
      (display "Loading ") (display filename) (newline)
      (with-input-from-file filename
	(lambda ()
	  (let ((segment (read)))
	    (dump-segment! h segment)))))

    ; Given a heap and a list of heap pointers to thunks, create a thunk
    ; in the heap which runs each thunk in turn. The list is assumed to be
    ; in reverse order when it gets in here. Returns the pointer to the
    ; thunk.

    (define (create-init-proc! h inits)

      ; The initialization procedure.

      (define (init-proc)
	`((,$.proc)
	  (,$args= 0)
	  (,$const (1))
	  (,$setreg 1)
	  (,$.label 0)
	  (,$reg 1)
	  (,$op1 null?)
	  (,$branchf 2)
	  (,$return)
	  (,$.label 2)
	  (,$save 3 1)
	  (,$reg 1)
	  (,$op1 car)
	  (,$invoke 0)
	  (,$.label 3)
	  (,$.cont)
	  (,$restore 1)
	  (,$pop 1)
	  (,$reg 1)
	  (,$op1 cdr)
	  (,$setreg 1)
	  (,$branch 0)))

      ; The car's are all heap pointers, so they should not be messed with.
      ; The cdr must be dumped, and then the pair.

      (define (dump-init-list! h inits)
	(if (null? inits)
	    $imm.null
	    (let ((the-car (car inits))
		  (the-cdr (dump-init-list! h (cdr inits))))
	      (let ((base (heap.top h)))
		(heap.word! h the-car)
		(heap.word! h the-cdr)
		(+ base $tag.pair)))))

      ; Dump the list of init procs, then assemble the thunk which
      ; traverses the list and calls each in turn.

      (display "Assembling final procedure") (newline)
      (let ((l       (dump-init-list! h (reverse inits)))
	    (segment (assemble (init-proc))))
	(vector-set! (cdr segment) 0 `(bits ,l))     ; patch constant vector
	(dump-segment! h segment)))

    ; Write to the output file.

    (define (dump-heap-to-file! h filename)

      (define (write-word w)
	(display (integer->char (quotient w twofiftysix^3)))
	(display (integer->char (quotient (remainder w twofiftysix^3) 
					  twofiftysix^2)))
	(display (integer->char (quotient (remainder w twofiftysix^2) 
					  twofiftysix)))
	(display (integer->char (remainder w twofiftysix))))


      (define (write-version-number)
	(write-word heap-version))

      (define (write-global x globals)
	(let ((q (assq x globals)))
	  (if q
	      (write-word (cdr q))
	      (write-word $imm.false))))

      (define (write-globals globals)
	(for-each
	 (lambda (x) (write-global x globals))
	 roots))

      (define (write-bytes bytes)
	(for-each
	  (lambda (x)
	    (display (integer->char x)))
	  bytes))

      (display "Dumping to file") (newline)
      (with-output-to-file filename
	(lambda ()
	  (write-version-number)
	  (write-globals (heap.globals h))
	  (write-word (quotient (length (heap.bytes h)) 4))
	  (write-bytes (reverse (heap.bytes h))))))

    ; Main loop.

    (define (build-bootstrap-heap outputfile . inputfiles)
      (delete-file outputfile)
      (let ((heap (make-new-heap)))
	(let loop ((files inputfiles) (inits '()))
	  (if (not (null? files))
	      (loop (cdr files)
		    (cons (load-file-into-heap! heap (car files)) inits))
	      (begin (heap.global! heap
				   'startproc
				   (create-init-proc! heap inits))
		     (dump-heap-to-file! heap outputfile))))))

    build-bootstrap-heap))
