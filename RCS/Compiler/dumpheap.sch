; -*- Scheme -*-
;
; Scheme 313 compiler
; Code to dump a bootstrap heap image from un-encoded scheme object files.
;
; Second major version.
;
; $Id: dumpheap.sch,v 1.8 92/02/23 16:55:50 lth Exp Locker: lth $
;
; Each input file consists of pairs. The car of a pair is a code vector
; and the cdr of the pair is a constant vector. The code vector is a regular
; vector, although it is treated as a byte vector rather than a word vector.
; The constant vector has all tagged entries (represented using lists), where
; the tags are `data', `codevector', `constantvector', `global', or `bits'.
;
; `Dump-heap' loads its file arguments into the heap, creates thunks from the
; code and constant vectors, and creates a list of the thunks. It also creates
; a list of all symbols used in the loaded files. Finally, it generates an
; initialization procedure (the LAP of which is hardcoded into this file; see
; below). A pointer to this procedure is installed in the SCHEME_ENTRY root
; pointer; hence, this procedure (a thunk, as it were) is called when the heap
; image is loaded.
;
; The initialization procedure calls each procedure in the thunk list in order.
; It then invokes the procedure ``go'', which takes one argument: the list of
; symbols. Typically, ``go'' will initialize the symbol table and then call
; the REP loop, but this is by no means required.
;
; The Scheme assembler must be co-resident, since it is used by this
; procedure to assemble the final startup code. This could be avoided
; by pre-assembling the code and patching it here, but the way it is now,
; this procedure is entirely portable -- no target dependencies.
;
; Usage: (dump-heap outputfile inputfile ... )

; When this switch is set, the cdr of the global value cell is initialized
; to the symbol which has the same printname as the name of the cell.
; Otherwise, the cdr of the global value cell is given an ordinal number
; which can be found later in the map file.

(define generate-global-symbols? #f)

;

(define dump-heap

  (let ()

    (define **unspecified** (string->symbol "**UNSPECIFIED**"))
    (define **eof** (string->symbol "**EOF**"))

    ; Neat constants.

    (define twofiftysix^3 (* 256 256 256))
    (define twofiftysix^2 (* 256 256))
    (define twofiftysix   256)                ; don't ask

    (define largest-fixnum (- (expt 2 29) 1))
    (define smallest-fixnum (- (expt 2 29)))

    (define heap-version 7)

    (define roots
      '(reg0 reg1 reg2 reg3 reg3 reg5 reg6 reg7 reg8 reg9 reg10 reg11 reg12
	reg13 reg14 reg15 reg16 reg17 reg18 reg19 reg20 reg21 reg22 reg23
	reg24 reg25 reg26 reg27 reg28 reg29 reg30 reg31 argreg2 argreg3
	result continuation scheme-entry millicode-support saved-result 
        glue-tmp1 glue-tmp2 glue-tmp3 glue-tmp4 gen-tmp1 gen-tmp2 mem-tmp1 mem-tmp2))
    
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

    (define make-global cons)
    (define global.value cadr)
    (define (global.value! g v) (set-car! (cdr g) v))

    ; Get the value of a global.

    (define (heap.global h g)
      (let ((x (assq g (heap.globals h))))
	(if x
	    (global.value x)
	    '())))

    ; Set the value of a global.

    (define (heap.global! h g v)
      (let ((x (assq g (heap.globals h))))
	(if x
	    (global.value! x v)
	    (heap.globals! h (cons (make-global g v) (heap.globals h))))))

    ; Put a byte on the heap.

    (define (heap.byte! h b)
      (if (inexact? b)
	  (error 'heap.byte! "Boinga!"))
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
	 (dump-bytevector! h (cadr item) $tag.bytevector-typetag))
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
      (dump-vector-like! h cv dump-item! $tag.vector-typetag))

    ; Only a subset of the data types have been accounted for here.

    (define (dump-data! h datum)
      (cond ((fixnum? datum)
	     (make-fixnum datum))
	    ((bignum? datum)
	     (dump-bignum! h datum))
	    ((ratnum? datum)
	     (dump-ratnum! h datum))
	    ((flonum? datum)
	     (dump-flonum! h datum))
	    ((compnum? datum)
	     (dump-compnum! h datum))
	    ((rectnum? datum)
	     (dump-rectnum! h datum))
	    ((char? datum)
	     (make-char datum))
	    ((null? datum)
	     $imm.null)
	    ((eq? datum #t)
	     $imm.true)
	    ((eq? datum #f)
	     $imm.false)
	    ((eq? datum **unspecified**)
	     $imm.unspecified)
	    ((eq? datum **eof**)
	     $imm.eof)
	    ((vector? datum)
	     (dump-vector! h datum $tag.vector-typetag))
	    ((bytevector? datum)
	     (dump-bytevector! h datum $tag.bytevector-typetag))
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
	   (exact? x)
	   (<= x largest-fixnum)
	   (>= x smallest-fixnum)))

    (define (bignum? x)
      (and (integer? x)
	   (exact? x)
	   (or (> x largest-fixnum)
	       (< x smallest-fixnum))))

    (define (ratnum? x)
      (and (rational? x)
	   (exact? x)
	   (not (integer? x))))

    (define (flonum? x)
      (and (real? x)
	   (inexact? x)))

    (define (compnum? x)
      (and (complex? x)
	   (inexact? x)
	   (not (real? x))))

    (define (rectnum? x)
      (and (complex? x)
	   (exact? x)
	   (not (real? x))))

    ; returns the two's complement representation as a positive number.

    (define (make-fixnum f)
      (if (negative? f)
	  (- #x100000000 (* (abs f) 4))
	  (* 4 f)))

    (define (make-char c)
      (+ (* (char->integer c) twofiftysix^2) $imm.character))

    ; misc->bytevector must be provided externally.

    (define (dump-bignum! h b)
      (dump-bytevector! h (bignum->bytevector b) $tag.bignum-typetag))

    (define (dump-ratnum! h r)
      (dump-vector! h 
		    (vector (numerator r) (denominator r)) 
		    $tag.ratnum-typetag))

    (define (dump-flonum! h f)
      (dump-bytevector! h (flonum->bytevector f) $tag.flonum-typetag))

    (define (dump-compnum! h c)
      (dump-bytevector! h (compnum->bytevector c) $tag.compnum-typetag))

    (define (dump-rectnum! h r)
      (dump-vector! h
		    (vector (real-part r) (imag-part r))
		    $tag.rectnum-typetag))

    (define (dump-string! h s)
      (dump-bytevector! h (string->bytevector s) $tag.string-typetag))

    (define (dump-pair! h p)
      (let ((the-car (dump-data! h (car p)))
	    (the-cdr (dump-data! h (cdr p))))
	(let ((base (heap.top h)))
	  (heap.word! h the-car)
	  (heap.word! h the-cdr)
	  (+ base $tag.pair-tag))))

    (define (dump-bytevector! h bv variation)
      (let ((base (heap.top h))
	    (l    (bytevector-length bv)))
	(dump-header-word! h (+ $imm.bytevector-header variation) l)
	(let loop ((i 0))
	  (if (< i l)
	      (begin (heap.byte! h (bytevector-ref bv i))
		     (loop (+ i 1)))
	      (begin (heap.adjust! h)
		     (+ base $tag.bytevector-tag))))))

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
			     (+ base $tag.vector-tag)))))))))

    ; Symbols and globals have an awful lot in common.
    ;
    ; Currently, we simply maintain a list of the locations of symbols
    ; and value cells in the heap -- no fancy hash table (yet).
    ; The symbol table is a table of quadruples: symbol, symbol location, 
    ; value cell location, and the value cell ordinal number. All of the
    ; last three may be null.

    (define symbol-table '())
    (define cell-number 0)

    (define (make-symcell s)
      (list s '() '() '()))

    (define symcell.name car)                   ; name
    (define symcell.symloc cadr)                ; symbol location (if any)
    (define symcell.valloc caddr)               ; value cell location (ditto)
    (define symcell.valno cadddr)               ; value cell number (ditto)

    (define (symcell.symloc! x y) (set-car! (cdr x) y))
    (define (symcell.valloc! x y) (set-car! (cddr x) y))
    (define (symcell.valno! x y) (set-car! (cdddr x) y))

    ; Find a symcell in the table, or make a new one if there's none.

    (define (symbol-cell s)
      (let ((x (assq s symbol-table)))
	(if (not x)
	    (let ((p (make-symcell s)))
	      (set! symbol-table (cons p symbol-table))
	      p)
	    x)))

    ; Return list of symbol locations for symbols in the heap.

    (define (symbol-names)
      (let loop ((t symbol-table) (l '()))
	(if (null? t)
	    (reverse l)
	    (if (not (null? (symcell.symloc (car t))))
		(loop (cdr t) (cons (symcell.symloc (car t)) l))
		(loop (cdr t) l)))))

    ; Return list of variable name to cell number mappings for global vars.

    (define (load-map)
      (let loop ((t symbol-table) (l '()))
	(if (null? t)
	    (reverse l)
	    (if (not (null? (symcell.valloc (car t))))
		(loop (cdr t) (cons (cons (symcell.name (car t))
					  (symcell.valno (car t)))
				    l))
		(loop (cdr t) l)))))

    ; Stuff a new symbol into the heap, return its location.

    (define (create-symbol! h s)
      (dump-vector-like! h 
			 (vector `(bits ,(dump-string! h (symbol->string s)))
				 '(data 0)
				 '(data ()))
			 dump-item!
			 $tag.symbol-typetag))

    ; Stuff a value cell into the heap, return a pair of its location
    ; and its cell number.

    (define (create-cell! h s)
      (let* ((n cell-number)
	     (p (dump-pair! h (cons **unspecified**
				    (if generate-global-symbols?
					s
					n)))))
	(set! cell-number (+ cell-number 1))
	(cons p n)))

    (define (dump-symbol! h s)
      (let ((x (symbol-cell s)))
	(if (null? (symcell.symloc x))
	    (symcell.symloc! x (create-symbol! h s)))
	(symcell.symloc x)))

    (define (dump-global! h g)
      (let ((x (symbol-cell g)))
	(if (null? (symcell.valloc x))
	    (let ((cell (create-cell! h g)))
	      (symcell.valloc! x (car cell))
	      (symcell.valno! x (cdr cell))))
	(symcell.valloc x)))

    ; Given a pair of code vector and constant vector, dump a thunk.

    (define (dump-segment! h segment)
      (let* ((the-code   (dump-bytevector! h
					  (car segment)
					  $tag.bytevector-typetag))
	     (the-consts (dump-constantvector! h (cdr segment))))
	(let ((base (heap.top h)))
	  (dump-header-word! h $imm.procedure-header 8)
	  (heap.word! h the-code)
	  (heap.word! h the-consts)
	  (heap.adjust! h)
	  (+ base $tag.procedure-tag))))

    ; Given a file name and a heap, load the file into the heap, create a
    ; thunk in the heap of the code and constant vector, and return the
    ; heap pointer to that thunk.

    (define (load-file-into-heap! h filename)
      (display "Loading ") (display filename) (newline)
      (with-input-from-file filename
	(lambda ()
	  (let loop ((segment (read)) (thunks '()))
	    (if (eof-object? segment)
		thunks    ; must not reverse here.
		(loop (read) (cons (dump-segment! h segment) thunks)))))))

    ; Given a heap and a list of heap pointers to thunks, create a thunk
    ; in the heap which runs each thunk in turn. The list is assumed to be
    ; in reverse order when it gets in here. Returns the pointer to the
    ; thunk.

    (define (create-init-proc! h inits)

      ; The initialization procedure. The lists are magically patched into
      ; the constant vector after the procedure has been assembled but before
      ; it is dumped into the heap. See below.
      ;
      ; (define (init-proc)
      ;   (let loop ((l <list-of-thunks>))
      ;     (if (null? l)
      ;         (go <list-of-symbols>)
      ;         (begin ((car l))
      ;                (loop (cdr l))))))

      (define (init-proc)
	`((,$.proc)
	  (,$args= 0)
	  (,$const (1))          ; dummy list of thunks.
	  (,$setreg 1)
	  (,$.label 0)
	  (,$reg 1)
	  (,$op1 null?)          ; (null? l)
	  (,$branchf 2)
	  (,$const (2))          ; dummy list of symbols
	  (,$setreg 1)
	  (,$global go)
	  (,$invoke 1)           ; (go <list of symbols>)
	  (,$.label 2)
	  (,$save 3 1)
	  (,$reg 1)
	  (,$op1 car)
	  (,$invoke 0)           ; ((car l))
	  (,$.label 3)
	  (,$.cont)
	  (,$restore 1)
	  (,$pop 1)
	  (,$reg 1)
	  (,$op1 cdr)
	  (,$setreg 1)
	  (,$branch 0)))         ; (loop (cdr l))

      ; The car's are all heap pointers, so they should not be messed with.
      ; The cdr must be dumped, and then the pair.

      (define (dump-list! h inits)
	(if (null? inits)
	    $imm.null
	    (let ((the-car (car inits))
		  (the-cdr (dump-list! h (cdr inits))))
	      (let ((base (heap.top h)))
		(heap.word! h the-car)
		(heap.word! h the-cdr)
		(+ base $tag.pair-tag)))))

      ; Given some value which might appear in the constant vector, 
      ; replace the entries matching that value with a new value.

      (define (patch-constant-vector! v old new)
	(let loop ((i (- (vector-length v) 1)))
	  (if (>= i 0)
	      (begin (if (equal? (vector-ref v i) old)
			 (vector-set! v i new))
		     (loop (- i 1))))))

      ; Dump the list of init procs, then assemble the thunk which
      ; traverses the list and calls each in turn.

      (display "Assembling final procedure") (newline)
      (let ((e enable-singlestep?))
	(set! enable-singlestep? #f)
	(let* ((l       (dump-list! h (reverse inits)))
	       (m       (dump-list! h (symbol-names)))
	       (segment (assemble (init-proc))))
	  (set! enable-singlestep? e)
	  (patch-constant-vector! (cdr segment) '(data (1)) `(bits ,l))
	  (patch-constant-vector! (cdr segment) '(data (2)) `(bits ,m))
	  (dump-segment! h segment))))

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

      ; This is just way obscure. Basically, we can define a global with 
      ; the name of a root, and the root will be initialized to the value 
      ; of that global. See the construct down below in the mainline code.

      (define (write-root root globals)
	(let ((q (assq root globals)))
	  (if q
	      (write-word (cdr q))
	      (write-word $imm.false))))

      (define (write-roots globals)
	(for-each
	 (lambda (x) (write-root x globals))
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
	  (write-roots (heap.globals h))
	  (write-word (quotient (length (heap.bytes h)) 4))
	  (write-bytes (reverse (heap.bytes h))))))

    ; Main loop.

    (define (build-bootstrap-heap outputfile . inputfiles)
      (delete-file outputfile)
      (set! cell-number 0)
      (set! symbol-table '())
      (let ((heap (make-new-heap)))
	(let loop ((files inputfiles) (inits '()))
	  (if (not (null? files))
	      (loop (cdr files)
		    (append (load-file-into-heap! heap (car files)) inits))
	      (begin (heap.global! heap
				   'scheme-entry
				   (create-init-proc! heap inits))
		     (heap.global! heap
				   'millicode-support
				   (dump-global! heap 'millicode-support))
		     (dump-heap-to-file! heap outputfile)
		     (load-map))))))

    build-bootstrap-heap))
