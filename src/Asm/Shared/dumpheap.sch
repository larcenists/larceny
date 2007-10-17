; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Bootstrap heap dumper.
;
; Usage: (build-heap-image outputfile inputfile-list)
;
; Each input file is a sequence of segments, the structure of which 
; depends on the target architecture, but at least segment.code and 
; segment.constants exist as accessors.
;
; The code is a bytevector.  The constant vector contains tagged 
; entries (represented using length-2 lists), where the tags are
; `data', `codevector', `constantvector', `global', or `bits'.
;
; `build-heap-image' reads its file arguments into the heap, creates 
; thunks from the segments, and creates a list of the thunks.  It also 
; creates a list of all symbols present in the loaded files.  Finally, 
; it generates an initialization procedure (the LAP of which is hardcoded
; into this file; see below).  A pointer to this procedure is installed 
; in the SCHEME_ENTRY root pointer; hence, this procedure (a thunk, as 
; it were) is called when the heap image is loaded.
;
; The initialization procedure calls each procedure in the thunk list in 
; order.  It then invokes the procedure `go', which takes one argument:
; the list of symbols.  Typically, `go' will initialize the symbol table
; and other system tables and then call `main', but this is by no means
; required.
;
; The Scheme assembler must be co-resident, since it is used by 
; `build-heap-image' procedure to assemble the final startup code.  This
; could be avoided by pre-assembling the code and patching it here, but 
; the way it is now, this procedure is entirely portable -- no target
; dependencies.
;
; The code is structured to allow most procedures to be overridden for
; target architectures with more complex needs (notably the C backend).

(define generate-global-symbols
  (make-twobit-flag 'generate-global-symbols))
(generate-global-symbols #t)

(define heap.version-number 9)          ; Heap version number

(define heap.root-names                 ; Roots in heap version 9
  '(result argreg2 argreg3 
    reg0 reg1 reg2 reg3 reg3 reg5 reg6 reg7 reg8 reg9 reg10 reg11 reg12
    reg13 reg14 reg15 reg16 reg17 reg18 reg19 reg20 reg21 reg22 reg23
    reg24 reg25 reg26 reg27 reg28 reg29 reg30 reg31 
    cont startup callouts schcall-arg4 alloci-tmp))
    
(define (build-heap-image output-file input-files)

  (define tmp-file "HEAPDATA.dat")

  (define (process-input-files heap)
    (let loop ((files input-files) (inits '()))
      (cond ((null? files)
             (heap.thunks! heap (apply append inits)))
            (else
             (let ((filename (car files)))
               (display "Loading ")
               (display filename)
               (newline)
               (loop (cdr files)
                     (append inits (list (dump-file! heap filename)))))))))

  ; FIXME:  Should use binary i/o here, but that must wait
  ; for MzScheme to implement R6RS i/o.

  (delete-file tmp-file)
  (let ((heap  (make-heap #f (open-raw-latin-1-output-file tmp-file))))
    (before-all-files heap output-file input-files)
    (process-input-files heap)
    (heap.set-root! heap
                    'startup
                    (dump-startup-procedure! heap))
    (heap.set-root! heap
                    'callouts
                    (dump-global! heap 'millicode-support))
    (write-header heap output-file)
    (after-all-files heap output-file input-files)
    (close-output-port (heap.output-port heap))
    (append-file-shell-command tmp-file output-file)
    (load-map heap)
    (unspecified)))

(define (before-all-files heap output-file-name input-file-names) #t)
(define (after-all-files heap output-file-name input-file-names) #t)

; Public
;
; A 'heap' is a data structure with the following public fields; none
; of them are constant unless so annotated:
;
;  version          a fixnum (constant) - heap type version number
;  roots            an assoc list that maps root names to values
;  top              an exact nonnegative integer: the address of the 
;                   next byte to be emitted
;  symbol-table     a symbol table abstract data type
;  extra            any value - a client-extension field
;  output-port      an output port (for the data stream)
;  thunks           a list of codevector addresses
;
; Bytes are emitted with the heap.byte! and heap.word! procedures,
; which emit a byte and a 4-byte word respectively.  These update
; the top field.

(define (make-heap extra output-port)
  (vector heap.version-number        ; version
          '()                        ; roots
          0                          ; top
          (make-heap-symbol-table)   ; symtab
          extra                      ; extra
          output-port                ; output port
          '()                        ; thunks
          ))

(define (heap.version h) (vector-ref h 0))
(define (heap.roots h) (vector-ref h 1))
(define (heap.top h) (vector-ref h 2))
(define (heap.symbol-table h) (vector-ref h 3))
(define (heap.extra h) (vector-ref h 4))
(define (heap.output-port h) (vector-ref h 5))
(define (heap.thunks h) (vector-ref h 6))

(define (heap.roots! h x) (vector-set! h 1 x))
(define (heap.top! h x) (vector-set! h 2 x))
(define (heap.thunks! h x) (vector-set! h 6 x))


; Symbol table.
;
; The symbol table maps names to symbol structures, and a symbol 
; structure contains information about that symbol.
;
; The structure has four fields:
;   name      a symbol - the print name
;   symloc    a fixnum or null - if fixnum, the location in the
;             heap of the symbol structure.
;   valloc    a fixnum or null - if fixnum, the location in the
;             heap of the global variable cell that has this
;             symbol for its name.
;   valno     a fixnum or null - if fixnum, the serial number of
;             the global variable cell (largely obsolete).
;
; Note therefore that the symbol table maintains information about
; whether the symbol is used as a symbol (in a datum), as a global
; variable, or both.

(define (make-heap-symbol-table)
  (vector '() 0))

(define (symtab.symbols st) (vector-ref st 0))
(define (symtab.cell-no st) (vector-ref st 1))

(define (symtab.symbols! st x) (vector-set! st 0 x))
(define (symtab.cell-no! st x) (vector-set! st 1 x))

(define (make-symcell name)
  (vector name '() '() '()))

(define (symcell.name sc) (vector-ref sc 0))    ; name
(define (symcell.symloc sc) (vector-ref sc 1))  ; symbol location (if any)
(define (symcell.valloc sc) (vector-ref sc 2))  ; value cell location (ditto)
(define (symcell.valno sc) (vector-ref sc 3))   ; value cell number (ditto)

(define (symcell.symloc! sc x) (vector-set! sc 1 x))
(define (symcell.valloc! sc x) (vector-set! sc 2 x))
(define (symcell.valno! sc x) (vector-set! sc 3 x))

; Find a symcell in the table, or make a new one if there's none.

(define (symbol-cell h name)
  (let ((symtab (heap.symbol-table h)))
    (let loop ((symbols (symtab.symbols symtab)))
      (cond ((null? symbols)
             (let ((new-sym (make-symcell name)))
               (symtab.symbols! symtab (cons new-sym
                                             (symtab.symbols symtab)))
               new-sym))
            ((eq? name (symcell.name (car symbols)))
             (car symbols))
            (else
             (loop (cdr symbols)))))))


; Fundamental data emitters

(define twofiftysix^3 (* 256 256 256))
(define twofiftysix^2 (* 256 256))
(define twofiftysix   256)

(define (heap.word! h w)
  (write-word w (heap.output-port h))
  (heap.top! h (+ 4 (heap.top h))))

(define (heap.byte! h b)
  (write-byte b (heap.output-port h))
  (heap.top! h (+ 1 (heap.top h))))

(define (write-word-be w out)
  (write-byte (quotient w twofiftysix^3)
              out)
  (write-byte (quotient (remainder w twofiftysix^3) 
                        twofiftysix^2)
              out)
  (write-byte (quotient (remainder w twofiftysix^2) 
                        twofiftysix)
              out)
  (write-byte (remainder w twofiftysix)
              out))

(define (write-word-el w out)
  (write-byte (remainder w twofiftysix)
              out)
  (write-byte (quotient (remainder w twofiftysix^2) 
                        twofiftysix)
              out)
  (write-byte (quotient (remainder w twofiftysix^3) 
                        twofiftysix^2)
              out)
  (write-byte (quotient w twofiftysix^3)
              out))

(define write-word write-word-be)

(define (dumpheap.set-endianness! which)
  (case which
    ((big) (set! write-word write-word-be))
    ((little) (set! write-word write-word-el))
    (else ???)))


; Useful abstractions and constants.

(define (heap.header-word! h immediate length)
  (heap.word! h (+ (* length 256) immediate)))

(define (heap.adjust! h)
  (let ((p (heap.top h)))
    (let loop ((i (- (* 8 (quotient (+ p 7) 8)) p)))
      (if (zero? i)
          '()
          (begin (heap.byte! h 0)
                 (loop (- i 1)))))))
  
(define heap.largest-fixnum (- (expt 2 29) 1))
(define heap.smallest-fixnum (- (expt 2 29)))

(define (heap.set-root! h name value)
  (heap.roots! h (cons (cons name value) (heap.roots h))))


;;; The segment.* procedures may be overridden by custom code.

(define segment.code car)
(define segment.constants cdr)

;;; The dump-*! procedures may be overridden by custom code.

; Load a LOP file into the heap, create a thunk in the heap to hold the
; code and constant vector, and return the list of thunk addresses in
; the order dumped.

(define (dump-file! h filename)
  (call-with-raw-latin-1-input-file filename
    (lambda (in)
      (let loop ((decls '()))
	(let ((item (read in)))
	  (if (string? item)
	      (loop (cons item decls))
	      (begin
		(before-dump-file h filename (reverse decls))
		(do ((segment item (read in))
		     (thunks  '() (cons (dump-segment! h segment) thunks)))
		    ((eof-object? segment)
		     (after-dump-file h filename)
		     (reverse thunks))))))))))

(define (before-dump-file h filename decls) #t)
(define (after-dump-file h filename) #t)

; Dump a segment and return the heap address of the resulting thunk.

(define (dump-segment! h segment)
  (let* ((the-code   (dump-codevector! h (segment.code segment)))
         (the-consts (dump-constantvector! h (segment.constants segment))))
    (dump-thunk! h the-code the-consts)))

(define (dump-tagged-item! h item)
  (case (car item)
    ((codevector)
     (dump-codevector! h (cadr item)))
    ((constantvector)
     (dump-constantvector! h (cadr item)))
    ((data)
     (dump-datum! h (cadr item)))
    ((global)
     (dump-global! h (cadr item)))
    ((bits)
     (cadr item))
    (else
     (error 'dump-tagged-item! "Unknown item ~a" item))))

(define (dump-datum! h datum)

  (define (fixnum? x)
    (and (integer? x)
         (exact? x)
         (<= heap.smallest-fixnum x heap.largest-fixnum)))

  (define (bignum? x)
    (and (integer? x)
         (exact? x)
         (or (> x heap.largest-fixnum)
             (< x heap.smallest-fixnum))))

  (define (ratnum? x)
    (and (rational? x) (exact? x) (not (integer? x))))

  (define (flonum? x)
    (and (real? x) (inexact? x)))

  (define (compnum? x)
    (and (complex? x) (inexact? x) (not (real? x))))

  (define (rectnum? x)
    (and (complex? x) (exact? x) (not (real? x))))

  (cond ((fixnum? datum)
         (dump-fixnum! h datum))
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
         (dump-char! h datum))
        ((null? datum)
         $imm.null)
        ((eq? datum #t)
         $imm.true)
        ((eq? datum #f)
         $imm.false)
        ((equal? datum (unspecified))
         $imm.unspecified)
        ((equal? datum (undefined))
         $imm.undefined)
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
         (error 'dump-datum! "Unsupported type of datum ~a" datum))))

; Returns the two's complement representation as a positive number.

(define (dump-fixnum! h f)
  (if (negative? f)
      (- #x100000000 (* (abs f) 4))
      (* 4 f)))

(define (dump-char! h c)
  (+ (* (char->integer c) twofiftysix^2) $imm.character))

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
  (let ((rep (nbuild-parameter 'target-string-rep)))
    (case rep
     ((flat1)
      (dump-bytevector! h (flat1->bytevector s) $tag.string-typetag))
     ((flat4)
      (let* ((n (string-length s))
             (bv (make-bytevector (* 4 n)))
             (offsets (case (nbuild-parameter 'target-endianness)
                        ((little) '#(3 2 1 0))
                        ((big)    '#(0 1 2 3)))))
        (do ((i 0 (+ i 1))
             (j 0 (+ j 4)))
            ((= i n)
             (dump-bytevector! h bv $tag.ustring-typetag))
          (bytevector-set! bv (+ j (vector-ref offsets 0)) 0)
          (bytevector-set! bv (+ j (vector-ref offsets 1)) 0)
          (bytevector-set! bv (+ j (vector-ref offsets 2)) (char->integer (string-ref s i)))
          (bytevector-set! bv (+ j (vector-ref offsets 3)) $imm.character))))
     (else
      (error 'dump-string! "Unknown string representation: " rep)))))

(define (dump-pair! h p)
  (let ((the-car (dump-datum! h (car p)))
        (the-cdr (dump-datum! h (cdr p))))
    (let ((base (heap.top h)))
      (heap.word! h the-car)
      (heap.word! h the-cdr)
      (+ base $tag.pair-tag))))

(define (dump-bytevector! h bv variation)
  (let ((base (heap.top h))
        (l    (bytevector-length bv)))
    (heap.header-word! h (+ $imm.bytevector-header variation) l)
    (let loop ((i 0))
      (if (< i l)
          (begin (heap.byte! h (bytevector-ref bv i))
                 (loop (+ i 1)))
          (begin (heap.adjust! h)
                 (+ base $tag.bytevector-tag))))))

(define (dump-vector! h v variation)
  (dump-vector-like! h v dump-datum! variation))

(define (dump-vector-like! h cv recur! variation)
  (let* ((l (vector-length cv))
         (v (make-vector l '())))
    (let loop ((i 0))
      (if (< i l)
          (begin (vector-set! v i (recur! h (vector-ref cv i)))
                 (loop (+ i 1)))
          (let ((base (heap.top h)))
            (heap.header-word! h (+ $imm.vector-header variation) (* l 4))
            (let loop ((i 0))
              (if (< i l)
                  (begin (heap.word! h (vector-ref v i))
                         (loop (+ i 1)))
                  (begin (heap.adjust! h)
                         (+ base $tag.vector-tag)))))))))

(define (dump-codevector! h cv)
  (dump-bytevector! h cv $tag.bytevector-typetag))

(define (dump-constantvector! h cv)
  (dump-vector-like! h cv dump-tagged-item! $tag.vector-typetag))

(define (dump-symbol! h s)
  (let ((x (symbol-cell h s)))
    (if (null? (symcell.symloc x))
        (symcell.symloc! x (create-symbol! h s)))
    (symcell.symloc x)))

(define (dump-global! h g)
  (let ((x (symbol-cell h g)))
    (if (null? (symcell.valloc x))
        (let ((cell (create-cell! h g)))
          (symcell.valloc! x (car cell))
          (symcell.valno! x (cdr cell))))
    (symcell.valloc x)))

(define (dump-thunk! h code constants)
  (let ((base (heap.top h)))
    (heap.header-word! h $imm.procedure-header 8)
    (heap.word! h code)
    (heap.word! h constants)
    (heap.adjust! h)
    (+ base $tag.procedure-tag)))

; The car's are all heap pointers, so they should not be messed with.
; The cdr must be dumped, and then the pair.

(define (dump-list-spine! h l)
  (if (null? l)
      $imm.null
      (let ((the-car (car l))
            (the-cdr (dump-list-spine! h (cdr l))))
        (let ((base (heap.top h)))
          (heap.word! h the-car)
          (heap.word! h the-cdr)
          (+ base $tag.pair-tag)))))

(define (dump-startup-procedure! h)
  (let ((thunks  (dump-list-spine! h (heap.thunks h)))
        (symbols (dump-list-spine! h (symbol-locations h))))
    (dump-segment! h (construct-startup-procedure symbols thunks))))

; The initialization procedure. The lists are magically patched into
; the constant vector after the procedure has been assembled but before
; it is dumped into the heap. See below.
;
; (define (init-proc argv)
;   (let loop ((l <list-of-thunks>))
;     (if (null? l)
;         (go <list-of-symbols> argv)
;         (begin ((car l))
;                (loop (cdr l))))))

(define init-proc
  `((,$.proc)
    (,$args= 1)
    (,$reg 1)                           ; argv into
    (,$setreg 2)                        ;   register 2
    (,$const (thunks))                  ; dummy list of thunks.
    (,$setreg 1)
    (,$.label 0)
    (,$reg 1)
    (,$op1 null?)                       ; (null? l)
    (,$branchf 2)
    (,$const (symbols))                 ; dummy list of symbols
    (,$setreg 1)
    (,$global go)
    ;(,$op1 debugvsm)
    (,$invoke 2)                        ; (go <list of symbols> argv)
    (,$.label 2)
    (,$save 2)
    (,$store 0 0)
    (,$store 1 1)
    (,$store 2 2)
    (,$reg 1)
    (,$op1 car)
    (,$setrtn 3)
    (,$invoke 0)                        ; ((car l))
    (,$.align 4)
    (,$.label 3)
    (,$.cont)
    (,$restore 2)
    (,$pop 2)
    (,$reg 1)
    (,$op1 cdr)
    (,$setreg 1)
    (,$branch 0)))                      ; (loop (cdr l))


;;; Non-overridable code beyond this point

; Stuff a new symbol into the heap, return its location.

(define (create-symbol! h s)
  (dump-vector-like!
   h 
   (vector `(bits ,(dump-string! h (symbol->string s)))
           '(data 0)
           '(data ()))
   dump-tagged-item!
   $tag.symbol-typetag))


; Stuff a value cell into the heap, return a pair of its location
; and its cell number.

(define (create-cell! h s)
  (let* ((symtab (heap.symbol-table h))
         (n (symtab.cell-no symtab))
         (p (dump-pair! h (cons (undefined)
                                (if (generate-global-symbols)
                                    s
                                    n)))))
    (symtab.cell-no! symtab (+ n 1))
    (cons p n)))


(define (construct-startup-procedure symbol-list-addr init-list-addr)

  ; Given some value which might appear in the constant vector, 
  ; replace the entries matching that value with a new value.

  (define (patch-constant-vector! v old new)
    (let loop ((i (- (vector-length v) 1)))
      (if (>= i 0)
          (begin (if (equal? (vector-ref v i) old)
                     (vector-set! v i new))
                 (loop (- i 1))))))

  ; Assemble the startup thunk, patch it, and return it.

  (display "Assembling final procedure") (newline)
  (let ((e (single-stepping)))
    (single-stepping #f)
    (let ((segment (assemble init-proc)))
      (single-stepping e)
      (patch-constant-vector! (segment.constants segment)
                              '(data (thunks))
                              `(bits ,init-list-addr))
      (patch-constant-vector! (segment.constants segment)
                              '(data (symbols))
                              `(bits ,symbol-list-addr))
      segment)))


; Return a list of symbol locations for symbols in the heap, in order.

(define (symbol-locations h)
  (let loop ((symbols (symtab.symbols (heap.symbol-table h))) (res '()))
    (cond ((null? symbols)
           (reverse res))
          ((not (null? (symcell.symloc (car symbols))))
           (loop (cdr symbols)
                 (cons (symcell.symloc (car symbols)) res)))
          (else
           (loop (cdr symbols) res)))))

; Return list of variable name to cell number mappings for global vars.

(define (load-map h)
  (let loop ((symbols (symtab.symbols (heap.symbol-table h))) (res '()))
    (cond ((null? symbols)
           (reverse res))
          ((not (null? (symcell.valloc (car symbols))))
           (loop (cdr symbols)
                 (cons (cons (symcell.name (car symbols))
                             (symcell.valno (car symbols)))
                       res)))
          (else
           (loop (cdr symbols) res)))))


; FIXME:  Should use binary i/o here, but that must wait
; for MzScheme to implement R6RS i/o.

(define (write-header h output-file)
  (delete-file output-file)
  (call-with-raw-latin-1-output-file output-file
    (lambda (out)

      (define (write-roots)
        (let ((assigned-roots (heap.roots h)))
          (for-each (lambda (root-name)
                      (let ((probe (assq root-name assigned-roots)))
                        (if probe
                            (write-word (cdr probe) out)
                            (write-word $imm.false out))))
                    heap.root-names)))

      (write-word heap.version-number out)
      (write-roots)
      (write-word (quotient (heap.top h) 4) out))))


; Ugliness.

(define *append-file-shell-command* #f)  ; assigned below

(define (append-file-shell-command file-to-append file-to-append-to)
  (display "Creating final image in \"")
  (display file-to-append-to) 
  (display "\"...") 
  (newline)
  (if (*append-file-shell-command* file-to-append file-to-append-to)
      (delete-file file-to-append)
      (begin (display "Failed to create image!")
             (newline))))

; Unix: just run CAT to append the file.

(define (append-file-shell-command-unix file-to-append file-to-append-to)
  (zero? (system (string-append "cat " 
                                file-to-append 
                                " >> " 
                                file-to-append-to))))

(define (append-file-shell-command-msdos file-to-append file-to-append-to)
  (let ((x (system (string-append "copy/b/y " 
				  file-to-append-to "+" file-to-append 
				  " TEMPCAT"))))
    (if (not (zero? x))
	#f
	(let ((x (system (string-append "copy/b/y TEMPCAT " file-to-append-to))))
	  (system "del TEMPCAT")
	  (zero? x)))))
  
			 
; Quasi-portable: use read-char and write-char.
; Needs to be read-byte and write-byte.
; FIXME:  Should use binary i/o here, but that must wait
; for MzScheme to implement R6RS i/o.

(define (append-file-shell-command-portable file-to-append file-to-append-to)
  (rename-file file-to-append-to "HEAPDUMP.TEMP")
  (delete-file file-to-append-to)
  (call-with-raw-latin-1-output-file file-to-append-to
    (lambda (out)
      (call-with-raw-latin-1-input-file "HEAPDUMP.TEMP"
        (lambda (in)
          (do ((c (read-byte in) (read-byte in)))
              ((eof-object? c))
            (write-byte c out))))
      (call-with-raw-latin-1-input-file file-to-append
        (lambda (in)
          (do ((c (read-byte in) (read-byte in)))
              ((eof-object? c))
            (write-byte c out))))))
  (delete-file "HEAPDUMP.TEMP")
  #t)

; This can be overridden, and is, by the procedure select-compiler
; in Asm/Standard-C/dumpheap-extra.sch.

(set! *append-file-shell-command* append-file-shell-command-unix)

; eof
