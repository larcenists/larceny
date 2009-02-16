; Complete source for Twobit and Sparc assembler in one file.
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; See 'twobit-benchmark', at end.

; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $

;; compatibility...
(define-syntax logand (syntax-rules () ((_ x y) (fxlogand x y))))
(define-syntax logior (syntax-rules () ((_ x y) (fxlogior x y))))
(define-syntax lsh    (syntax-rules () ((_ x y) (fxlsh x y))))
(define-syntax rsha   (syntax-rules () ((_ x y) (fxrsha x y))))
(define-syntax rshl   (syntax-rules () ((_ x y) (fxrshl x y))))

; Completely fundamental pathname manipulation.

; This takes zero or more directory components and a file name and
; constructs a filename relative to the current directory.

(define (make-relative-filename . components)

  (define (construct l)
    (if (null? (cdr l))
	l
	(cons (car l)
	      (cons "/" (construct (cdr l))))))

  (if (null? (cdr components))
      (car components)
      (apply string-append (construct components))))

; This takes one or more directory components and constructs a 
; directory name with proper termination (a crock -- we can finess 
; this later).

(define (pathname-append . components)

  (define (construct l)
    (cond ((null? (cdr l))
	   l)
	  ((string=? (car l) "")
	   (construct (cdr l)))
          ((char=? #\/ (string-ref (car l) (- (string-length (car l)) 1)))
           (cons (car l) (construct (cdr l))))
	  (else
	   (cons (car l)
		 (cons "/" (construct (cdr l)))))))

  (let ((n (if (null? (cdr components))
	       (car components)
	       (apply string-append (construct components)))))
    (if (not (char=? #\/ (string-ref n (- (string-length n) 1))))
	(string-append n "/")
	n)))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Nbuild parameters for SPARC Larceny.

(define (make-nbuild-parameter dir source? verbose? hostdir hostname)
  (let ((parameters 
	 `((compiler       . ,(pathname-append dir "Compiler"))
	   (util           . ,(pathname-append dir "Util"))
	   (build          . ,(pathname-append dir "Rts" "Build"))
	   (source         . ,(pathname-append dir "Lib"))
           (common-source  . ,(pathname-append dir "Lib" "Common"))
           (repl-source    . ,(pathname-append dir "Repl"))
           (interp-source  . ,(pathname-append dir "Eval"))
           (machine-source . ,(pathname-append dir "Lib" "Sparc"))
	   (common-asm     . ,(pathname-append dir "Asm" "Common"))
	   (sparc-asm      . ,(pathname-append dir "Asm" "Sparc"))
	   (target-machine . SPARC)
	   (endianness     . big)
	   (word-size      . 32)
	   (always-source? . ,source?)
	   (verbose-load?  . ,verbose?)
	   (compatibility  . ,(pathname-append dir "Compat" hostdir))
	   (host-system    . ,hostname)
	   )))
    (lambda (key)
      (let ((probe (assq key parameters)))
	(if probe 
	    (cdr probe)
	    #f)))))

(define nbuild-parameter
  (make-nbuild-parameter "" #f #f "Larceny" "Larceny"))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Useful list functions.
;
; Notes:
; * Reduce, reduce-right, fold-right, fold-left are compatible with MIT Scheme.
; * Make-list is compatible with MIT Scheme and Chez Scheme.
; * These are not (yet) compatible with Shivers's proposed list functions.
; * remq, remv, remove, remq!, remv!, remov!, every?, and some? are in the 
;   basic library.

; Destructively remove all associations whose key matches `key' from `alist'.

(define (aremq! key alist)
  (cond ((null? alist) alist)
	((eq? key (caar alist))
	 (aremq! key (cdr alist)))
	(else
	 (set-cdr! alist (aremq! key (cdr alist)))
	 alist)))

(define (aremv! key alist)
  (cond ((null? alist) alist)
	((eqv? key (caar alist))
	 (aremv! key (cdr alist)))
	(else
	 (set-cdr! alist (aremv! key (cdr alist)))
	 alist)))

(define (aremove! key alist)
  (cond ((null? alist) alist)
	((equal? key (caar alist))
	 (aremove! key (cdr alist)))
	(else
	 (set-cdr! alist (aremove! key (cdr alist)))
	 alist)))

; Return a list of elements of `list' selected by the predicate.

(define (filter select? list)
  (cond ((null? list) list)
	((select? (car list))
	 (cons (car list) (filter select? (cdr list))))
	(else
	 (filter select? (cdr list)))))

; Return the first element of `list' selected by the predicate.

(define (find selected? list)
  (cond ((null? list) #f)
	((selected? (car list)) (car list))
	(else (find selected? (cdr list)))))

; Return a list with all duplicates (according to predicate) removed.

(define (remove-duplicates list same?)

  (define (member? x list)
    (cond ((null? list) #f)
          ((same? x (car list)) #t)
          (else (member? x (cdr list)))))

  (cond ((null? list) list)
        ((member? (car list) (cdr list))
         (remove-duplicates (cdr list) same?))
        (else
         (cons (car list) (remove-duplicates (cdr list) same?)))))

; Return the least element of `list' according to some total order.

(define (least less? list)
  (reduce (lambda (a b) (if (less? a b) a b)) #f list))

; Return the greatest element of `list' according to some total order.

(define (greatest greater? list)
  (reduce (lambda (a b) (if (greater? a b) a b)) #f list))
  
; (mappend p l) = (apply append (map p l))

(define (mappend proc l)
  (apply append (map proc l)))

; (make-list n)   => (a1 ... an) for some ai
; (make-list n x) => (a1 ... an) where ai = x

(define (make-list nelem . rest)
  (let ((val (if (null? rest) #f (car rest))))
    (define (loop n l)
      (if (zero? n)
	  l
	  (loop (- n 1) (cons val l))))
    (loop nelem '())))

; (reduce p x ()) => x
; (reduce p x (a)) => a
; (reduce p x (a b ...)) => (p (p a b) ...))

(define (reduce proc initial l)

  (define (loop val l)
    (if (null? l)
        val
        (loop (proc val (car l)) (cdr l))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop (car l) (cdr l)))))

; (reduce-right p x ()) => x
; (reduce-right p x (a)) => a
; (reduce-right p x (a b ...)) => (p a (p b ...))

(define (reduce-right proc initial l)

  (define (loop l)
    (if (null? (cdr l))
	(car l)
	(proc (car l) (loop (cdr l)))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop l))))

; (fold-left p x (a b ...)) => (p (p (p x a) b) ...)

(define (fold-left proc initial l)
  (if (null? l)
      initial
      (fold-left proc (proc initial (car l)) (cdr l))))

; (fold-right p x (a b ...)) => (p a (p b (p ... x)))

(define (fold-right proc initial l)
  (if (null? l)
      initial
      (proc (car l) (fold-right proc initial (cdr l)))))

; (iota n) => (0 1 2 ... n-1)

(define (iota n)
  (let loop ((n (- n 1)) (r '()))
    (let ((r (cons n r)))
      (if (= n 0)
	  r
	  (loop (- n 1) r)))))

; (list-head (a1 ... an) m) => (a1 ... am)   for m <= n

(define (list-head l n)
  (if (zero? n)
      '()
      (cons (car l) (list-head (cdr l) (- n 1)))))

	
; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Larceny -- compatibility library for Twobit running under Larceny.

(define ($$trace x) #t)

(define host-system 'larceny)

; Temporary?

(define (.check! flag exn . args)
  (if (not flag)
      (apply error "Runtime check exception: " exn args)))

; The compatibility library loads Auxlib if compat:initialize is called
; without arguments.  Compat:load will load fasl files when appropriate.

(define (compat:initialize . rest)
  (if (null? rest)
      (let ((dir (nbuild-parameter 'compatibility)))
	(compat:load (string-append dir "compat2.sch"))
	(compat:load (string-append dir "../../Auxlib/list.sch"))
	(compat:load (string-append dir "../../Auxlib/pp.sch")))))

(define (with-optimization level thunk) 
  (thunk))

; Calls thunk1, and if thunk1 causes an error to be signalled, calls thunk2.

(define (call-with-error-control thunk1 thunk2) 
  (let ((eh (error-handler)))
    (error-handler (lambda args
		     (error-handler eh)
		     (thunk2)
		     (apply eh args)))
    (thunk1)
    (error-handler eh)))

(define (larc-new-extension fn ext)
  (let* ((l (string-length fn))
	 (x (let loop ((i (- l 1)))
	      (cond ((< i 0) #f)
		    ((char=? (string-ref fn i) #\.) (+ i 1))
		    (else (loop (- i 1)))))))
    (if (not x)
	(string-append fn "." ext)
	(string-append (substring fn 0 x) ext))))

(define (compat:load filename)
  (define (loadit fn)
    (if (nbuild-parameter 'verbose-load?)
	(format #t "~a~%" fn))
    (load fn))
  (if (nbuild-parameter 'always-source?)
      (loadit filename)
      (let ((fn (larc-new-extension filename "fasl")))
	(if (and (file-exists? fn)
		 (compat:file-newer? fn filename))
	    (loadit fn)
	    (loadit filename)))))

(define (compat:file-newer? a b)
  (let* ((ta    (file-modification-time a))
	 (tb    (file-modification-time b))
	 (limit (vector-length ta)))
    (let loop ((i 0))
      (cond ((= i limit)
	     #f)
	    ((= (vector-ref ta i) (vector-ref tb i))
	     (loop (+ i 1)))
	    (else
	     (> (vector-ref ta i) (vector-ref tb i)))))))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Larceny -- second part of compatibility code
; This file ought to be compiled, but doesn't have to be.
;
; 12 April 1999

(define host-system 'larceny)		; Don't remove this!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure.

(define compat:sort (lambda (list less?) (sort list less?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Well-defined character codes.
; Returns the UCS-2 code for a character.

(define compat:char->integer char->integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

(define (write-lop item port)
  (lowlevel-write item port)
  (newline port)
  (newline port))

(define write-fasl-datum lowlevel-write)

; The power of self-hosting ;-)

(define (misc->bytevector x)
  (let ((bv (bytevector-like-copy x)))
    (typetag-set! bv $tag.bytevector-typetag)
    bv))

(define string->bytevector misc->bytevector)

(define bignum->bytevector misc->bytevector)

(define (flonum->bytevector x)
  (clear-first-word (misc->bytevector x)))

(define (compnum->bytevector x)
  (clear-first-word (misc->bytevector x)))

; Clears garbage word of compnum/flonum; makes regression testing much
; easier.

(define (clear-first-word bv)
  (bytevector-like-set! bv 0 0)
  (bytevector-like-set! bv 1 0)
  (bytevector-like-set! bv 2 0)
  (bytevector-like-set! bv 3 0)
  bv)

(define (list->bytevector l)
  (let ((b (make-bytevector (length l))))
    (do ((i 0 (+ i 1))
	 (l l (cdr l)))
	((null? l) b)
      (bytevector-set! b i (car l)))))

(define bytevector-word-ref 
  (let ((two^8  (expt 2 8))
	(two^16 (expt 2 16))
	(two^24 (expt 2 24)))
    (lambda (bv i)
      (+ (* (bytevector-ref bv i) two^24)
	 (* (bytevector-ref bv (+ i 1)) two^16)
	 (* (bytevector-ref bv (+ i 2)) two^8)
	 (bytevector-ref bv (+ i 3))))))

(define (twobit-format fmt . rest)
  (let ((out (open-output-string)))
    (apply format out fmt rest)
    (get-output-string out)))

; This needs to be a random number in both a weaker and stronger sense
; than `random': it doesn't need to be a truly random number, so a sequence
; of calls can return a non-random sequence, but if two processes generate
; two sequences, then those sequences should not be the same.
;
; Gross, huh?

(define (an-arbitrary-number)
  (system "echo \\\"`date`\\\" > a-random-number")
  (let ((x (string-hash (call-with-input-file "a-random-number" read))))
    (delete-file "a-random-number")
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous

(define cerror error)

; eof
; Copyright 1991 Wiliam Clinger.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Sets represented as lists.
;
; 5 April 1999.

(define (empty-set) '())

(define (empty-set? x) (null? x))

(define (make-set x)
  (define (loop x y)
    (cond ((null? x) y)
          ((member (car x) y) (loop (cdr x) y))
          (else (loop (cdr x) (cons (car x) y)))))
  (loop x '()))

(define (set-equal? x y)
  (and (subset? x y) (subset? y x)))

(define (subset? x y)
  (every? (lambda (x) (member x y))
          x))

; To get around MacScheme's limit on the number of arguments.

(define apply-union)

(define union
  (letrec ((union2
            (lambda (x y)
              (cond ((null? x) y)
                    ((member (car x) y)
                     (union2 (cdr x) y))
                    (else (union2 (cdr x) (cons (car x) y)))))))
    
    (set! apply-union
          (lambda (sets)
            (do ((sets sets (cdr sets))
                 (result '() (union2 (car sets) result)))
                ((null? sets)
                 result))))
    
    (lambda args
      (cond ((null? args) '())
            ((null? (cdr args)) (car args))
            ((null? (cddr args)) (union2 (car args) (cadr args)))
            (else (union2 (union2 (car args)
                                  (cadr args))
                          (apply union (cddr args))))))))

(define intersection
  (letrec ((intersection2
            (lambda (x y)
              (cond ((null? x) '())
                    ((member (car x) y)
                     (cons (car x) (intersection2 (cdr x) y)))
                    (else (intersection2 (cdr x) y))))))
    (lambda args
      (cond ((null? args) '())
            ((null? (cdr args)) (car args))
            ((null? (cddr args)) (intersection2 (car args) (cadr args)))
            (else (intersection2 (intersection2 (car args)
                                                (cadr args))
                                 (apply intersection (cddr args))))))))

(define (difference x y)
  (cond ((null? x) '())
        ((member (car x) y)
         (difference (cdr x) y))
        (else (cons (car x) (difference (cdr x) y)))))
; Reasonably portable hashing on EQ?, EQV?, EQUAL?.
; Requires bignums, SYMBOL-HASH.
;
; Given any Scheme object, returns a non-negative exact integer
; less than 2^24.

(define object-hash (lambda (x) 0))    ; hash on EQ?, EQV?
(define equal-hash (lambda (x) 0))     ; hash on EQUAL?

(let ((n 16777216)
      (n-1 16777215)
      (adj:fixnum   9000000)
      (adj:negative 8000000)
      (adj:large    7900000)
      (adj:ratnum   7800000)
      (adj:complex  7700000)
      (adj:flonum   7000000)
      (adj:compnum  6900000)
      (adj:char     6111000)
      (adj:string   5022200)
      (adj:vector   4003330)
      (adj:misc     3000444)
      (adj:pair     2555000)
      (adj:proc     2321001)
      (adj:iport    2321002)
      (adj:oport    2321003)
      (adj:weird    2321004)
      (budget0      32))
  
  (define (combine hash adjustment)
    (modulo (+ hash hash hash adjustment) 16777216))
  
  (define (hash-on-equal x budget)
    (if (> budget 0)
        (cond ((string? x)
               (string-hash x))
              ((pair? x)
               (let ((budget (quotient budget 2)))
                 (combine (hash-on-equal (car x) budget)
                          (hash-on-equal (cdr x) budget))))
              ((vector? x)
               (let ((n (vector-length x))
                     (budget (quotient budget 4)))
                 (if (> n 0)
                     (combine
                      (combine (hash-on-equal (vector-ref x 0) budget)
                               (hash-on-equal (vector-ref x (- n 1)) budget))
                      (hash-on-equal (vector-ref x (quotient n 2))
                                     (+ budget budget)))
                     adj:vector)))
              (else
               (object-hash x)))
        adj:weird))
  
  (set! object-hash
        (lambda (x)
          (cond ((symbol? x)
                 (symbol-hash x))
                ((number? x)
                 (if (exact? x)
                     (cond ((integer? x)
                            (cond ((negative? x)
                                   (combine (object-hash (- x)) adj:negative))
                                  ((< x n)
                                   (combine x adj:fixnum))
                                  (else
                                   (combine (modulo x n) adj:large))))
                           ((rational? x)
                            (combine (combine (object-hash (numerator x))
                                              adj:ratnum)
                                     (object-hash (denominator x))))
                           ((real? x)
                            adj:weird)
                           ((complex? x)
                            (combine (combine (object-hash (real-part x))
                                              adj:complex)
                                     (object-hash (imag-part x))))
                           (else
                            adj:weird))
                     (cond (#t
                            ; We can't really do anything with inexact numbers
                            ; unless infinities and NaNs behave reasonably.
                            adj:flonum)
                           ((rational? x)
                            (combine
                             (combine (object-hash
                                       (inexact->exact (numerator x)))
                                      adj:flonum)
                             (object-hash (inexact->exact (denominator x)))))
                           ((real? x)
                            adj:weird)
                           ((complex? x)
                            (combine (combine (object-hash (real-part x))
                                              adj:compnum)
                                     (object-hash (imag-part x))))
                           (else adj:weird))))
                ((char? x)
                 (combine (char->integer x) adj:char))
                ((string? x)
                 (combine (string-length x) adj:string))
                ((vector? x)
                 (combine (vector-length x) adj:vector))
                ((eq? x #t)
                 (combine 1 adj:misc))
                ((eq? x #f)
                 (combine 2 adj:misc))
                ((null? x)
                 (combine 3 adj:misc))
                ((pair? x)
                 adj:pair)
                ((procedure? x)
                 adj:proc)
                ((input-port? x)
                 adj:iport)
                ((output-port? x)
                 adj:oport)
                (else
                 adj:weird))))
  
  (set! equal-hash
        (lambda (x)
          (hash-on-equal x budget0)))); Hash tables.
; Requires CALL-WITHOUT-INTERRUPTS.
; This code should be thread-safe provided VECTOR-REF is atomic.
;
; (make-hashtable <hash-function> <bucket-searcher> <size>)
;
;     Returns a newly allocated mutable hash table
;     using <hash-function> as the hash function
;     and <bucket-searcher>, e.g. ASSQ, ASSV, ASSOC, to search a bucket
;     with <size> buckets at first, expanding the number of buckets as needed.
;     The <hash-function> must accept a key and return a non-negative exact
;     integer.
;
; (make-hashtable <hash-function> <bucket-searcher>)
;
;     Equivalent to (make-hashtable <hash-function> <bucket-searcher> n)
;     for some value of n chosen by the implementation.
;
; (make-hashtable <hash-function>)
;
;     Equivalent to (make-hashtable <hash-function> assv).
;
; (make-hashtable)
;
;     Equivalent to (make-hashtable object-hash assv).
;
; (hashtable-contains? <hashtable> <key>)
;
;     Returns true iff the <hashtable> contains an entry for <key>.
;
; (hashtable-fetch <hashtable> <key> <flag>)
;
;     Returns the value associated with <key> in the <hashtable> if the
;     <hashtable> contains <key>; otherwise returns <flag>.
;
; (hashtable-get <hashtable> <key>)
;
;     Equivalent to (hashtable-fetch <hashtable> <key> #f)
;
; (hashtable-put! <hashtable> <key> <value>)
;
;     Changes the <hashtable> to associate <key> with <value>, replacing
;     any existing association for <key>.
;
; (hashtable-remove! <hashtable> <key>)
;
;     Removes any association for <key> within the <hashtable>.
;
; (hashtable-clear! <hashtable>)
;
;     Removes all associations from the <hashtable>.
;
; (hashtable-size <hashtable>)
;
;     Returns the number of keys contained within the <hashtable>.
;
; (hashtable-for-each <procedure> <hashtable>)
;
;     The <procedure> must accept two arguments, a key and the value
;     associated with that key.  Calls the <procedure> once for each
;     key-value association.  The order of these calls is indeterminate.
;
; (hashtable-map <procedure> <hashtable>)
;
;     The <procedure> must accept two arguments, a key and the value
;     associated with that key.  Calls the <procedure> once for each
;     key-value association, and returns a list of the results.  The
;     order of the calls is indeterminate.
;
; (hashtable-copy <hashtable> <mutable>)
;
;     Returns a copy of the <hashtable>.

; These global variables are assigned new values later.

(define make-hashtable      (lambda args '*))
(define hashtable-contains? (lambda (ht key) #f))
(define hashtable-fetch     (lambda (ht key flag) flag))
(define hashtable-get       (lambda (ht key) (hashtable-fetch ht key #f)))
(define hashtable-put!      (lambda (ht key val) '*))
(define hashtable-remove!   (lambda (ht key) '*))
(define hashtable-clear!    (lambda (ht) '*))
(define hashtable-size      (lambda (ht) 0))
(define hashtable-for-each  (lambda (ht proc) '*))
(define hashtable-map       (lambda (ht proc) '()))
(define hashtable-copy      (lambda (ht mutable?) ht))

; Implementation.
; A hashtable is represented as a vector of the form
;
;     #(("HASHTABLE") <count> <hasher> <searcher> <buckets>)
;
; where <count> is the number of associations within the hashtable,
; <hasher> is the hash function, <searcher> is the bucket searcher,
; and <buckets> is a vector of buckets.
;
; The <hasher> and <searcher> fields are constant, but
; the <count> and <buckets> fields are mutable.
;
; For thread-safe operation, the mutators must modify both
; as an atomic operation.  Other operations do not require
; critical sections provided VECTOR-REF is an atomic operation
; and the operation does not modify the hashtable, does not
; reference the <count> field, and fetches the <buckets>
; field exactly once.

(let ((doc      (list "HASHTABLE"))
      (count    (lambda (ht)   (vector-ref ht 1)))
      (count!   (lambda (ht n) (vector-set! ht 1 n)))
      (hasher   (lambda (ht)   (vector-ref ht 2)))
      (searcher (lambda (ht)   (vector-ref ht 3)))
      (buckets  (lambda (ht)   (vector-ref ht 4)))
      (buckets! (lambda (ht v) (vector-set! ht 4 v)))
      (defaultn 10))
  (let ((hashtable? (lambda (ht)
                      (and (vector? ht)
                           (= 5 (vector-length ht))
                           (eq? doc (vector-ref ht 0)))))
        (hashtable-error (lambda (x)
                           (display "ERROR: Bad hash table: ")
                           (newline)
                           (write x)
                           (newline))))
    
    ; Internal operations.
    
    (define (make-ht hashfun searcher size)
      (vector doc 0 hashfun searcher (make-vector size '())))
    
    ; Substitute x for the first occurrence of y within the list z.
    ; y is known to occur within z.
    
    (define (substitute1 x y z)
      (cond ((eq? y (car z))
             (cons x (cdr z)))
            (else
             (cons (car z)
                   (substitute1 x y (cdr z))))))
    
    ; Remove the first occurrence of x from y.
    ; x is known to occur within y.
    
    (define (remq1 x y)
      (cond ((eq? x (car y))
             (cdr y))
            (else
             (cons (car y)
                   (remq1 x (cdr y))))))
    
    (define (resize ht0)
      (call-without-interrupts
       (lambda ()
         (let ((ht (make-ht (hasher ht0)
                            (searcher ht0)
                            (+ 1 (* 2 (count ht0))))))
           (ht-for-each (lambda (key val)
                          (put! ht key val))
                        ht0)
           (buckets! ht0 (buckets ht))))))
    
    ; Returns the contents of the hashtable as a vector of pairs.
    
    (define (contents ht)
      (let* ((v (buckets ht))
             (n (vector-length v))
             (z (make-vector (count ht) '())))
        (define (loop i bucket j)
          (if (null? bucket)
              (if (= i n)
                  (if (= j (vector-length z))
                      z
                      (begin (display "BUG in hashtable")
                             (newline)
                             '#()))
                  (loop (+ i 1)
                        (vector-ref v i)
                        j))
              (let ((entry (car bucket)))
                (vector-set! z j (cons (car entry) (cdr entry)))
                (loop i
                      (cdr bucket)
                      (+ j 1)))))
        (loop 0 '() 0)))
    
    (define (contains? ht key)
      (if (hashtable? ht)
          (let* ((v (buckets ht))
                 (n (vector-length v))
                 (h (modulo ((hasher ht) key) n))
                 (b (vector-ref v h)))
            (if ((searcher ht) key b)
                #t
                #f))
          (hashtable-error ht)))
    
    (define (fetch ht key flag)
      (if (hashtable? ht)
          (let* ((v (buckets ht))
                 (n (vector-length v))
                 (h (modulo ((hasher ht) key) n))
                 (b (vector-ref v h))
                 (probe ((searcher ht) key b)))
            (if probe
                (cdr probe)
                flag))
          (hashtable-error ht)))
    
    (define (put! ht key val)
      (if (hashtable? ht)
          (call-without-interrupts
           (lambda ()
             (let* ((v (buckets ht))
                    (n (vector-length v))
                    (h (modulo ((hasher ht) key) n))
                    (b (vector-ref v h))
                    (probe ((searcher ht) key b)))
               (if probe
                   ; Using SET-CDR! on the probe would make it necessary
                   ; to synchronize the CONTENTS routine.
                   (vector-set! v h (substitute1 (cons key val) probe b))
                   (begin (count! ht (+ (count ht) 1))
                          (vector-set! v h (cons (cons key val) b))
                          (if (> (count ht) n)
                              (resize ht)))))
             #f))
          (hashtable-error ht)))
    
    (define (remove! ht key)
      (if (hashtable? ht)
          (call-without-interrupts
           (lambda ()
             (let* ((v (buckets ht))
                    (n (vector-length v))
                    (h (modulo ((hasher ht) key) n))
                    (b (vector-ref v h))
                    (probe ((searcher ht) key b)))
               (if probe
                   (begin (count! ht (- (count ht) 1))
                          (vector-set! v h (remq1 probe b))
                          (if (< (* 2 (+ defaultn (count ht))) n)
                              (resize ht))))
               #f)))
          (hashtable-error ht)))
    
    (define (clear! ht)
      (if (hashtable? ht)
          (call-without-interrupts
           (lambda ()
             (begin (count! ht 0)
                    (buckets! ht (make-vector defaultn '()))
                    #f)))
          (hashtable-error ht)))
    
    (define (size ht)
      (if (hashtable? ht)
          (count ht)
          (hashtable-error ht)))
    
    ; This code must be written so that the procedure can modify the
    ; hashtable without breaking any invariants.
    
    (define (ht-for-each f ht)
      (if (hashtable? ht)
          (let* ((v (contents ht))
                 (n (vector-length v)))
            (do ((j 0 (+ j 1)))
                ((= j n))
                (let ((x (vector-ref v j)))
                  (f (car x) (cdr x)))))
          (hashtable-error ht)))
    
    (define (ht-map f ht)
      (if (hashtable? ht)
          (let* ((v (contents ht))
                 (n (vector-length v)))
            (do ((j 0 (+ j 1))
                 (results '() (let ((x (vector-ref v j)))
                                (cons (f (car x) (cdr x))
                                      results))))
                ((= j n)
                 (reverse results))))
          (hashtable-error ht)))
    
    (define (ht-copy ht)
      (if (hashtable? ht)
          (let* ((newtable (make-hashtable (hasher ht) (searcher ht) 0))
                 (v (buckets ht))
                 (n (vector-length v))
                 (newvector (make-vector n '())))
            (count! newtable (count ht))
            (buckets! newtable newvector)
            (do ((i 0 (+ i 1)))
                ((= i n))
                (vector-set! newvector i (append (vector-ref v i) '())))
            newtable)
          (hashtable-error ht)))
    
    ; External entry points.
    
    (set! make-hashtable
          (lambda args
            (let* ((hashfun (if (null? args) object-hash (car args)))
                   (searcher (if (or (null? args) (null? (cdr args)))
                                 assv
                                 (cadr args)))
                   (size (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                             defaultn
                             (caddr args))))
              (make-ht hashfun searcher size))))
    
    (set! hashtable-contains? (lambda (ht key)      (contains? ht key)))
    (set! hashtable-fetch     (lambda (ht key flag) (fetch ht key flag)))
    (set! hashtable-get       (lambda (ht key)      (fetch ht key #f)))
    (set! hashtable-put!      (lambda (ht key val)  (put! ht key val)))
    (set! hashtable-remove!   (lambda (ht key)      (remove! ht key)))
    (set! hashtable-clear!    (lambda (ht)          (clear! ht)))
    (set! hashtable-size      (lambda (ht)          (size ht)))
    (set! hashtable-for-each  (lambda (ht proc)     (ht-for-each ht proc)))
    (set! hashtable-map       (lambda (ht proc)     (ht-map ht proc)))
    (set! hashtable-copy      (lambda (ht mutable?) (ht-copy ht)))
    #f))
; Hash trees: a functional data structure analogous to hash tables.
;
; (make-hashtree <hash-function> <bucket-searcher>)
;
;     Returns a newly allocated mutable hash table
;     using <hash-function> as the hash function
;     and <bucket-searcher>, e.g. ASSQ, ASSV, ASSOC, to search a bucket.
;     The <hash-function> must accept a key and return a non-negative exact
;     integer.
;
; (make-hashtree <hash-function>)
;
;     Equivalent to (make-hashtree <hash-function> assv).
;
; (make-hashtree)
;
;     Equivalent to (make-hashtree object-hash assv).
;
; (hashtree-contains? <hashtree> <key>)
;
;     Returns true iff the <hashtree> contains an entry for <key>.
;
; (hashtree-fetch <hashtree> <key> <flag>)
;
;     Returns the value associated with <key> in the <hashtree> if the
;     <hashtree> contains <key>; otherwise returns <flag>.
;
; (hashtree-get <hashtree> <key>)
;
;     Equivalent to (hashtree-fetch <hashtree> <key> #f)
;
; (hashtree-put <hashtree> <key> <value>)
;
;     Returns a new hashtree that is like <hashtree> except that
;     <key> is associated with <value>.
;
; (hashtree-remove <hashtree> <key>)
;
;     Returns a new hashtree that is like <hashtree> except that
;     <key> is not associated with any value.
;
; (hashtree-size <hashtree>)
;
;     Returns the number of keys contained within the <hashtree>.
;
; (hashtree-for-each <procedure> <hashtree>)
;
;     The <procedure> must accept two arguments, a key and the value
;     associated with that key.  Calls the <procedure> once for each
;     key-value association.  The order of these calls is indeterminate.
;
; (hashtree-map <procedure> <hashtree>)
;
;     The <procedure> must accept two arguments, a key and the value
;     associated with that key.  Calls the <procedure> once for each
;     key-value association, and returns a list of the results.  The
;     order of the calls is indeterminate.

; These global variables are assigned new values later.

(define make-hashtree      (lambda args '*))
(define hashtree-contains? (lambda (ht key) #f))
(define hashtree-fetch     (lambda (ht key flag) flag))
(define hashtree-get       (lambda (ht key) (hashtree-fetch ht key #f)))
(define hashtree-put       (lambda (ht key val) '*))
(define hashtree-remove    (lambda (ht key) '*))
(define hashtree-size      (lambda (ht) 0))
(define hashtree-for-each  (lambda (ht proc) '*))
(define hashtree-map       (lambda (ht proc) '()))

; Implementation.
; A hashtree is represented as a vector of the form
;
;     #(("hashtree") <count> <hasher> <searcher> <buckets>)
;
; where <count> is the number of associations within the hashtree,
; <hasher> is the hash function, <searcher> is the bucket searcher,
; and <buckets> is generated by the following grammar:
;
; <buckets>       ::=  ()
;                   |  (<fixnum> <associations> <buckets> <buckets>)
; <alist>         ::=  (<associations>)
; <associations>  ::=  
;                   |  <association> <associations>
; <association>   ::=  (<key> . <value>)
;
; If <buckets> is of the form (n alist buckets1 buckets2),
; then n is the hash code of all keys in alist, all keys in buckets1
; have a hash code less than n, and all keys in buckets2 have a hash
; code greater than n.

(let ((doc      (list "hashtree"))
      (count    (lambda (ht)   (vector-ref ht 1)))
      (hasher   (lambda (ht)   (vector-ref ht 2)))
      (searcher (lambda (ht)   (vector-ref ht 3)))
      (buckets  (lambda (ht)   (vector-ref ht 4)))
      
      (make-empty-buckets (lambda () '()))
      
      (make-buckets
       (lambda (h alist buckets1 buckets2)
         (list h alist buckets1 buckets2)))
      
      (buckets-empty? (lambda (buckets) (null? buckets)))
      
      (buckets-n      (lambda (buckets) (car buckets)))
      (buckets-alist  (lambda (buckets) (cadr buckets)))
      (buckets-left   (lambda (buckets) (caddr buckets)))
      (buckets-right  (lambda (buckets) (cadddr buckets))))
  
  (let ((hashtree? (lambda (ht)
                      (and (vector? ht)
                           (= 5 (vector-length ht))
                           (eq? doc (vector-ref ht 0)))))
        (hashtree-error (lambda (x)
                           (display "ERROR: Bad hash tree: ")
                           (newline)
                           (write x)
                           (newline))))
    
    ; Internal operations.
    
    (define (make-ht count hashfun searcher buckets)
      (vector doc count hashfun searcher buckets))
    
    ; Substitute x for the first occurrence of y within the list z.
    ; y is known to occur within z.
    
    (define (substitute1 x y z)
      (cond ((eq? y (car z))
             (cons x (cdr z)))
            (else
             (cons (car z)
                   (substitute1 x y (cdr z))))))
    
    ; Remove the first occurrence of x from y.
    ; x is known to occur within y.
    
    (define (remq1 x y)
      (cond ((eq? x (car y))
             (cdr y))
            (else
             (cons (car y)
                   (remq1 x (cdr y))))))
    
    ; Returns the contents of the hashtree as a list of pairs.
    
    (define (contents ht)
      (let* ((t (buckets ht)))
        
        (define (contents t alist)
          (if (buckets-empty? t)
              alist
              (contents (buckets-left t)
                        (contents (buckets-right t)
                                  (append-reverse (buckets-alist t)
                                                  alist)))))
        
        (define (append-reverse x y)
          (if (null? x)
              y
              (append-reverse (cdr x)
                              (cons (car x) y))))
        
        ; Creating a new hashtree from a list that is almost sorted
        ; in hash code order would create an extremely unbalanced
        ; hashtree, so this routine randomizes the order a bit.
        
        (define (randomize1 alist alist1 alist2 alist3)
          (if (null? alist)
              (randomize-combine alist1 alist2 alist3)
              (randomize2 (cdr alist)
                          (cons (car alist) alist1)
                          alist2
                          alist3)))
        
        (define (randomize2 alist alist1 alist2 alist3)
          (if (null? alist)
              (randomize-combine alist1 alist2 alist3)
              (randomize3 (cdr alist)
                          alist1
                          (cons (car alist) alist2)
                          alist3)))
        
        (define (randomize3 alist alist1 alist2 alist3)
          (if (null? alist)
              (randomize-combine alist1 alist2 alist3)
              (randomize1 (cdr alist)
                          alist1
                          alist2
                          (cons (car alist) alist3))))
        
        (define (randomize-combine alist1 alist2 alist3)
          (cond ((null? alist2)
                 alist1)
                ((null? alist3)
                 (append-reverse alist2 alist1))
                (else
                 (append-reverse
                  (randomize1 alist3 '() '() '())
                  (append-reverse
                   (randomize1 alist1 '() '() '())
                   (randomize1 alist2 '() '() '()))))))
        
        (randomize1 (contents t '()) '() '() '())))
    
    (define (contains? ht key)
      (if (hashtree? ht)
          (let* ((t (buckets ht))
                 (h ((hasher ht) key)))
            (if ((searcher ht) key (find-bucket t h))
                #t
                #f))
          (hashtree-error ht)))
    
    (define (fetch ht key flag)
      (if (hashtree? ht)
          (let* ((t (buckets ht))
                 (h ((hasher ht) key))
                 (probe ((searcher ht) key (find-bucket t h))))
            (if probe
                (cdr probe)
                flag))
          (hashtree-error ht)))
    
    ; Given a <buckets> t and a hash code h, returns the alist for h.
    
    (define (find-bucket t h)
      (if (buckets-empty? t)
          '()
          (let ((n (buckets-n t)))
            (cond ((< h n)
                   (find-bucket (buckets-left t) h))
                  ((< n h)
                   (find-bucket (buckets-right t) h))
                  (else
                   (buckets-alist t))))))
    
    (define (put ht key val)
      (if (hashtree? ht)
          (let ((t (buckets ht))
                (h ((hasher ht) key))
                (association (cons key val))
                (c (count ht)))
            (define (put t h)
              (if (buckets-empty? t)
                  (begin (set! c (+ c 1))
                         (make-buckets h (list association) t t))
                  (let ((n     (buckets-n t))
                        (alist (buckets-alist t))
                        (left  (buckets-left t))
                        (right (buckets-right t)))
                    (cond ((< h n)
                           (make-buckets n
                                         alist
                                         (put (buckets-left t) h)
                                         right))
                          ((< n h)
                           (make-buckets n
                                         alist
                                         left
                                         (put (buckets-right t) h)))
                          (else
                           (let ((probe ((searcher ht) key alist)))
                             (if probe
                                 (make-buckets n
                                               (substitute1 association
                                                            probe
                                                            alist)
                                               left
                                               right)
                                 (begin
                                  (set! c (+ c 1))
                                  (make-buckets n
                                                (cons association alist)
                                                left
                                                right)))))))))
            (let ((buckets (put t h)))
              (make-ht c (hasher ht) (searcher ht) buckets)))
          (hashtree-error ht)))
    
    (define (remove ht key)
      (if (hashtree? ht)
          (let ((t (buckets ht))
                (h ((hasher ht) key))
                (c (count ht)))
            (define (remove t h)
              (if (buckets-empty? t)
                  t
                  (let ((n     (buckets-n t))
                        (alist (buckets-alist t))
                        (left  (buckets-left t))
                        (right (buckets-right t)))
                    (cond ((< h n)
                           (make-buckets n
                                         alist
                                         (remove left h)
                                         right))
                          ((< n h)
                           (make-buckets n
                                         alist
                                         left
                                         (remove right h)))
                          (else
                           (let ((probe ((searcher ht) key alist)))
                             (if probe
                                 (begin (set! c (- c 1))
                                        (make-buckets n
                                                      (remq1 probe alist)
                                                      left
                                                      right))
                                 t)))))))
            (let ((buckets (remove t h)))
              (make-ht c (hasher ht) (searcher ht) buckets)))
          (hashtree-error ht)))
    
    (define (size ht)
      (if (hashtree? ht)
          (count ht)
          (hashtree-error ht)))
    
    (define (ht-for-each f ht)
      (if (hashtree? ht)
          (for-each (lambda (association)
                      (f (car association)
                         (cdr association)))
                    (contents ht))
          (hashtree-error ht)))
    
    (define (ht-map f ht)
      (if (hashtree? ht)
          (map (lambda (association)
                 (f (car association)
                    (cdr association)))
               (contents ht))
          (hashtree-error ht)))
    
    ; External entry points.
    
    (set! make-hashtree
          (lambda args
            (let* ((hashfun (if (null? args) object-hash (car args)))
                   (searcher (if (or (null? args) (null? (cdr args)))
                                 assv
                                 (cadr args))))
              (make-ht 0 hashfun searcher (make-empty-buckets)))))
    
    (set! hashtree-contains? (lambda (ht key)      (contains? ht key)))
    (set! hashtree-fetch     (lambda (ht key flag) (fetch ht key flag)))
    (set! hashtree-get       (lambda (ht key)      (fetch ht key #f)))
    (set! hashtree-put       (lambda (ht key val)  (put ht key val)))
    (set! hashtree-remove    (lambda (ht key)      (remove ht key)))
    (set! hashtree-size      (lambda (ht)          (size ht)))
    (set! hashtree-for-each  (lambda (ht proc)     (ht-for-each ht proc)))
    (set! hashtree-map       (lambda (ht proc)     (ht-map ht proc)))
    #f))
; Copyright 1994 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 24 April 1999
;
; Compiler switches needed by Twobit.

(define make-twobit-flag)
(define display-twobit-flag)

(define make-twobit-flag
  (lambda (name)

    (define (twobit-warning)
      (display "Error: incorrect arguments to ")
      (write name)
      (newline)
      (reset))

    (define (display-flag state)
      (display (if state "  + " "  - "))
      (display name)
      (display " is ")
      (display (if state "on" "off"))
      (newline))

    (let ((state #t))
      (lambda args
        (cond ((null? args) state)
              ((and (null? (cdr args))
                    (boolean? (car args)))
               (set! state (car args))
               state)
              ((and (null? (cdr args))
                    (eq? (car args) 'display))
               (display-flag state))
              (else (twobit-warning)))))))

(define (display-twobit-flag flag)
  (flag 'display))
  
; Debugging and convenience.

(define issue-warnings
  (make-twobit-flag 'issue-warnings))

(define include-source-code
  (make-twobit-flag 'include-source-code))

(define include-variable-names
  (make-twobit-flag 'include-variable-names))

(define include-procedure-names
  (make-twobit-flag 'include-procedure-names))

; Space efficiency.
; This switch isn't fully implemented yet.  If it is true, then
; Twobit will generate flat closures and will go to some trouble
; to zero stale registers and stack slots.
; Don't turn this switch off unless space is more important than speed.

(define avoid-space-leaks
  (make-twobit-flag 'avoid-space-leaks))

; Major optimizations.

(define integrate-usual-procedures
  (make-twobit-flag 'integrate-usual-procedures))

(define control-optimization
  (make-twobit-flag 'control-optimization))

(define parallel-assignment-optimization
  (make-twobit-flag 'parallel-assignment-optimization))

(define lambda-optimization
  (make-twobit-flag 'lambda-optimization))

(define benchmark-mode
  (make-twobit-flag 'benchmark-mode))

(define benchmark-block-mode
  (make-twobit-flag 'benchmark-block-mode))

(define global-optimization
  (make-twobit-flag 'global-optimization))

(define interprocedural-inlining
  (make-twobit-flag 'interprocedural-inlining))

(define interprocedural-constant-propagation
  (make-twobit-flag 'interprocedural-constant-propagation))

(define common-subexpression-elimination
  (make-twobit-flag 'common-subexpression-elimination))

(define representation-inference
  (make-twobit-flag 'representation-inference))

(define local-optimization
  (make-twobit-flag 'local-optimization))

; For backwards compatibility, until I can change the code.

(define (ignore-space-leaks . args)
  (if (null? args)
      (not (avoid-space-leaks))
      (avoid-space-leaks (not (car args)))))

(define lambda-optimizations lambda-optimization)
(define local-optimizations local-optimization)

(define (set-compiler-flags! how)
  (case how
    ((no-optimization)
     (set-compiler-flags! 'standard)
     (avoid-space-leaks #t)
     (integrate-usual-procedures #f)
     (control-optimization #f)
     (parallel-assignment-optimization #f)
     (lambda-optimization #f)
     (benchmark-mode #f)
     (benchmark-block-mode #f)
     (global-optimization #f)
     (interprocedural-inlining #f)
     (interprocedural-constant-propagation #f)
     (common-subexpression-elimination #f)
     (representation-inference #f)
     (local-optimization #f))
    ((standard) 
     (issue-warnings #t)
     (include-source-code #f)
     (include-procedure-names #t)
     (include-variable-names #t)
     (avoid-space-leaks #f)
     (runtime-safety-checking #t)
     (integrate-usual-procedures #f)
     (control-optimization #t)
     (parallel-assignment-optimization #t)
     (lambda-optimization #t)
     (benchmark-mode #f)
     (benchmark-block-mode #f)
     (global-optimization #t)
     (interprocedural-inlining #t)
     (interprocedural-constant-propagation #t)
     (common-subexpression-elimination #t)
     (representation-inference #t)
     (local-optimization #t))
    ((fast-safe)
     (let ((bbmode (benchmark-block-mode)))
       (set-compiler-flags! 'standard)
       (integrate-usual-procedures #t)
       (benchmark-mode #t)
       (benchmark-block-mode bbmode)))
    ((fast-unsafe) 
     (set-compiler-flags! 'fast-safe)
     (runtime-safety-checking #f))
    (else 
     (error "set-compiler-flags!: unknown mode " how))))

(define (display-twobit-flags which)
  (case which
    ((debugging)
     (display-twobit-flag issue-warnings)
     (display-twobit-flag include-procedure-names)
     (display-twobit-flag include-variable-names)
     (display-twobit-flag include-source-code))
    ((safety)
     (display-twobit-flag avoid-space-leaks))
    ((optimization)
     (display-twobit-flag integrate-usual-procedures)
     (display-twobit-flag control-optimization)
     (display-twobit-flag parallel-assignment-optimization)
     (display-twobit-flag lambda-optimization)
     (display-twobit-flag benchmark-mode)
     (display-twobit-flag benchmark-block-mode)
     (display-twobit-flag global-optimization)
     (if (global-optimization)
         (begin (display "  ")
                (display-twobit-flag interprocedural-inlining)
                (display "  ")
                (display-twobit-flag interprocedural-constant-propagation)
                (display "  ")
                (display-twobit-flag common-subexpression-elimination)
                (display "  ")
                (display-twobit-flag representation-inference)))
     (display-twobit-flag local-optimization))
    (else
     ; The switch might mean something to the assembler, but not to Twobit
     #t)))

; eof
; Copyright 1991 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 14 April 1999 / wdc

($$trace "pass1.aux")

;***************************************************************
;
; Each definition in this section should be overridden by an assignment
; in a target-specific file.
;
; If a lambda expression has more than @maxargs-with-rest-arg@ required
; arguments followed by a rest argument, then the macro expander will
; rewrite the lambda expression as a lambda expression with only one
; argument (a rest argument) whose body is a LET that binds the arguments
; of the original lambda expression.

(define @maxargs-with-rest-arg@
  1000000)                              ; infinity

(define (prim-entry name) #f)           ; no integrable procedures
(define (prim-arity name) 0)            ; all of which take 0 arguments
(define (prim-opcodename name) name)    ; and go by their source names

; End of definitions to be overridden by target-specific assignments.
;
;***************************************************************

; Miscellaneous routines.

(define (m-warn msg . more)
  (if (issue-warnings)
      (begin
       (display "WARNING from macro expander:")
       (newline)
       (display msg)
       (newline)
       (for-each (lambda (x) (write x) (newline))
                 more))))

(define (m-error msg . more)
  (display "ERROR detected during macro expansion:")
  (newline)
  (display msg)
  (newline)
  (for-each (lambda (x) (write x) (newline))
            more)
  (m-quit (make-constant #f)))

(define (m-bug msg . more)
  (display "BUG in macro expander: ")
  (newline)
  (display msg)
  (newline)
  (for-each (lambda (x) (write x) (newline))
            more)
  (m-quit (make-constant #f)))

; Given a <formals>, returns a list of bound variables.

'
(define (make-null-terminated x)
  (cond ((null? x) '())
        ((pair? x)
         (cons (car x) (make-null-terminated (cdr x))))
        (else (list x))))

; Returns the length of the given list, or -1 if the argument
; is not a list.  Does not check for circular lists.

(define (safe-length x)
  (define (loop x n)
    (cond ((null? x) n)
          ((pair? x) (loop (cdr x) (+ n 1)))
          (else -1)))
  (loop x 0))

; Given a unary predicate and a list, returns a list of those
; elements for which the predicate is true.

(define (filter1 p x)
  (cond ((null? x) '())
        ((p (car x)) (cons (car x) (filter1 p (cdr x))))
        (else (filter1 p (cdr x)))))

; Given a unary predicate and a list, returns #t if the
; predicate is true of every element of the list.

(define (every1? p x)
  (cond ((null? x) #t)
        ((p (car x)) (every1? p (cdr x)))
        (else #f)))

; Binary union of two sets represented as lists, using equal?.

(define (union2 x y)
  (cond ((null? x) y)
        ((member (car x) y)
         (union2 (cdr x) y))
        (else (union2 (cdr x) (cons (car x) y)))))

; Given an association list, copies the association pairs.

(define (copy-alist alist)
  (map (lambda (x) (cons (car x) (cdr x)))
       alist))

; Removes a value from a list.  May destroy the list.

'
(define remq!
  (letrec ((loop (lambda (x y prev)
                   (cond ((null? y) #t)
                         ((eq? x (car y))
                          (set-cdr! prev (cdr y))
                          (loop x (cdr prev) prev))
                         (else
                          (loop x (cdr y) y))))))
    (lambda (x y)
      (cond ((null? y) '())
            ((eq? x (car y))
             (remq! x (cdr y)))
            (else
             (loop x (cdr y) y)
             y)))))

; Procedure-specific source code transformations.
; The transformer is passed a source code expression and a predicate
; and returns one of:
;
;    the original source code expression
;    a new source code expression to use in place of the original
;    #f to indicate that the procedure is being called
;      with an incorrect number of arguments or
;      with an incorrect operand
;
; The original source code expression is guaranteed to be a list whose
; car is the name associated with the transformer.
; The predicate takes an identifier (a symbol) and returns true iff
; that identifier is bound to something other than its global binding.
;
; Since the procedures and their transformations are target-specific,
; they are defined in another file, in the Target subdirectory.

; FIXME:
; I think this is now used in only one place, in simplify-if.

(define (integrable? name)
  (and (integrate-usual-procedures)
       (prim-entry name)))

; MAKE-READABLE strips the referencing information
; and replaces (begin I) by I.
; If the optional argument is true, then it also reconstructs LET.

(define (make-readable exp . rest)
  (let ((fancy? (and (not (null? rest))
                     (car rest))))
    (define (make-readable exp)
      (case (car exp)
        ((quote)    (make-readable-quote exp))
        ((lambda)   `(lambda ,(lambda.args exp)
                             ,@(map (lambda (def)
                                      `(define ,(def.lhs def)
                                               ,(make-readable (def.rhs def))))
                                    (lambda.defs exp))
                               ,(make-readable (lambda.body exp))))
        ((set!)     `(set! ,(assignment.lhs exp)
                           ,(make-readable (assignment.rhs exp))))
        ((if)       `(if ,(make-readable (if.test exp))
                         ,(make-readable (if.then exp))
                         ,(make-readable (if.else exp))))
        ((begin)    (if (variable? exp)
                        (variable.name exp)
                        `(begin ,@(map make-readable (begin.exprs exp)))))
        (else       (make-readable-call exp))))
    (define (make-readable-quote exp)
      (let ((x (constant.value exp)))
        (if (and fancy?
                 (or (boolean? x)
                     (number? x)
                     (char? x)
                     (string? x)))
            x
            exp)))
    (define (make-readable-call exp)
      (let ((proc (call.proc exp)))
        (if (and fancy?
                 (lambda? proc)
                 (list? (lambda.args proc)))
            ;(make-readable-let* exp '() '() '())
            (make-readable-let exp)
            `(,(make-readable (call.proc exp))
              ,@(map make-readable (call.args exp))))))
    (define (make-readable-let exp)
      (let* ((L (call.proc exp))
             (formals (lambda.args L))
             (args (map make-readable (call.args exp)))
             (body (make-readable (lambda.body L))))
        (if (and (null? (lambda.defs L))
                 (= (length args) 1)
                 (pair? body)
                 (or (and (eq? (car body) 'let)
                          (= (length (cadr body)) 1))
                     (eq? (car body) 'let*)))
            `(let* ((,(car formals) ,(car args))
                    ,@(cadr body))
                   ,@(cddr body))
            `(let ,(map list
                        (lambda.args L)
                        args)
                  ,@(map (lambda (def)
                           `(define ,(def.lhs def)
                                    ,(make-readable (def.rhs def))))
                         (lambda.defs L))
                    ,body))))
    (define (make-readable-let* exp vars inits defs)
      (if (and (null? defs)
               (call? exp)
               (lambda? (call.proc exp))
               (= 1 (length (lambda.args (call.proc exp)))))
          (let ((proc (call.proc exp))
                (arg (car (call.args exp))))
            (if (and (call? arg)
                     (lambda? (call.proc arg))
                     (= 1 (length (lambda.args (call.proc arg))))
                     (null? (lambda.defs (call.proc arg))))
                (make-readable-let*
                 (make-call proc (list (lambda.body (call.proc arg))))
                 (cons (car (lambda.args (call.proc arg))) vars)
                 (cons (make-readable (car (call.args arg))) inits)
                 '())
                (make-readable-let* (lambda.body proc)
                                    (cons (car (lambda.args proc)) vars)
                                    (cons (make-readable (car (call.args exp)))
                                          inits)
                                    (map (lambda (def)
                                           `(define ,(def.lhs def)
                                                    ,(make-readable (def.rhs def))))
                                         (reverse (lambda.defs proc))))))
          (cond ((or (not (null? vars))
                     (not (null? defs)))
                 `(let* ,(map list
                              (reverse vars)
                              (reverse inits))
                        ,@defs
                         ,(make-readable exp)))
                ((and (call? exp)
                      (lambda? (call.proc exp)))
                 (let ((proc (call.proc exp)))
                   `(let ,(map list
                               (lambda.args proc)
                               (map make-readable (call.args exp)))
                         ,@(map (lambda (def)
                                  `(define ,(def.lhs def)
                                           ,(make-readable (def.rhs def))))
                                (lambda.defs proc))
                          ,(make-readable (lambda.body proc)))))
                (else
                 (make-readable exp)))))
    (make-readable exp)))

; For testing.

; MAKE-UNREADABLE does the reverse.
; It assumes there are no internal definitions.

(define (make-unreadable exp)
  (cond ((symbol? exp) (list 'begin exp))
        ((pair? exp)
         (case (car exp)
           ((quote) exp)
           ((lambda) (list 'lambda
                           (cadr exp)
                           '(begin)
                           (list '() '() '() '())
                           (make-unreadable (cons 'begin (cddr exp)))))
           ((set!) (list 'set! (cadr exp) (make-unreadable (caddr exp))))
           ((if) (list 'if
                       (make-unreadable (cadr exp))
                       (make-unreadable (caddr exp))
                       (if (= (length exp) 3)
                           '(unspecified)
                           (make-unreadable (cadddr exp)))))
           ((begin) (if (= (length exp) 2)
                        (make-unreadable (cadr exp))
                        (cons 'begin (map make-unreadable (cdr exp)))))
           (else (map make-unreadable exp))))
        (else (list 'quote exp))))
; Copyright 1991 William D Clinger.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 12 April 1999.
;
; Procedures for fetching and clobbering parts of expressions.

($$trace "pass2.aux")

(define (constant? exp) (eq? (car exp) 'quote))
(define (variable? exp)
  (and (eq? (car exp) 'begin)
       (null? (cddr exp))))
(define (lambda? exp) (eq? (car exp) 'lambda))
(define (call? exp) (pair? (car exp)))
(define (assignment? exp) (eq? (car exp) 'set!))
(define (conditional? exp) (eq? (car exp) 'if))
(define (begin? exp)
  (and (eq? (car exp) 'begin)
       (not (null? (cddr exp)))))

(define (make-constant value) (list 'quote value))
(define (make-variable name) (list 'begin name))
(define (make-lambda formals defs R F G decls doc body)
  (list 'lambda
        formals
        (cons 'begin defs)
        (list 'quote (list R F G decls doc))
        body))
(define (make-call proc args) (cons proc (append args '())))
(define (make-assignment lhs rhs) (list 'set! lhs rhs))
(define (make-conditional e0 e1 e2) (list 'if e0 e1 e2))
(define (make-begin exprs)
  (if (null? (cdr exprs))
      (car exprs)
      (cons 'begin (append exprs '()))))
(define (make-definition lhs rhs) (list 'define lhs rhs))

(define (constant.value exp) (cadr exp))
(define (variable.name exp) (cadr exp))
(define (lambda.args exp) (cadr exp))
(define (lambda.defs exp) (cdr (caddr exp)))
(define (lambda.R exp) (car (cadr (cadddr exp))))
(define (lambda.F exp) (cadr (cadr (cadddr exp))))
(define (lambda.G exp) (caddr (cadr (cadddr exp))))
(define (lambda.decls exp) (cadddr (cadr (cadddr exp))))
(define (lambda.doc exp) (car (cddddr (cadr (cadddr exp)))))
(define (lambda.body exp) (car (cddddr exp)))
(define (call.proc exp) (car exp))
(define (call.args exp) (cdr exp))
(define (assignment.lhs exp) (cadr exp))
(define (assignment.rhs exp) (caddr exp))
(define (if.test exp) (cadr exp))
(define (if.then exp) (caddr exp))
(define (if.else exp) (cadddr exp))
(define (begin.exprs exp) (cdr exp))
(define (def.lhs exp) (cadr exp))
(define (def.rhs exp) (caddr exp))

(define (variable-set! exp newexp)
  (set-car! exp (car newexp))
  (set-cdr! exp (append (cdr newexp) '())))
(define (lambda.args-set! exp args) (set-car! (cdr exp) args))
(define (lambda.defs-set! exp defs) (set-cdr! (caddr exp) defs))
(define (lambda.R-set! exp R) (set-car! (cadr (cadddr exp)) R))
(define (lambda.F-set! exp F) (set-car! (cdr (cadr (cadddr exp))) F))
(define (lambda.G-set! exp G) (set-car! (cddr (cadr (cadddr exp))) G))
(define (lambda.decls-set! exp decls) (set-car! (cdddr (cadr (cadddr exp))) decls))
(define (lambda.doc-set! exp doc) (set-car! (cddddr (cadr (cadddr exp))) doc))
(define (lambda.body-set! exp exp0) (set-car! (cddddr exp) exp0))
(define (call.proc-set! exp exp0) (set-car! exp exp0))
(define (call.args-set! exp exprs) (set-cdr! exp exprs))
(define (assignment.rhs-set! exp exp0) (set-car! (cddr exp) exp0))
(define (if.test-set! exp exp0) (set-car! (cdr exp) exp0))
(define (if.then-set! exp exp0) (set-car! (cddr exp) exp0))
(define (if.else-set! exp exp0) (set-car! (cdddr exp) exp0))
(define (begin.exprs-set! exp exprs) (set-cdr! exp exprs))

(define expression-set! variable-set!)  ; used only by pass 3

; FIXME:  This duplicates information in Lib/procinfo.sch.

(define (make-doc name arity formals source-code filename filepos)
  (vector name source-code arity filename filepos formals))
(define (doc.name d)    (vector-ref d 0))
(define (doc.code d)    (vector-ref d 1))
(define (doc.arity d)   (vector-ref d 2))
(define (doc.file d)    (vector-ref d 3))
(define (doc.filepos d) (vector-ref d 4))
(define (doc.formals d) (vector-ref d 5))
(define (doc.name-set! d x)    (if d (vector-set! d 0 x)))
(define (doc.code-set! d x)    (if d (vector-set! d 1 x)))
(define (doc.arity-set! d x)   (if d (vector-set! d 2 x)))
(define (doc.file-set! d x)    (if d (vector-set! d 3 x)))
(define (doc.filepos-set! d x) (if d (vector-set! d 4 x)))
(define (doc.formals-set! d x) (if d (vector-set! d 5 x)))
(define (doc-copy d) (list->vector (vector->list d)))

(define (ignored? name) (eq? name name:IGNORED))

; Fairly harmless bug: rest arguments aren't getting flagged.

(define (flag-as-ignored name L)
  (define (loop name formals)
    (cond ((null? formals)
           ;(pass2-error p2error:violation-of-invariant name formals)
           #t)
          ((symbol? formals) #t)
          ((eq? name (car formals))
           (set-car! formals name:IGNORED)
           (if (not (local? (lambda.R L) name:IGNORED))
               (lambda.R-set! L
                 (cons (make-R-entry name:IGNORED '() '() '())
                       (lambda.R L)))))
          (else (loop name (cdr formals)))))
  (loop name (lambda.args L)))

(define (make-null-terminated formals)
  (cond ((null? formals) '())
        ((symbol? formals) (list formals))
        (else (cons (car formals)
                    (make-null-terminated (cdr formals))))))

(define (list-head x n)
  (cond ((zero? n) '())
        (else (cons (car x) (list-head (cdr x) (- n 1))))))

(define (remq x y)
  (cond ((null? y) '())
        ((eq? x (car y)) (remq x (cdr y)))
        (else (cons (car y) (remq x (cdr y))))))

(define (make-call-to-LIST args)
  (cond ((null? args) (make-constant '()))
        ((null? (cdr args))
         (make-call (make-variable name:CONS)
                    (list (car args) (make-constant '()))))
        (else (make-call (make-variable name:LIST) args))))

(define (pass2-error i . etc)
  (apply cerror (cons (vector-ref pass2-error-messages i) etc)))

(define pass2-error-messages
  '#("System error: violation of an invariant in pass 2"
     "Wrong number of arguments to known procedure"))

(define p2error:violation-of-invariant 0)
(define p2error:wna 1)

; Procedures for fetching referencing information from R-tables.

(define (make-R-entry name refs assigns calls)
  (list name refs assigns calls))

(define (R-entry.name x) (car x))
(define (R-entry.references x) (cadr x))
(define (R-entry.assignments x) (caddr x))
(define (R-entry.calls x) (cadddr x))

(define (R-entry.references-set! x refs) (set-car! (cdr x) refs))
(define (R-entry.assignments-set! x assignments) (set-car! (cddr x) assignments))
(define (R-entry.calls-set! x calls) (set-car! (cdddr x) calls))

(define (local? R I)
  (assq I R))

(define (R-entry R I)
  (assq I R))

(define (R-lookup R I)
  (or (assq I R)
      (pass2-error p2error:violation-of-invariant R I)))

(define (references R I)
  (cadr (R-lookup R I)))

(define (assignments R I)
  (caddr (R-lookup R I)))

(define (calls R I)
  (cadddr (R-lookup R I)))

(define (references-set! R I X)
  (set-car! (cdr (R-lookup R I)) X))

(define (assignments-set! R I X)
  (set-car! (cddr (R-lookup R I)) X))

(define (calls-set! R I X)
  (set-car! (cdddr (R-lookup R I)) X))

; A notepad is a vector of the form #(L0 (L1 ...) (L2 ...) (I ...)),
; where the components are:
;    element 0: a parent lambda expression (or #f if there is no enclosing
;               parent, or we want to pretend that there isn't).
;    element 1: a list of lambda expressions that the parent lambda
;               expression encloses immediately.
;    element 2: a subset of that list that does not escape.
;    element 3: a list of free variables.

(define (make-notepad L)
  (vector L '() '() '()))

(define (notepad.parent np)      (vector-ref np 0))
(define (notepad.lambdas np)     (vector-ref np 1))
(define (notepad.nonescaping np) (vector-ref np 2))
(define (notepad.vars np)        (vector-ref np 3))

(define (notepad.lambdas-set! np x)     (vector-set! np 1 x))
(define (notepad.nonescaping-set! np x) (vector-set! np 2 x))
(define (notepad.vars-set! np x)        (vector-set! np 3 x))

(define (notepad-lambda-add! np L)
  (notepad.lambdas-set! np (cons L (notepad.lambdas np))))

(define (notepad-nonescaping-add! np L)
  (notepad.nonescaping-set! np (cons L (notepad.nonescaping np))))

(define (notepad-var-add! np I)
  (let ((vars (notepad.vars np)))
    (if (not (memq I vars))
        (notepad.vars-set! np (cons I vars)))))

; Given a notepad, returns the list of variables that are closed
; over by some nested lambda expression that escapes.

(define (notepad-captured-variables np)
  (let ((nonescaping (notepad.nonescaping np)))
    (apply-union
     (map (lambda (L)
            (if (memq L nonescaping)
                (lambda.G L)
                (lambda.F L)))
          (notepad.lambdas np)))))

; Given a notepad, returns a list of free variables computed
; as the union of the immediate free variables with the free
; variables of nested lambda expressions.

(define (notepad-free-variables np)
  (do ((lambdas (notepad.lambdas np) (cdr lambdas))
       (fv (notepad.vars np)
           (let ((L (car lambdas)))
             (union (difference (lambda.F L)
                                (make-null-terminated (lambda.args L)))
                    fv))))
      ((null? lambdas) fv)))
; Copyright 1992 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 13 December 1998
; Implementation-dependent parameters and preferences that determine
; how identifiers are represented in the output of the macro expander.
;
; The basic problem is that there are no reserved words, so the
; syntactic keywords of core Scheme that are used to express the
; output need to be represented by data that cannot appear in the
; input.  This file defines those data.

($$trace "prefs")

; FIXME: The following definitions are currently ignored.

; The following definitions assume that identifiers of mixed case
; cannot appear in the input.

(define begin1  (string->symbol "Begin"))
(define define1 (string->symbol "Define"))
(define quote1  (string->symbol "Quote"))
(define lambda1 (string->symbol "Lambda"))
(define if1     (string->symbol "If"))
(define set!1   (string->symbol "Set!"))

; The following defines an implementation-dependent expression
; that evaluates to an undefined (not unspecified!) value, for
; use in expanding the (define x) syntax.

(define undefined1 (list (string->symbol "Undefined")))

; End of FIXME.

; A variable is renamed by suffixing a vertical bar followed by a unique
; integer.  In IEEE and R4RS Scheme, a vertical bar cannot appear as part
; of an identifier, but presumably this is enforced by the reader and not
; by the compiler.  Any other character that cannot appear as part of an
; identifier may be used instead of the vertical bar.

(define renaming-prefix-character #\.)
(define renaming-suffix-character #\|)

(define renaming-prefix (string renaming-prefix-character))
(define renaming-suffix (string renaming-suffix-character))

; Patches for Twobit.  Here temporarily.

(define (make-toplevel-definition id exp)
  (if (lambda? exp)
      (doc.name-set! (lambda.doc exp) id))
  (make-begin
   (list (make-assignment id exp)
         (make-constant id))))
        
(define (make-undefined)
  (make-call (make-variable 'undefined) '()))

(define (make-unspecified)
  (make-call (make-variable 'unspecified) '()))
; Copyright 1992 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 9 December 1998
; Syntactic environments.
;
; A syntactic environment maps identifiers to denotations,
; where a denotation is one of
;
;    (special <special>)
;    (macro <rules> <env>)
;    (inline <rules> <env>)
;    (identifier <id> <references> <assignments> <calls>)
;
; and where <special> is one of
;
;    quote
;    lambda
;    if
;    set!
;    begin
;    define
;    define-syntax
;    let-syntax
;    letrec-syntax
;    syntax-rules
;
; and where <rules> is a compiled <transformer spec> (see R4RS),
; <env> is a syntactic environment, and <id> is an identifier.
;
; An inline denotation is like a macro denotation, except that it
; is not an error when none of the rules match the use.  Inline
; denotations are created by DEFINE-INLINE.
; The standard syntactic environment should not include any
; identifier denotations; space leaks will result if it does.

($$trace "syntaxenv")

(define standard-syntactic-environment
  `((quote         . (special quote))
    (lambda        . (special lambda))
    (if            . (special if))
    (set!          . (special set!))
    (begin         . (special begin))
    (define        . (special define))
    (define-inline . (special define-inline))
    (define-syntax . (special define-syntax))
    (let-syntax    . (special let-syntax))
    (letrec-syntax . (special letrec-syntax))
    (syntax-rules  . (special syntax-rules))
    ))

; Unforgeable synonyms for lambda and set!, used to expand definitions.

(define lambda0 (string->symbol " lambda "))
(define set!0 (string->symbol " set! "))

(define (syntactic-copy env)
  (copy-alist env))

(define (make-basic-syntactic-environment)
  (cons (cons lambda0
              (cdr (assq 'lambda standard-syntactic-environment)))
        (cons (cons set!0
                    (cdr (assq 'set! standard-syntactic-environment)))
              (syntactic-copy standard-syntactic-environment))))

; The global-syntactic-environment will always be a nonempty
; association list since there is no way to remove the entry
; for lambda0.  That entry is used as a header by destructive
; operations.

(define global-syntactic-environment
  (make-basic-syntactic-environment))

(define (global-syntactic-environment-set! env)
  (set-cdr! global-syntactic-environment env)
  #t)

(define (syntactic-bind-globally! id denotation)
  (if (and (identifier-denotation? denotation)
           (eq? id (identifier-name denotation)))
      (letrec ((remove-bindings-for-id
                (lambda (bindings)
                  (cond ((null? bindings) '())
                        ((eq? (caar bindings) id)
                         (remove-bindings-for-id (cdr bindings)))
                        (else (cons (car bindings)
                                    (remove-bindings-for-id (cdr bindings))))))))
        (global-syntactic-environment-set!
         (remove-bindings-for-id (cdr global-syntactic-environment))))
      (let ((x (assq id global-syntactic-environment)))
        (if x
            (begin (set-cdr! x denotation) #t)
            (global-syntactic-environment-set!
             (cons (cons id denotation)
                   (cdr global-syntactic-environment)))))))

(define (syntactic-divert env1 env2)
  (append env2 env1))

(define (syntactic-extend env ids denotations)
  (syntactic-divert env (map cons ids denotations)))

(define (syntactic-lookup env id)
  (let ((entry (assq id env)))
    (if entry
        (cdr entry)
        (make-identifier-denotation id))))

(define (syntactic-assign! env id denotation)
  (let ((entry (assq id env)))
    (if entry
        (set-cdr! entry denotation)
        (m-bug "Bug detected in syntactic-assign!" env id denotation))))

; Denotations.

(define denotation-class car)

(define (special-denotation? denotation)
  (eq? (denotation-class denotation) 'special))

(define (macro-denotation? denotation)
  (eq? (denotation-class denotation) 'macro))

(define (inline-denotation? denotation)
  (eq? (denotation-class denotation) 'inline))

(define (identifier-denotation? denotation)
  (eq? (denotation-class denotation) 'identifier))

(define (make-macro-denotation rules env)
  (list 'macro rules env))

(define (make-inline-denotation id rules env)
  (list 'inline rules env id))

(define (make-identifier-denotation id)
  (list 'identifier id '() '() '()))

(define macro-rules        cadr)
(define macro-env          caddr)

(define inline-rules       macro-rules)
(define inline-env         macro-env)
(define inline-name        cadddr)

(define identifier-name    cadr)
(define identifier-R-entry cdr)

(define (same-denotation? d1 d2)
  (or (eq? d1 d2)
      (and (identifier-denotation? d1)
           (identifier-denotation? d2)
           (eq? (identifier-name d1)
                (identifier-name d2)))))

(define denotation-of-quote
  (syntactic-lookup standard-syntactic-environment 'quote))

(define denotation-of-lambda
  (syntactic-lookup standard-syntactic-environment 'lambda))

(define denotation-of-if
  (syntactic-lookup standard-syntactic-environment 'if))

(define denotation-of-set!
  (syntactic-lookup standard-syntactic-environment 'set!))

(define denotation-of-begin
  (syntactic-lookup standard-syntactic-environment 'begin))

(define denotation-of-define
  (syntactic-lookup standard-syntactic-environment 'define))

(define denotation-of-define-inline
  (syntactic-lookup standard-syntactic-environment 'define-inline))

(define denotation-of-define-syntax
  (syntactic-lookup standard-syntactic-environment 'define-syntax))

(define denotation-of-let-syntax
  (syntactic-lookup standard-syntactic-environment 'let-syntax))

(define denotation-of-letrec-syntax
  (syntactic-lookup standard-syntactic-environment 'letrec-syntax))

(define denotation-of-syntax-rules
  (syntactic-lookup standard-syntactic-environment 'syntax-rules))

(define denotation-of-...
  (syntactic-lookup standard-syntactic-environment '...))

(define denotation-of-transformer
  (syntactic-lookup standard-syntactic-environment 'transformer))

; Given a syntactic environment env to be extended, an alist returned
; by rename-vars, and a syntactic environment env2, extends env by
; binding the fresh identifiers to the denotations of the original
; identifiers in env2.

(define (syntactic-alias env alist env2)
  (syntactic-divert
   env
   (map (lambda (name-pair)
          (let ((old-name (car name-pair))
                (new-name (cdr name-pair)))
            (cons new-name
                  (syntactic-lookup env2 old-name))))
        alist)))

; Given a syntactic environment and an alist returned by rename-vars,
; extends the environment by binding the old identifiers to the fresh
; identifiers.
; For Twobit, it also binds the fresh identifiers to their denotations.
; This is ok so long as the fresh identifiers are not legal Scheme
; identifiers.

(define (syntactic-rename env alist)
  (if (null? alist)
      env
      (let* ((old (caar alist))
             (new (cdar alist))
             (denotation (make-identifier-denotation new)))
        (syntactic-rename
         (cons (cons old denotation)
               (cons (cons new denotation)
                     env))
         (cdr alist)))))

; Renaming of variables.

(define renaming-counter 0)

(define (make-rename-procedure)
  (set! renaming-counter (+ renaming-counter 1))
  (let ((suffix (string-append renaming-suffix (number->string renaming-counter))))
    (lambda (sym)
      (if (symbol? sym)
          (let ((s (symbol->string sym)))
            (if (and (positive? (string-length s))
                     (char=? (string-ref s 0) renaming-prefix-character))
                (string->symbol (string-append s suffix))
                (string->symbol (string-append renaming-prefix s suffix))))
          (m-warn "Illegal use of rename procedure" 'ok:FIXME sym)))))

; Given a datum, strips the suffixes from any symbols that appear within
; the datum, trying not to copy any more of the datum than necessary.

(define (m-strip x)
  (define (original-symbol x)
    (define (loop sym s i n)
      (cond ((= i n) sym)
            ((char=? (string-ref s i)
                     renaming-suffix-character)
             (string->symbol (substring s 1 i)))
            (else
             (loop sym s (+ i 1) n))))
    (let ((s (symbol->string x)))
      (if (and (positive? (string-length s))
               (char=? (string-ref s 0) renaming-prefix-character))
          (loop x s 0 (string-length s))
          x)))
  (cond ((symbol? x)
         (original-symbol x))
        ((pair? x)
         (let ((a (m-strip (car x)))
               (b (m-strip (cdr x))))
           (if (and (eq? a (car x))
                    (eq? b (cdr x)))
               x
               (cons a b))))
        ((vector? x)
         (let* ((v (vector->list x))
                (v2 (map m-strip v)))
           (if (equal? v v2)
               x
               (list->vector v2))))
        (else x)))

; Given a list of identifiers, or a formal parameter "list",
; returns an alist that associates each identifier with a fresh identifier.

(define (rename-vars original-vars)
  (let ((rename (make-rename-procedure)))
    (define (loop vars newvars)
      (cond ((null? vars) (reverse newvars))
            ((pair? vars)
             (let ((var (car vars)))
               (if (symbol? var)
                   (loop (cdr vars)
                         (cons (cons var (rename var))
                               newvars))
                   (m-error "Illegal variable" var))))
            ((symbol? vars)
             (loop (list vars) newvars))
            (else (m-error "Malformed parameter list" original-vars))))
    (loop original-vars '())))

; Given a <formals> and an alist returned by rename-vars that contains
; a new name for each formal identifier in <formals>, renames the
; formal identifiers.

(define (rename-formals formals alist)
  (cond ((null? formals) '())
        ((pair? formals)
         (cons (cdr (assq (car formals) alist))
               (rename-formals (cdr formals) alist)))
        (else (cdr (assq formals alist)))))
; Copyright 1992 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 23 November 1998
; Compiler for a <transformer spec>.
;
; References:
;
;    The Revised^4 Report on the Algorithmic Language Scheme.
;    Clinger and Rees [editors].  To appear in Lisp Pointers.
;    Also available as a technical report from U of Oregon,
;    MIT AI Lab, and Cornell.
;
;    Macros That Work.  Clinger and Rees.  POPL '91.
;
; The input is a <transformer spec> and a syntactic environment.
; Syntactic environments are described in another file.
;
; The supported syntax differs from the R4RS in that vectors are
; allowed as patterns and as templates and are not allowed as
; pattern or template data.
;
;    <transformer spec>  -->  (syntax-rules <literals> <rules>)
;    <rules>  -->  ()  |  (<rule> . <rules>)
;    <rule> --> (<pattern> <template>)
;    <pattern> --> <pattern_var>      ; a <symbol> not in <literals>
;                | <symbol>           ; a <symbol> in <literals>
;                | ()
;                | (<pattern> . <pattern>)
;                | (<ellipsis_pattern>)
;                | #(<pattern>*)                     ; extends R4RS
;                | #(<pattern>* <ellipsis_pattern>)  ; extends R4RS
;                | <pattern_datum>
;    <template> --> <pattern_var>
;                |  <symbol>
;                |  ()
;                |  (<template2> . <template2>)
;                |  #(<template>*)                   ; extends R4RS
;                |  <pattern_datum>
;    <template2> --> <template>  |  <ellipsis_template>
;    <pattern_datum> --> <string>                    ; no <vector>
;                     |  <character>
;                     |  <boolean>
;                     |  <number>
;    <ellipsis_pattern>  --> <pattern> ...
;    <ellipsis_template> --> <template> ...
;    <pattern_var>       --> <symbol> ; not in <literals>
;    <literals>  -->  ()  |  (<symbol> . <literals>)
;
; Definitions.
;
; scope of an ellipsis
;
;    Within a pattern or template, the scope of an ellipsis
;    (...) is the pattern or template that appears to its left.
;
; rank of a pattern variable
;
;    The rank of a pattern variable is the number of ellipses
;    within whose scope it appears in the pattern.
;
; rank of a subtemplate
;
;    The rank of a subtemplate is the number of ellipses within
;    whose scope it appears in the template.
;
; template rank of an occurrence of a pattern variable
;
;    The template rank of an occurrence of a pattern variable
;    within a template is the rank of that occurrence, viewed
;    as a subtemplate.
;
; variables bound by a pattern
;
;    The variables bound by a pattern are the pattern variables
;    that appear within it.
;
; referenced variables of a subtemplate
;
;    The referenced variables of a subtemplate are the pattern
;    variables that appear within it.
;
; variables opened by an ellipsis template
;
;    The variables opened by an ellipsis template are the
;    referenced pattern variables whose rank is greater than
;    the rank of the ellipsis template.
;    
;
; Restrictions.
;
;    No pattern variable appears more than once within a pattern.
;
;    For every occurrence of a pattern variable within a template,
;    the template rank of the occurrence must be greater than or
;    equal to the pattern variable's rank.
;
;    Every ellipsis template must open at least one variable.
;    
;    For every ellipsis template, the variables opened by an
;    ellipsis template must all be bound to sequences of the
;    same length.
;
;
; The compiled form of a <rule> is
;
;    <rule> --> (<pattern> <template> <inserted>)
;    <pattern> --> <pattern_var>
;                | <symbol>
;                | ()
;                | (<pattern> . <pattern>)
;                | <ellipsis_pattern>
;                | #(<pattern>)
;                | <pattern_datum>
;    <template> --> <pattern_var>
;                |  <symbol>
;                |  ()
;                |  (<template2> . <template2>)
;                |  #(<pattern>)
;                |  <pattern_datum>
;    <template2> --> <template>  |  <ellipsis_template>
;    <pattern_datum> --> <string>
;                     |  <character>
;                     |  <boolean>
;                     |  <number>
;    <pattern_var>       --> #(<V> <symbol> <rank>)
;    <ellipsis_pattern>  --> #(<E> <pattern> <pattern_vars>)
;    <ellipsis_template> --> #(<E> <template> <pattern_vars>)
;    <inserted> -->     ()  |  (<symbol> . <inserted>)
;    <pattern_vars> --> ()  |  (<pattern_var> . <pattern_vars>)
;    <rank>  -->  <exact non-negative integer>
;
; where <V> and <E> are unforgeable values.
; The pattern variables associated with an ellipsis pattern
; are the variables bound by the pattern, and the pattern
; variables associated with an ellipsis template are the
; variables opened by the ellipsis template.
;
;
; What's wrong with the above?
; If the template contains a big chunk that contains no pattern variables
; or inserted identifiers, then the big chunk will be copied unnecessarily.
; That shouldn't matter very often.

($$trace "syntaxrules")

(define pattern-variable-flag (list 'v))
(define ellipsis-pattern-flag (list 'e))
(define ellipsis-template-flag ellipsis-pattern-flag)

(define (make-patternvar v rank)
  (vector pattern-variable-flag v rank))
(define (make-ellipsis-pattern P vars)
  (vector ellipsis-pattern-flag P vars))
(define (make-ellipsis-template T vars)
  (vector ellipsis-template-flag T vars))

(define (patternvar? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) pattern-variable-flag)))

(define (ellipsis-pattern? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) ellipsis-pattern-flag)))

(define (ellipsis-template? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) ellipsis-template-flag)))

(define (patternvar-name V) (vector-ref V 1))
(define (patternvar-rank V) (vector-ref V 2))
(define (ellipsis-pattern P) (vector-ref P 1))
(define (ellipsis-pattern-vars P) (vector-ref P 2))
(define (ellipsis-template T) (vector-ref T 1))
(define (ellipsis-template-vars T) (vector-ref T 2))

(define (pattern-variable v vars)
  (cond ((null? vars) #f)
        ((eq? v (patternvar-name (car vars)))
         (car vars))
        (else (pattern-variable v (cdr vars)))))

; Given a <transformer spec> and a syntactic environment,
; returns a macro denotation.
;
; A macro denotation is of the form
;
;    (macro (<rule> ...) env)
;
; where each <rule> has been compiled as described above.

(define (m-compile-transformer-spec spec env)
  (if (and (> (safe-length spec) 1)
           (eq? (syntactic-lookup env (car spec))
                denotation-of-syntax-rules))
      (let ((literals (cadr spec))
            (rules (cddr spec)))
        (if (or (not (list? literals))
                (not (every1? (lambda (rule)
                                (and (= (safe-length rule) 2)
                                     (pair? (car rule))))
                              rules)))
            (m-error "Malformed syntax-rules" spec))
        (list 'macro
              (map (lambda (rule)
                     (m-compile-rule rule literals env))
                   rules)
              env))
      (m-error "Malformed syntax-rules" spec)))

(define (m-compile-rule rule literals env)
  (m-compile-pattern (cdr (car rule))
                     literals
                     env
                     (lambda (compiled-rule patternvars)
                       ; FIXME
                       ; should check uniqueness of pattern variables here
                       (cons compiled-rule
                             (m-compile-template
                              (cadr rule)
                              patternvars
                              env)))))

(define (m-compile-pattern P literals env k)
  (define (loop P vars rank k)
    (cond ((symbol? P)
           (if (memq P literals)
               (k P vars)
               (let ((var (make-patternvar P rank)))
                 (k var (cons var vars)))))
          ((null? P) (k '() vars))
          ((pair? P)
           (if (and (pair? (cdr P))
                    (symbol? (cadr P))
                    (same-denotation? (syntactic-lookup env (cadr P))
                                      denotation-of-...))
               (if (null? (cddr P))
                   (loop (car P)
                         '()
                         (+ rank 1)
                         (lambda (P vars1)
                           (k (make-ellipsis-pattern P vars1)
                              (union2 vars1 vars))))
                   (m-error "Malformed pattern" P))
               (loop (car P)
                     vars
                     rank
                     (lambda (P1 vars)
                       (loop (cdr P)
                             vars
                             rank
                             (lambda (P2 vars)
                               (k (cons P1 P2) vars)))))))
          ((vector? P)
           (loop (vector->list P)
                 vars
                 rank
                 (lambda (P vars)
                   (k (vector P) vars))))
          (else (k P vars))))
  (loop P '() 0 k))

(define (m-compile-template T vars env)
  
  (define (loop T inserted referenced rank escaped? k)
    (cond ((symbol? T)
           (let ((x (pattern-variable T vars)))
             (if x
                 (if (>= rank (patternvar-rank x))
                     (k x inserted (cons x referenced))
                     (m-error
                      "Too few ellipses follow pattern variable in template"
                      (patternvar-name x)))
                 (k T (cons T inserted) referenced))))
          ((null? T) (k '() inserted referenced))
          ((pair? T)
           (cond ((and (not escaped?)
                       (symbol? (car T))
                       (same-denotation? (syntactic-lookup env (car T))
                                         denotation-of-...)
                       (pair? (cdr T))
                       (null? (cddr T)))
                  (loop (cadr T) inserted referenced rank #t k))
                 ((and (not escaped?)
                       (pair? (cdr T))
                       (symbol? (cadr T))
                       (same-denotation? (syntactic-lookup env (cadr T))
                                         denotation-of-...))
                  (loop1 T inserted referenced rank escaped? k))
                 (else
                  (loop (car T)
                        inserted
                        referenced
                        rank
                        escaped?
                        (lambda (T1 inserted referenced)
                          (loop (cdr T)
                                inserted
                                referenced
                                rank
                                escaped?
                                (lambda (T2 inserted referenced)
                                  (k (cons T1 T2) inserted referenced))))))))
          ((vector? T)
           (loop (vector->list T)
                 inserted
                 referenced
                 rank
                 escaped?
                 (lambda (T inserted referenced)
                   (k (vector T) inserted referenced))))
          (else (k T inserted referenced))))
  
  (define (loop1 T inserted referenced rank escaped? k)
    (loop (car T)
          inserted
          '()
          (+ rank 1)
          escaped?
          (lambda (T1 inserted referenced1)
            (loop (cddr T)
                  inserted
                  (append referenced1 referenced)
                  rank
                  escaped?
                  (lambda (T2 inserted referenced)
                    (k (cons (make-ellipsis-template
                              T1
                              (filter1 (lambda (var)
                                         (> (patternvar-rank var)
                                            rank))
                                       referenced1))
                             T2)
                       inserted
                       referenced))))))
  
  (loop T
        '()
        '()
        0
        #f
        (lambda (T inserted referenced)
          (list T inserted))))

; The pattern matcher.
;
; Given an input, a pattern, and two syntactic environments,
; returns a pattern variable environment (represented as an alist)
; if the input matches the pattern, otherwise returns #f.

(define empty-pattern-variable-environment
  (list (make-patternvar (string->symbol "") 0)))

(define (m-match F P env-def env-use)
  
  (define (match F P answer rank)
    (cond ((null? P)
           (and (null? F) answer))
          ((pair? P)
           (and (pair? F)
                (let ((answer (match (car F) (car P) answer rank)))
                  (and answer (match (cdr F) (cdr P) answer rank)))))
          ((symbol? P)
           (and (symbol? F)
                (same-denotation? (syntactic-lookup env-def P)
                                  (syntactic-lookup env-use F))
                answer))
          ((patternvar? P)
           (cons (cons P F) answer))
          ((ellipsis-pattern? P)
           (match1 F P answer (+ rank 1)))
          ((vector? P)
           (and (vector? F)
                (match (vector->list F) (vector-ref P 0) answer rank)))
          (else (and (equal? F P) answer))))
  
  (define (match1 F P answer rank)
    (cond ((not (list? F)) #f)
          ((null? F)
           (append (map (lambda (var) (cons var '()))
                        (ellipsis-pattern-vars P))
                   answer))
          (else
           (let* ((P1 (ellipsis-pattern P))
                  (answers (map (lambda (F) (match F P1 answer rank))
                                F)))
             (if (every1? (lambda (answer) answer) answers)
                 (append (map (lambda (var)
                                (cons var
                                      (map (lambda (answer)
                                             (cdr (assq var answer)))
                                           answers)))
                              (ellipsis-pattern-vars P))
                         answer)
                 #f)))))
  
  (match F P empty-pattern-variable-environment 0))

(define (m-rewrite T alist)
  
  (define (rewrite T alist rank)
    (cond ((null? T) '())
          ((pair? T)
           ((if (ellipsis-pattern? (car T))
                append
                cons)
            (rewrite (car T) alist rank)
            (rewrite (cdr T) alist rank)))
          ((symbol? T) (cdr (assq T alist)))
          ((patternvar? T) (cdr (assq T alist)))
          ((ellipsis-template? T)
           (rewrite1 T alist (+ rank 1)))
          ((vector? T)
           (list->vector (rewrite (vector-ref T 0) alist rank)))
          (else T)))
  
  (define (rewrite1 T alist rank)
    (let* ((T1 (ellipsis-template T))
           (vars (ellipsis-template-vars T))
           (rows (map (lambda (var) (cdr (assq var alist)))
                      vars)))
      (map (lambda (alist) (rewrite T1 alist rank))
           (make-columns vars rows alist))))
  
  (define (make-columns vars rows alist)
    (define (loop rows)
      (if (null? (car rows))
          '()
          (cons (append (map (lambda (var row)
                               (cons var (car row)))
                             vars
                             rows)
                        alist)
                (loop (map cdr rows)))))
    (if (or (null? (cdr rows))
            (apply = (map length rows)))
        (loop rows)
        (m-error "Use of macro is not consistent with definition"
                 vars
                 rows)))
  
  (rewrite T alist 0))

; Given a use of a macro, the syntactic environment of the use,
; a continuation that expects a transcribed expression and
; a new environment in which to continue expansion, and a boolean
; that is true if this transcription is for an inline procedure,
; does the right thing.

(define (m-transcribe0 exp env-use k inline?)
  (let* ((m (syntactic-lookup env-use (car exp)))
         (rules (macro-rules m))
         (env-def (macro-env m))
         (F (cdr exp)))
    (define (loop rules)
      (if (null? rules)
          (if inline?
              (k exp env-use)
              (m-error "Use of macro does not match definition" exp))
          (let* ((rule (car rules))
                 (pattern (car rule))
                 (alist (m-match F pattern env-def env-use)))
            (if alist
                (let* ((template (cadr rule))
                       (inserted (caddr rule))
                       (alist2 (rename-vars inserted))
                       (newexp (m-rewrite template (append alist2 alist))))
                  (k newexp
                     (syntactic-alias env-use alist2 env-def)))
                (loop (cdr rules))))))
    (if (procedure? rules)
        (m-transcribe-low-level exp env-use k rules env-def)
        (loop rules))))

(define (m-transcribe exp env-use k)
  (m-transcribe0 exp env-use k #f))

(define (m-transcribe-inline exp env-use k)
  (m-transcribe0 exp env-use k #t))

; Copyright 1998 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Low-level macro facility based on explicit renaming.  See
; William D Clinger. Hygienic macros through explicit renaming.
; In Lisp Pointers IV(4), 25-28, December 1991.

($$trace "lowlevel")

(define (m-transcribe-low-level exp env-use k transformer env-def)
  (let ((rename0 (make-rename-procedure))
        (renamed '())
        (ok #t))
    (define (lookup sym)
      (let loop ((alist renamed))
        (cond ((null? alist)
               (syntactic-lookup env-use sym))
              ((eq? sym (cdr (car alist)))
               (syntactic-lookup env-def (car (car alist))))
              (else
               (loop (cdr alist))))))
    (let ((rename
           (lambda (sym)
             (if ok
                 (let ((probe (assq sym renamed)))
                   (if probe
                       (cdr probe)
                       (let ((sym2 (rename0 sym)))
                         (set! renamed (cons (cons sym sym2) renamed))
                         sym2)))
                 (m-error "Illegal use of a rename procedure" sym))))
          (compare
           (lambda (sym1 sym2)
             (same-denotation? (lookup sym1) (lookup sym2)))))
      (let ((exp2 (transformer exp rename compare)))
        (set! ok #f)
        (k exp2
           (syntactic-alias env-use renamed env-def))))))

(define identifier? symbol?)

(define (identifier->symbol id)
  (m-strip id))
; Copyright 1992 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 22 April 1999

($$trace "expand")

; This procedure sets the default scope of global macro definitions.

(define define-syntax-scope
  (let ((flag 'letrec))
    (lambda args
      (cond ((null? args) flag)
            ((not (null? (cdr args)))
             (apply m-warn
                    "Too many arguments passed to define-syntax-scope"
                    args))
            ((memq (car args) '(letrec letrec* let*))
             (set! flag (car args)))
            (else (m-warn "Unrecognized argument to define-syntax-scope"
                          (car args)))))))

; The main entry point.
; The outermost lambda allows known procedures to be lifted outside
; all local variables.

(define (macro-expand def-or-exp)
  (call-with-current-continuation
   (lambda (k)
     (set! m-quit k)
     (set! renaming-counter 0)
     (make-call
      (make-lambda '() ; formals
                   '() ; definitions
                   '() ; R
                   '() ; F
                   '() ; G
                   '() ; declarations
                   #f  ; documentation
                   (desugar-definitions def-or-exp
                                        global-syntactic-environment
                                        make-toplevel-definition))
      '()))))

(define (desugar-definitions exp env make-toplevel-definition)
  (letrec
    
    ((define-loop 
       (lambda (exp rest first env)
         (cond ((and (pair? exp)
                     (symbol? (car exp))
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-begin)
                     (pair? (cdr exp)))
                (define-loop (cadr exp) (append (cddr exp) rest) first env))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define))
                (let ((exp (desugar-define exp env)))
                  (cond ((and (null? first) (null? rest))
                         exp)
                        ((null? rest)
                         (make-begin (reverse (cons exp first))))
                        (else (define-loop (car rest)
                                (cdr rest)
                                (cons exp first)
                                env)))))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (or (eq? (syntactic-lookup env (car exp))
                              denotation-of-define-syntax)
                         (eq? (syntactic-lookup env (car exp))
                              denotation-of-define-inline))
                     (null? first))
                (define-syntax-loop exp rest env))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (macro-denotation? (syntactic-lookup env (car exp))))
                (m-transcribe exp
                              env
                              (lambda (exp env)
                                (define-loop exp rest first env))))
               ((and (null? first) (null? rest))
                (m-expand exp env))
               ((null? rest)
                (make-begin (reverse (cons (m-expand exp env) first))))
               (else (make-begin
                      (append (reverse first)
                              (map (lambda (exp) (m-expand exp env))
                                   (cons exp rest))))))))
     
     (define-syntax-loop 
       (lambda (exp rest env)
         (cond ((and (pair? exp)
                     (symbol? (car exp))
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-begin)
                     (pair? (cdr exp)))
                (define-syntax-loop (cadr exp) (append (cddr exp) rest) env))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define-syntax))
                (if (pair? (cdr exp))
                    (redefinition (cadr exp)))
                (if (null? rest)
                    (m-define-syntax exp env)
                    (begin (m-define-syntax exp env)
                           (define-syntax-loop (car rest) (cdr rest) env))))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define-inline))
                (if (pair? (cdr exp))
                    (redefinition (cadr exp)))
                (if (null? rest)
                    (m-define-inline exp env)
                    (begin (m-define-inline exp env)
                           (define-syntax-loop (car rest) (cdr rest) env))))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (macro-denotation? (syntactic-lookup env (car exp))))
                (m-transcribe exp
                              env
                              (lambda (exp env)
                                (define-syntax-loop exp rest env))))
               ((and (pair? exp)
                     (symbol? (car exp))
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define))
                (define-loop exp rest '() env))
               ((null? rest)
                (m-expand exp env))
               (else (make-begin
                      (map (lambda (exp) (m-expand exp env))
                           (cons exp rest)))))))
     
     (desugar-define
      (lambda (exp env)
        (cond 
         ((null? (cdr exp)) (m-error "Malformed definition" exp))
         ; (define foo) syntax is transformed into (define foo (undefined)).
         ((null? (cddr exp))
          (let ((id (cadr exp)))
            (if (or (null? pass1-block-inlines)
                    (not (memq id pass1-block-inlines)))
                (begin
                 (redefinition id)
                 (syntactic-bind-globally! id (make-identifier-denotation id))))
            (make-toplevel-definition id (make-undefined))))
         ((pair? (cadr exp))              
          (desugar-define
           (let* ((def (car exp))
                  (pattern (cadr exp))
                  (f (car pattern))
                  (args (cdr pattern))
                  (body (cddr exp)))
             (if (and (symbol? (car (cadr exp)))
                      (benchmark-mode)
                      (list? (cadr exp)))
                 `(,def ,f
                        (,lambda0 ,args
                           ((,lambda0 (,f)
                               (,set!0 ,f (,lambda0 ,args ,@body))
                               ,pattern)
                            0)))
                 `(,def ,f (,lambda0 ,args ,@body))))
           env))
         ((> (length exp) 3) (m-error "Malformed definition" exp))
         (else (let ((id (cadr exp)))
                 (if (or (null? pass1-block-inlines)
                         (not (memq id pass1-block-inlines)))
                     (begin
                      (redefinition id)
                      (syntactic-bind-globally! id (make-identifier-denotation id))))
                 (make-toplevel-definition id (m-expand (caddr exp) env)))))))
     
     (redefinition
      (lambda (id)
        (if (symbol? id)
            (if (not (identifier-denotation?
                      (syntactic-lookup global-syntactic-environment id)))
                (if (issue-warnings)
                    (m-warn "Redefining " id)))
            (m-error "Malformed variable or keyword" id)))))
    
    ; body of letrec
    
    (define-loop exp '() '() env)))

; Given an expression and a syntactic environment,
; returns an expression in core Scheme.

(define (m-expand exp env)
  (cond ((not (pair? exp))
         (m-atom exp env))
        ((not (symbol? (car exp)))
         (m-application exp env))
        (else
         (let ((keyword (syntactic-lookup env (car exp))))
           (case (denotation-class keyword)
             ((special)
              (cond
               ((eq? keyword denotation-of-quote)         (m-quote exp))
               ((eq? keyword denotation-of-lambda)        (m-lambda exp env))
               ((eq? keyword denotation-of-if)            (m-if exp env))
               ((eq? keyword denotation-of-set!)          (m-set exp env))
               ((eq? keyword denotation-of-begin)         (m-begin exp env))
               ((eq? keyword denotation-of-let-syntax)
		(m-let-syntax exp env))
               ((eq? keyword denotation-of-letrec-syntax)
		(m-letrec-syntax exp env))
               ((or (eq? keyword denotation-of-define)
                    (eq? keyword denotation-of-define-syntax)
                    (eq? keyword denotation-of-define-inline))
                (m-error "Definition out of context" exp))
               (else (m-bug "Bug detected in m-expand" exp env))))
             ((macro) (m-macro exp env))
             ((inline) (m-inline exp env))
             ((identifier) (m-application exp env))
             (else (m-bug "Bug detected in m-expand" exp env)))))))

(define (m-atom exp env)
  (cond ((not (symbol? exp))
         ; Here exp ought to be a boolean, number, character, or string.
         ; I'll warn about other things but treat them as if quoted.
	 ;
	 ; I'm turning off some of the warnings because notably procedures
	 ; and #!unspecified can occur in loaded files and it's a major
	 ; pain if a warning is printed for each. --lars
         (if (and (not (boolean? exp))
                  (not (number? exp))
                  (not (char? exp))
                  (not (string? exp))
		  (not (procedure? exp))
		  (not (eq? exp (unspecified))))
             (m-warn "Malformed constant -- should be quoted" exp))
         (make-constant exp))
        (else (let ((denotation (syntactic-lookup env exp)))
                (case (denotation-class denotation)
                  ((special macro)
                   (m-warn "Syntactic keyword used as a variable" exp)
                   ; Syntactic keywords used as variables are treated as #t.
                   (make-constant #t))
                  ((inline)
                   (make-variable (inline-name denotation)))
                  ((identifier)
                   (let ((var (make-variable (identifier-name denotation)))
                         (R-entry (identifier-R-entry denotation)))
                     (R-entry.references-set!
                      R-entry
                      (cons var (R-entry.references R-entry)))
                     var))
                  (else (m-bug "Bug detected by m-atom" exp env)))))))

(define (m-quote exp)
  (if (and (pair? (cdr exp))
           (null? (cddr exp)))
      (make-constant (m-strip (cadr exp)))
      (m-error "Malformed quoted constant" exp)))

(define (m-lambda exp env)
  (if (> (safe-length exp) 2)
      
      (let* ((formals (cadr exp))
             (alist (rename-vars formals))
             (env (syntactic-rename env alist))
             (body (cddr exp)))
        
        (do ((alist alist (cdr alist)))
            ((null? alist))
            (if (assq (caar alist) (cdr alist))
                (m-error "Malformed parameter list" formals)))
        
        ; To simplify the run-time system, there's a limit on how many
        ; fixed arguments can be followed by a rest argument.
        ; That limit is removed here.
        ; Bug: documentation slot isn't right when this happens.
        ; Bug: this generates extremely inefficient code.
        
        (if (and (not (list? formals))
                 (> (length alist) @maxargs-with-rest-arg@))
            (let ((TEMP (car (rename-vars '(temp)))))
              (m-lambda
               `(,lambda0 ,TEMP
                           ((,lambda0 ,(map car alist)
                                      ,@(cddr exp))
                            ,@(do ((actuals '() (cons (list name:CAR path)
                                                      actuals))
                                   (path TEMP (list name:CDR path))
                                   (formals formals (cdr formals)))
                                  ((symbol? formals)
                                   (append (reverse actuals) (list path))))))
               env))
            (make-lambda (rename-formals formals alist)
                         '() ; no definitions yet
                         (map (lambda (entry)
                                (cdr (syntactic-lookup env (cdr entry))))
                              alist) ; R
                         '() ; F
                         '() ; G
                         '() ; decls
                         (make-doc #f
                                   (if (list? formals)
                                       (length alist)
                                       (exact->inexact (- (length alist) 1)))
                                   (if (include-variable-names)
                                       formals
                                       #f)
                                   (if (include-source-code)
                                       exp
                                       #f)
                                   source-file-name
                                   source-file-position)
                         (m-body body env))))
      
      (m-error "Malformed lambda expression" exp)))

(define (m-body body env)
  (define (loop body env defs)
    (if (null? body)
        (m-error "Empty body"))
    (let ((exp (car body)))
      (if (and (pair? exp)
               (symbol? (car exp)))
          (let ((denotation (syntactic-lookup env (car exp))))
            (case (denotation-class denotation)
              ((special)
               (cond ((eq? denotation denotation-of-begin)
                      (loop (append (cdr exp) (cdr body)) env defs))
                     ((eq? denotation denotation-of-define)
                      (loop (cdr body) env (cons exp defs)))
                     (else (finalize-body body env defs))))
              ((macro)
               (m-transcribe exp
                             env
                             (lambda (exp env)
                               (loop (cons exp (cdr body))
                                     env
                                     defs))))
              ((inline identifier)
               (finalize-body body env defs))
              (else (m-bug "Bug detected in m-body" body env))))
          (finalize-body body env defs))))
  (loop body env '()))

(define (finalize-body body env defs)
  (if (null? defs)
      (let ((body (map (lambda (exp) (m-expand exp env))
                       body)))
        (if (null? (cdr body))
            (car body)
            (make-begin body)))
      (let ()
        (define (sort-defs defs)
          (let* ((augmented
                  (map (lambda (def)
                         (let ((rhs (cadr def)))
                           (if (not (pair? rhs))
                               (cons 'trivial def)
                               (let ((denotation
                                      (syntactic-lookup env (car rhs))))
                                 (cond ((eq? denotation
                                             denotation-of-lambda)
                                        (cons 'procedure def))
                                       ((eq? denotation
                                             denotation-of-quote)
                                        (cons 'trivial def))
                                       (else
                                        (cons 'miscellaneous def)))))))
                       defs))
                 (sorted (twobit-sort (lambda (x y)
                                        (or (eq? (car x) 'procedure)
                                            (eq? (car y) 'miscellaneous)))
                                      augmented)))
            (map cdr sorted)))
        (define (desugar-definition def)
          (if (> (safe-length def) 2)
              (cond ((pair? (cadr def))
                     (desugar-definition
                      `(,(car def)
                        ,(car (cadr def))
                        (,lambda0
                          ,(cdr (cadr def))
                          ,@(cddr def)))))
                    ((and (= (length def) 3)
                          (symbol? (cadr def)))
                     (cdr def))
                    (else (m-error "Malformed definition" def)))
              (m-error "Malformed definition" def)))
        (define (expand-letrec bindings body)
          (make-call
           (m-expand
            `(,lambda0 ,(map car bindings)
                       ,@(map (lambda (binding)
                                `(,set!0 ,(car binding)
                                         ,(cadr binding)))
                              bindings)
                         ,@body)
            env)
           (map (lambda (binding) (make-unspecified)) bindings)))
        (expand-letrec (sort-defs (map desugar-definition
                                       (reverse defs)))
                       body))))

(define (m-if exp env)
  (let ((n (safe-length exp)))
    (if (or (= n 3) (= n 4))
        (make-conditional (m-expand (cadr exp) env)
                          (m-expand (caddr exp) env)
                          (if (= n 3)
                              (make-unspecified)
                              (m-expand (cadddr exp) env)))
        (m-error "Malformed if expression" exp))))

(define (m-set exp env)
  (if (= (safe-length exp) 3)
      (let ((lhs (m-expand (cadr exp) env))
            (rhs (m-expand (caddr exp) env)))
        (if (variable? lhs)
            (let* ((x (variable.name lhs))
                   (assignment (make-assignment x rhs))
                   (denotation (syntactic-lookup env x)))
              (if (identifier-denotation? denotation)
                  (let ((R-entry (identifier-R-entry denotation)))
                    (R-entry.references-set!
                     R-entry
                     (remq lhs (R-entry.references R-entry)))
                    (R-entry.assignments-set!
                     R-entry
                     (cons assignment (R-entry.assignments R-entry)))))
              (if (and (lambda? rhs)
                       (include-procedure-names))
                  (let ((doc (lambda.doc rhs)))
                    (doc.name-set! doc x)))
              (if pass1-block-compiling?
                  (set! pass1-block-assignments
                        (cons x pass1-block-assignments)))
              assignment)
            (m-error "Malformed assignment" exp)))
      (m-error "Malformed assignment" exp)))

(define (m-begin exp env)
  (cond ((> (safe-length exp) 1)
         (make-begin (map (lambda (exp) (m-expand exp env)) (cdr exp))))
        ((= (safe-length exp) 1)
         (m-warn "Non-standard begin expression" exp)
         (make-unspecified))
        (else
         (m-error "Malformed begin expression" exp))))

(define (m-application exp env)
  (if (> (safe-length exp) 0)
      (let* ((proc (m-expand (car exp) env))
             (args (map (lambda (exp) (m-expand exp env))
                        (cdr exp)))
             (call (make-call proc args)))
        (if (variable? proc)
            (let* ((procname (variable.name proc))
                   (entry
                    (and (not (null? args))
                         (constant? (car args))
                         (integrate-usual-procedures)
                         (every1? constant? args)
                         (let ((entry (constant-folding-entry procname)))
                           (and entry
                                (let ((predicates
                                       (constant-folding-predicates entry)))
                                  (and (= (length args)
                                          (length predicates))
                                       (let loop ((args args)
                                                  (predicates predicates))
                                         (cond ((null? args) entry)
                                               (((car predicates)
                                                 (constant.value (car args)))
                                                (loop (cdr args)
                                                      (cdr predicates)))
                                               (else #f))))))))))
              (if entry
                  (make-constant (apply (constant-folding-folder entry)
                                        (map constant.value args)))
                  (let ((denotation (syntactic-lookup env procname)))
                    (if (identifier-denotation? denotation)
                        (let ((R-entry (identifier-R-entry denotation)))
                          (R-entry.calls-set!
                           R-entry
                           (cons call (R-entry.calls R-entry)))))
                    call)))
            call))
      (m-error "Malformed application" exp)))

; The environment argument should always be global here.

(define (m-define-inline exp env)
  (cond ((and (= (safe-length exp) 3)
              (symbol? (cadr exp)))
         (let ((name (cadr exp)))
           (m-define-syntax1 name
                             (caddr exp)
                             env
                             (define-syntax-scope))
           (let ((denotation
                  (syntactic-lookup global-syntactic-environment name)))
             (syntactic-bind-globally!
              name
              (make-inline-denotation name
                                      (macro-rules denotation)
                                      (macro-env denotation))))
           (make-constant name)))
        (else
         (m-error "Malformed define-inline" exp))))

; The environment argument should always be global here.

(define (m-define-syntax exp env)
  (cond ((and (= (safe-length exp) 3)
              (symbol? (cadr exp)))
         (m-define-syntax1 (cadr exp)
                           (caddr exp)
                           env
                           (define-syntax-scope)))
        ((and (= (safe-length exp) 4)
              (symbol? (cadr exp))
              ; FIXME: should use denotations here
              (memq (caddr exp) '(letrec letrec* let*)))
         (m-define-syntax1 (cadr exp)
                           (cadddr exp)
                           env
                           (caddr exp)))
        (else (m-error "Malformed define-syntax" exp))))

(define (m-define-syntax1 keyword spec env scope)
  (if (and (pair? spec)
           (symbol? (car spec)))
      (let* ((transformer-keyword (car spec))
             (denotation (syntactic-lookup env transformer-keyword)))
        (cond ((eq? denotation denotation-of-syntax-rules)
               (case scope
                 ((letrec)  (m-define-syntax-letrec keyword spec env))
                 ((letrec*) (m-define-syntax-letrec* keyword spec env))
                 ((let*)    (m-define-syntax-let* keyword spec env))
                 (else      (m-bug "Weird scope" scope))))
              ((same-denotation? denotation denotation-of-transformer)
               ; FIXME: no error checking here
               (syntactic-bind-globally!
                keyword
                (make-macro-denotation (eval (cadr spec)) env)))
              (else
               (m-error "Malformed syntax transformer" spec))))
      (m-error "Malformed syntax transformer" spec))
  (make-constant keyword))

(define (m-define-syntax-letrec keyword spec env)
  (syntactic-bind-globally!
   keyword
   (m-compile-transformer-spec spec env)))

(define (m-define-syntax-letrec* keyword spec env)
  (let* ((env (syntactic-extend (syntactic-copy env)
                                (list keyword)
                                '((fake denotation))))
         (transformer (m-compile-transformer-spec spec env)))
    (syntactic-assign! env keyword transformer)
    (syntactic-bind-globally! keyword transformer)))

(define (m-define-syntax-let* keyword spec env)
  (syntactic-bind-globally!
   keyword
   (m-compile-transformer-spec spec (syntactic-copy env))))

(define (m-let-syntax exp env)
  (if (and (> (safe-length exp) 2)
           (every1? (lambda (binding)
                      (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding))
                           (null? (cddr binding))))
                    (cadr exp)))
      (m-body (cddr exp)
              (syntactic-extend env
                                (map car (cadr exp))
                                (map (lambda (spec)
                                       (m-compile-transformer-spec
                                        spec
                                        env))
                                     (map cadr (cadr exp)))))
      (m-error "Malformed let-syntax" exp)))

(define (m-letrec-syntax exp env)
  (if (and (> (safe-length exp) 2)
           (every1? (lambda (binding)
                      (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding))
                           (null? (cddr binding))))
                    (cadr exp)))
      (let ((env (syntactic-extend env
                                   (map car (cadr exp))
                                   (map (lambda (id)
                                          '(fake denotation))
                                        (cadr exp)))))
        (for-each (lambda (id spec)
                    (syntactic-assign!
                     env
                     id
                     (m-compile-transformer-spec spec env)))
                  (map car (cadr exp))
                  (map cadr (cadr exp)))
        (m-body (cddr exp) env))
      (m-error "Malformed let-syntax" exp)))

(define (m-macro exp env)
  (m-transcribe exp
                env
                (lambda (exp env)
                  (m-expand exp env))))

(define (m-inline exp env)
  (if (integrate-usual-procedures)
      (m-transcribe-inline exp
                           env
                           (lambda (newexp env)
                             (if (eq? exp newexp)
                                 (m-application exp env)
                                 (m-expand newexp env))))
      (m-application exp env)))

(define m-quit             ; assigned by macro-expand
  (lambda (v) v))

; To do:
; Clean up alist hacking et cetera.
; Declarations.
; Integrable procedures.
; New semantics for body of LET-SYNTAX and LETREC-SYNTAX.
; Copyright 1992 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 5 April 1999.

($$trace "usual")

; The usual macros, adapted from Jonathan's Version 2 implementation.
; DEFINE is handled primitively, since top-level DEFINE has a side
; effect on the global syntactic environment, and internal definitions
; have to be handled specially anyway.
;
; Some extensions are noted, as are some optimizations.
;
; The LETREC* scope rule is used here to protect these macros against
; redefinition of LAMBDA etc.  The scope rule is changed to LETREC at
; the end of this file.

(define-syntax-scope 'letrec*)

(for-each (lambda (form)
            (macro-expand form))
          '(

; Named LET is defined later, after LETREC has been defined.

(define-syntax let
  (syntax-rules ()
    ((let ((?name ?val) ...) ?body ?body1 ...)
     ((lambda (?name ...) ?body ?body1 ...) ?val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () ?body ?body1 ...)
     (let () ?body ?body1 ...))
    ((let* ((?name1 ?val1) (?name ?val) ...) ?body ?body1 ...)
     (let ((?name1 ?val1)) (let* ((?name ?val) ...) ?body ?body1 ...)))))

; Internal definitions have to be handled specially anyway,
; so we might as well rely on them here.

(define-syntax letrec
  (syntax-rules (lambda quote)
   ((letrec ((?name ?val) ...) ?body ?body2 ...)
    ((lambda ()
       (define ?name ?val) ...
       ?body ?body2 ...)))))

; This definition of named LET extends the prior definition of LET.
; The first rule is non-circular, thanks to the LET* scope that is
; specified for this use of DEFINE-SYNTAX.

(define-syntax let let*
  (syntax-rules ()
    ((let (?bindings ...) . ?body)
     (let (?bindings ...) . ?body))
    ((let ?tag ((?name ?val) ...) ?body ?body1 ...)
     (let ((?name ?val) ...)
       (letrec ((?tag (lambda (?name ...) ?body ?body1 ...)))
         (?tag ?name ...))))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and ?e) ?e)
    ((and ?e1 ?e2 ?e3 ...)
     (if ?e1 (and ?e2 ?e3 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or ?e) ?e)
    ((or ?e1 ?e2 ?e3 ...)
     (let ((temp ?e1))
       (if temp temp (or ?e2 ?e3 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else ?result ?result2 ...))
     (begin ?result ?result2 ...))
    
    ((cond (?test => ?result))
     (let ((temp ?test))
       (if temp (?result temp))))
    
    ((cond (?test)) ?test)
    
    ((cond (?test ?result ?result2 ...))
     (if ?test (begin ?result ?result2 ...)))
    
    ((cond (?test => ?result) ?clause ?clause2 ...)
     (let ((temp ?test))
       (if temp (?result temp) (cond ?clause ?clause2 ...))))
    
    ((cond (?test) ?clause ?clause2 ...)
     (or ?test (cond ?clause ?clause2 ...)))
    
    ((cond (?test ?result ?result2 ...)
           ?clause ?clause2 ...)
     (if ?test
         (begin ?result ?result2 ...)
         (cond ?clause ?clause2 ...)))))

; The R4RS says a <step> may be omitted.
; That's a good excuse for a macro-defining macro that uses LETREC-SYNTAX
; and the ... escape.

(define-syntax do
  (syntax-rules ()
    ((do (?bindings0 ...) (?test) ?body0 ...)
     (do (?bindings0 ...) (?test (if #f #f)) ?body0 ...))
    ((do (?bindings0 ...) ?clause0 ?body0 ...)
     (letrec-syntax
       ((do-aux
         (... (syntax-rules ()
                ((do-aux () ((?name ?init ?step) ...) ?clause ?body ...)
                 (letrec ((loop (lambda (?name ...)
                                  (cond ?clause
                                        (else
                                         (begin #t ?body ...)
                                         (loop ?step ...))))))
                   (loop ?init ...)))
                ((do-aux ((?name ?init ?step) ?todo ...)
                         (?bindings ...)
                         ?clause
                         ?body ...)
                 (do-aux (?todo ...)
                         (?bindings ... (?name ?init ?step))
                         ?clause
                         ?body ...))
                ((do-aux ((?name ?init) ?todo ...)
                         (?bindings ...)
                         ?clause
                         ?body ...)
                 (do-aux (?todo ...)
                         (?bindings ... (?name ?init ?name))
                         ?clause
                         ?body ...))))))
       (do-aux (?bindings0 ...) () ?clause0 ?body0 ...)))))

(define-syntax delay
  (syntax-rules ()
    ((delay ?e) (.make-promise (lambda () ?e)))))

; Another use of LETREC-SYNTAX and the escape extension.

(define-syntax case
  (syntax-rules (else)
    ((case ?e1 (else ?body ?body2 ...))
     (begin ?e1 ?body ?body2 ...))
    ((case ?e1 (?z ?body ?body2 ...))
     (if (memv ?e1 '?z) (begin ?body ?body2 ...)))
    ((case ?e1 ?clause1 ?clause2 ?clause3 ...)
     (letrec-syntax
       ((case-aux
          (... (syntax-rules (else)
                ((case-aux ?temp (else ?body ?body2 ...))
                 (begin ?body ?body2 ...))
                ((case-aux ?temp ((?z ...) ?body ?body2 ...))
                 (if (memv ?temp '(?z ...)) (begin ?body ?body2 ...)))
                ((case-aux ?temp ((?z ...) ?body ?body2 ...) ?c1 ?c2 ...)
                 (if (memv ?temp '(?z ...))
                     (begin ?body ?body2 ...)
                     (case-aux ?temp ?c1 ?c2 ...)))
                ; a popular extension
                ((case-aux ?temp (?z ?body ...) ?c1 ...)
                 (case-aux ?temp ((?z) ?body ...) ?c1 ...))))))
       (let ((temp ?e1))
         (case-aux temp ?clause1 ?clause2 ?clause3 ...))))))

; A complete implementation of quasiquote, obtained by translating
; Jonathan Rees's implementation that was posted to RRRS-AUTHORS
; on 22 December 1986.
; Unfortunately, the use of LETREC scope means that it is vulnerable
; to top-level redefinitions of QUOTE etc.  That could be fixed, but
; it has hair enough already.

(begin
 
 (define-syntax .finalize-quasiquote letrec
   (syntax-rules (quote unquote unquote-splicing)
    ((.finalize-quasiquote quote ?arg ?return)
     (.interpret-continuation ?return (quote ?arg)))
    ((.finalize-quasiquote unquote ?arg ?return)
     (.interpret-continuation ?return ?arg))
    ((.finalize-quasiquote unquote-splicing ?arg ?return)
     (syntax-error ",@ in illegal context" ?arg))
    ((.finalize-quasiquote ?mode ?arg ?return)
     (.interpret-continuation ?return (?mode . ?arg)))))
 
 ; The first two "arguments" to .descend-quasiquote and to
 ; .descend-quasiquote-pair are always identical.
 
 (define-syntax .descend-quasiquote letrec
   (syntax-rules (quasiquote unquote unquote-splicing)
    ((.descend-quasiquote `?y ?x ?level ?return)
     (.descend-quasiquote-pair ?x ?x (?level) ?return))
    ((.descend-quasiquote ,?y ?x () ?return)
     (.interpret-continuation ?return unquote ?y))
    ((.descend-quasiquote ,?y ?x (?level) ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote ,@?y ?x () ?return)
     (.interpret-continuation ?return unquote-splicing ?y))
    ((.descend-quasiquote ,@?y ?x (?level) ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote (?y . ?z) ?x ?level ?return)
     (.descend-quasiquote-pair ?x ?x ?level ?return))
    ((.descend-quasiquote #(?y ...) ?x ?level ?return)
     (.descend-quasiquote-vector ?x ?x ?level ?return))
    ((.descend-quasiquote ?y ?x ?level ?return)
     (.interpret-continuation ?return quote ?x))))
 
 (define-syntax .descend-quasiquote-pair letrec
   (syntax-rules (quote unquote unquote-splicing)
    ((.descend-quasiquote-pair (?carx . ?cdrx) ?x ?level ?return)
     (.descend-quasiquote ?carx ?carx ?level (1 ?cdrx ?x ?level ?return)))))
 
 (define-syntax .descend-quasiquote-vector letrec
   (syntax-rules (quote)
    ((.descend-quasiquote-vector #(?y ...) ?x ?level ?return)
     (.descend-quasiquote (?y ...) (?y ...) ?level (6 ?x ?return)))))
 
 ; Representations for continuations used here.
 ; Continuation types 0, 1, 2, and 6 take a mode and an expression.
 ; Continuation types -1, 3, 4, 5, and 7 take just an expression.
 ;
 ; (-1)
 ;     means no continuation
 ; (0)
 ;     means to call .finalize-quasiquote with no further continuation
 ; (1 ?cdrx ?x ?level ?return)
 ;     means a return from the call to .descend-quasiquote from
 ;     .descend-quasiquote-pair
 ; (2 ?car-mode ?car-arg ?x ?return)
 ;     means a return from the second call to .descend-quasiquote in
 ;     in Jonathan's code for .descend-quasiquote-pair
 ; (3 ?car-arg ?return)
 ;     means take the result and return an append of ?car-arg with it
 ; (4 ?cdr-mode ?cdr-arg ?return)
 ;     means take the result and call .finalize-quasiquote on ?cdr-mode
 ;     and ?cdr-arg with a continuation of type 5
 ; (5 ?car-result ?return)
 ;     means take the result and return a cons of ?car-result onto it
 ; (6 ?x ?return)
 ;     means a return from the call to .descend-quasiquote from
 ;     .descend-quasiquote-vector
 ; (7 ?return)
 ;     means take the result and return a call of list->vector on it
 
 (define-syntax .interpret-continuation letrec
   (syntax-rules (quote unquote unquote-splicing)
    ((.interpret-continuation (-1) ?e) ?e)
    ((.interpret-continuation (0) ?mode ?arg)
     (.finalize-quasiquote ?mode ?arg (-1)))    
    ((.interpret-continuation (1 ?cdrx ?x ?level ?return) ?car-mode ?car-arg)
     (.descend-quasiquote ?cdrx
                          ?cdrx
                          ?level
                          (2 ?car-mode ?car-arg ?x ?return)))    
    ((.interpret-continuation (2 quote ?car-arg ?x ?return) quote ?cdr-arg)
     (.interpret-continuation ?return quote ?x))    
    ((.interpret-continuation (2 unquote-splicing ?car-arg ?x ?return) quote ())
     (.interpret-continuation ?return unquote ?car-arg))
    ((.interpret-continuation (2 unquote-splicing ?car-arg ?x ?return)
                              ?cdr-mode ?cdr-arg)
     (.finalize-quasiquote ?cdr-mode ?cdr-arg (3 ?car-arg ?return)))  
    ((.interpret-continuation (2 ?car-mode ?car-arg ?x ?return)
                              ?cdr-mode ?cdr-arg)
     (.finalize-quasiquote ?car-mode ?car-arg (4 ?cdr-mode ?cdr-arg ?return)))
      
    ((.interpret-continuation (3 ?car-arg ?return) ?e)
     (.interpret-continuation ?return append (?car-arg ?e)))
    ((.interpret-continuation (4 ?cdr-mode ?cdr-arg ?return) ?e1)
     (.finalize-quasiquote ?cdr-mode ?cdr-arg (5 ?e1 ?return)))
    ((.interpret-continuation (5 ?e1 ?return) ?e2)
     (.interpret-continuation ?return .cons (?e1 ?e2)))
    ((.interpret-continuation (6 ?x ?return) quote ?arg)
     (.interpret-continuation ?return quote ?x))
    ((.interpret-continuation (6 ?x ?return) ?mode ?arg)
     (.finalize-quasiquote ?mode ?arg (7 ?return)))
    ((.interpret-continuation (7 ?return) ?e)
     (.interpret-continuation ?return .list->vector (?e)))))
 
 (define-syntax quasiquote letrec
   (syntax-rules ()
    ((quasiquote ?x)
     (.descend-quasiquote ?x ?x () (0)))))
 )

(define-syntax let*-syntax
  (syntax-rules ()
    ((let*-syntax () ?body)
     (let-syntax () ?body))
    ((let*-syntax ((?name1 ?val1) (?name ?val) ...) ?body)
     (let-syntax ((?name1 ?val1)) (let*-syntax ((?name ?val) ...) ?body)))))


            ))

(define-syntax-scope 'letrec)

(define standard-syntactic-environment
  (syntactic-copy global-syntactic-environment))

(define (make-standard-syntactic-environment)
  (syntactic-copy standard-syntactic-environment))
; Copyright 1998 William Clinger.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 25 April 1999
;
; Given an expression in the subset of Scheme used as an intermediate language
; by Twobit, returns a newly allocated copy of the expression in which the
; local variables have been renamed and the referencing information has been
; recomputed.

(define (copy-exp exp)
  
  (define special-names (cons name:IGNORED argument-registers))
  
  (define original-names (make-hashtable symbol-hash assq))
  
  (define renaming-counter 0)
  
  (define (rename-vars vars)
    (let ((rename (make-rename-procedure)))
      (map (lambda (var)
             (cond ((memq var special-names)
                    var)
                   ((hashtable-get original-names var)
                    (rename var))
                   (else
                    (hashtable-put! original-names var #t)
                    var)))
           vars)))
  
  (define (rename-formals formals newnames)
    (cond ((null? formals) '())
          ((symbol? formals) (car newnames))
          ((memq (car formals) special-names)
           (cons (car formals)
                 (rename-formals (cdr formals)
                                 (cdr newnames))))
          (else (cons (car newnames)
                      (rename-formals (cdr formals)
                                      (cdr newnames))))))
  
  ; Environments that map symbols to arbitrary information.
  ; This data type is mutable, and uses the shallow binding technique.
  
  (define (make-env) (make-hashtable symbol-hash assq))
  
  (define (env-bind! env sym info)
    (let ((stack (hashtable-get env sym)))
      (hashtable-put! env sym (cons info stack))))
  
  (define (env-unbind! env sym)
    (let ((stack (hashtable-get env sym)))
      (hashtable-put! env sym (cdr stack))))
  
  (define (env-lookup env sym default)
    (let ((stack (hashtable-get env sym)))
      (if stack
          (car stack)
          default)))
  
  (define (env-bind-multiple! env symbols infos)
    (for-each (lambda (sym info) (env-bind! env sym info))
              symbols
              infos))
  
  (define (env-unbind-multiple! env symbols)
    (for-each (lambda (sym) (env-unbind! env sym))
              symbols))
  
  ;
  
  (define (lexical-lookup R-table name)
    (assq name R-table))
  
  (define (copy exp env notepad R-table)
    (cond ((constant? exp) exp)
          ((lambda? exp)
           (let* ((bvl (make-null-terminated (lambda.args exp)))
                  (newnames (rename-vars bvl))
                  (procnames (map def.lhs (lambda.defs exp)))
                  (newprocnames (rename-vars procnames))
                  (refinfo (map (lambda (var)
                                  (make-R-entry var '() '() '()))
                                (append newnames newprocnames)))
                  (newexp
                   (make-lambda
                    (rename-formals (lambda.args exp) newnames)
                    '()
                    refinfo
                    '()
                    '()
                    (lambda.decls exp)
                    (lambda.doc exp)
                    (lambda.body exp))))
             (env-bind-multiple! env procnames newprocnames)
             (env-bind-multiple! env bvl newnames)
             (for-each (lambda (entry)
                         (env-bind! R-table (R-entry.name entry) entry))
                       refinfo)
             (notepad-lambda-add! notepad newexp)
             (let ((newnotepad (make-notepad notepad)))
               (for-each (lambda (name rhs)
                           (lambda.defs-set!
                             newexp
                             (cons (make-definition
                                    name
                                    (copy rhs env newnotepad R-table))
                                   (lambda.defs newexp))))
                         (reverse newprocnames)
                         (map def.rhs
                              (reverse (lambda.defs exp))))
               (lambda.body-set!
                 newexp
                 (copy (lambda.body exp) env newnotepad R-table))
               (lambda.F-set! newexp (notepad-free-variables newnotepad))
               (lambda.G-set! newexp (notepad-captured-variables newnotepad)))
             (env-unbind-multiple! env procnames)
             (env-unbind-multiple! env bvl)
             (for-each (lambda (entry)
                         (env-unbind! R-table (R-entry.name entry)))
                       refinfo)
             newexp))
          ((assignment? exp)
           (let* ((oldname (assignment.lhs exp))
                  (name (env-lookup env oldname oldname))
                  (varinfo (env-lookup R-table name #f))
                  (newexp
                   (make-assignment name
                                    (copy (assignment.rhs exp) env notepad R-table))))
             (notepad-var-add! notepad name)
             (if varinfo
                 (R-entry.assignments-set!
                  varinfo
                  (cons newexp (R-entry.assignments varinfo))))
             newexp))
          ((conditional? exp)
           (make-conditional (copy (if.test exp) env notepad R-table)
                             (copy (if.then exp) env notepad R-table)
                             (copy (if.else exp) env notepad R-table)))
          ((begin? exp)
           (make-begin (map (lambda (exp) (copy exp env notepad R-table))
                            (begin.exprs exp))))
          ((variable? exp)
           (let* ((oldname (variable.name exp))
                  (name (env-lookup env oldname oldname))
                  (varinfo (env-lookup R-table name #f))
                  (newexp (make-variable name)))
             (notepad-var-add! notepad name)
             (if varinfo
                 (R-entry.references-set!
                  varinfo
                  (cons newexp (R-entry.references varinfo))))
             newexp))
          ((call? exp)
           (let ((newexp (make-call (copy (call.proc exp) env notepad R-table)
                                    (map (lambda (exp)
                                           (copy exp env notepad R-table))
                                         (call.args exp)))))
             (if (variable? (call.proc newexp))
                 (let ((varinfo
                        (env-lookup R-table
                                    (variable.name
                                     (call.proc newexp))
                                    #f)))
                   (if varinfo
                       (R-entry.calls-set!
                        varinfo
                        (cons newexp (R-entry.calls varinfo))))))
             (if (lambda? (call.proc newexp))
                 (notepad-nonescaping-add! notepad (call.proc newexp)))
             newexp))
          (else ???)))
  
  (copy exp (make-env) (make-notepad #f) (make-env)))

; For debugging.
; Given an expression, traverses the expression to confirm
; that the referencing invariants are correct.

(define (check-referencing-invariants exp . flags)
  
  (let ((check-free-variables? (memq 'free flags))
        (check-referencing? (memq 'reference flags))
        (first-violation? #t))
    
    ; env is the list of enclosing lambda expressions,
    ; beginning with the innermost.
    
    (define (check exp env)
      (cond ((constant? exp) (return exp #t))
            ((lambda? exp)
             (let ((env (cons exp env)))
               (return exp
                       (and (every? (lambda (exp)
                                      (check exp env))
                                    (map def.rhs (lambda.defs exp)))
                            (check (lambda.body exp) env)
                            (if (and check-free-variables?
                                     (not (null? env)))
                                 (subset? (difference
                                           (lambda.F exp)
                                           (make-null-terminated
                                            (lambda.args exp)))
                                          (lambda.F (car env)))
                                #t)
                            (if check-referencing?
                                (let ((env (cons exp env))
                                      (R (lambda.R exp)))
                                  (every? (lambda (formal)
                                            (or (ignored? formal)
                                                (R-entry R formal)))
                                          (make-null-terminated
                                           (lambda.args exp))))
                                #t)))))
            ((variable? exp)
             (return exp
                     (and (if (and check-free-variables?
                                   (not (null? env)))
                              (memq (variable.name exp)
                                    (lambda.F (car env)))
                              #t)
                          (if check-referencing?
                              (let ((Rinfo (lookup env (variable.name exp))))
                                (if Rinfo
                                    (memq exp (R-entry.references Rinfo))
                                    #t))
                              #t))))
            ((assignment? exp)
             (return exp
                     (and (check (assignment.rhs exp) env)
                          (if (and check-free-variables?
                                   (not (null? env)))
                              (memq (assignment.lhs exp)
                                    (lambda.F (car env)))
                              #t)
                          (if check-referencing?
                              (let ((Rinfo (lookup env (assignment.lhs exp))))
                                (if Rinfo
                                    (memq exp (R-entry.assignments Rinfo))
                                    #t))
                              #t))))
            ((conditional? exp)
             (return exp
                     (and (check (if.test exp) env)
                          (check (if.then exp) env)
                          (check (if.else exp) env))))
            ((begin? exp)
             (return exp
                     (every? (lambda (exp) (check exp env))
                             (begin.exprs exp))))
            ((call? exp)
             (return exp
                     (and (check (call.proc exp) env)
                          (every? (lambda (exp) (check exp env))
                                  (call.args exp))
                          (if (and check-referencing?
                                   (variable? (call.proc exp)))
                              (let ((Rinfo (lookup env
                                                   (variable.name 
                                                    (call.proc exp)))))
                                (if Rinfo
                                    (memq exp (R-entry.calls Rinfo))
                                    #t))
                              #t))))
            (else ???)))
    
    (define (return exp flag)
      (cond (flag
             #t)
            (first-violation?
             (set! first-violation? #f)
             (display "Violation of referencing invariants")
             (newline)
             (pretty-print (make-readable exp))
             #f)
            (else (pretty-print (make-readable exp))
                  #f)))
    
    (define (lookup env I)
      (if (null? env)
          #f
          (let ((Rinfo (R-entry (lambda.R (car env)) I)))
            (or Rinfo
                (lookup (cdr env) I)))))
    
    (if (null? flags)
        (begin (set! check-free-variables? #t)
               (set! check-referencing? #t)))
    
    (check exp '())))


; Calculating the free variable information for an expression
; as output by pass 2.  This should be faster than computing both
; the free variables and the referencing information.

(define (compute-free-variables! exp)
  
  (define empty-set (make-set '()))
  
  (define (singleton x) (list x))
  
  (define (union2 x y) (union x y))
  (define (union3 x y z) (union x y z))
  
  (define (set->list set) set)
  
  (define (free exp)
    (cond ((constant? exp) empty-set)
          ((lambda? exp)
           (let* ((defs (lambda.defs exp))
                  (formals (make-set
                            (make-null-terminated (lambda.args exp))))
                  (defined (make-set (map def.lhs defs)))
                  (Fdefs
                   (apply-union
                    (map (lambda (def)
                           (free (def.rhs def)))
                         defs)))
                  (Fbody (free (lambda.body exp)))
                  (F (union2 Fdefs Fbody)))
             (lambda.F-set! exp (set->list F))
             (lambda.G-set! exp (set->list F))
             (difference F (union2 formals defined))))
          ((assignment? exp)
           (union2 (make-set (list (assignment.lhs exp)))
                   (free (assignment.rhs exp))))
          ((conditional? exp)
           (union3 (free (if.test exp))
                   (free (if.then exp))
                   (free (if.else exp))))
          ((begin? exp)
           (apply-union
            (map (lambda (exp) (free exp))
                 (begin.exprs exp))))
          ((variable? exp)
           (singleton (variable.name exp)))
          ((call? exp)
           (union2 (free (call.proc exp))
                   (apply-union
                    (map (lambda (exp) (free exp))
                         (call.args exp)))))
          (else ???)))
  
  (free exp))

; As above, but representing sets as hashtrees.
; This is commented out because it is much slower than the implementation
; above.  Because the set of free variables is represented as a list
; within a lambda expression, this implementation must convert the
; representation for every lambda expression, which is quite expensive
; for A-normal form.

(begin
'
(define (compute-free-variables! exp)
  
  (define empty-set (make-hashtree symbol-hash assq))
  
  (define (singleton x)
    (hashtree-put empty-set x #t))
  
  (define (make-set values)
    (if (null? values)
        empty-set
        (hashtree-put (make-set (cdr values))
                      (car values)
                      #t)))
  
  (define (union2 x y)
    (hashtree-for-each (lambda (key val)
                         (set! x (hashtree-put x key #t)))
                       y)
    x)
  
  (define (union3 x y z)
    (union2 (union2 x y) z))
  
  (define (apply-union sets)
    (cond ((null? sets)
           (make-set '()))
          ((null? (cdr sets))
           (car sets))
          (else
           (union2 (car sets)
                   (apply-union (cdr sets))))))
  
  (define (difference x y)
    (hashtree-for-each (lambda (key val)
                         (set! x (hashtree-remove x key)))
                       y)
    x)
  
  (define (set->list set)
    (hashtree-map (lambda (sym val) sym) set))
  
  (define (free exp)
    (cond ((constant? exp) empty-set)
          ((lambda? exp)
           (let* ((defs (lambda.defs exp))
                  (formals (make-set
                            (make-null-terminated (lambda.args exp))))
                  (defined (make-set (map def.lhs defs)))
                  (Fdefs
                   (apply-union
                    (map (lambda (def)
                           (free (def.rhs def)))
                         defs)))
                  (Fbody (free (lambda.body exp)))
                  (F (union2 Fdefs Fbody)))
             (lambda.F-set! exp (set->list F))
             (lambda.G-set! exp (set->list F))
             (difference F (union2 formals defined))))
          ((assignment? exp)
           (union2 (make-set (list (assignment.lhs exp)))
                   (free (assignment.rhs exp))))
          ((conditional? exp)
           (union3 (free (if.test exp))
                   (free (if.then exp))
                   (free (if.else exp))))
          ((begin? exp)
           (apply-union
            (map (lambda (exp) (free exp))
                 (begin.exprs exp))))
          ((variable? exp)
           (singleton (variable.name exp)))
          ((call? exp)
           (union2 (free (call.proc exp))
                   (apply-union
                    (map (lambda (exp) (free exp))
                         (call.args exp)))))
          (else ???)))
  
  (hashtree-map (lambda (sym val) sym)
                (free exp)))
#t); Copyright 1991 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 24 April 1999
;
; First pass of the Twobit compiler:
;   macro expansion, syntax checking, alpha conversion,
;   preliminary annotation.
;
; The input to this pass is a Scheme definition or expression.
; The output is an expression in the subset of Scheme described
; by the following grammar, where the output satisfies certain
; additional invariants described below.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the output:
;   *  There are no internal definitions.
;   *  No identifier containing an upper case letter is bound anywhere.
;      (Change the "name:..." variables if upper case is preferred.)
;   *  No identifier is bound in more than one place.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.
;   *  F and G are garbage.

($$trace "pass1")

(define source-file-name #f)
(define source-file-position #f)

(define pass1-block-compiling? #f)
(define pass1-block-assignments '())
(define pass1-block-inlines '())

(define (pass1 def-or-exp . rest)
  (set! source-file-name #f)
  (set! source-file-position #f)
  (set! pass1-block-compiling? #f)
  (set! pass1-block-assignments '())
  (set! pass1-block-inlines '())
  (if (not (null? rest))
      (begin (set! source-file-name (car rest))
             (if (not (null? (cdr rest)))
                 (set! source-file-position (cadr rest)))))
  (set! renaming-counter 0)
  (macro-expand def-or-exp))

; Compiles a whole sequence of top-level forms on the assumption
; that no variable that is defined by a form in the sequence is
; ever defined or assigned outside of the sequence.
;
; This is a crock in three parts:
;
;    1.  Macro-expand each form and record assignments.
;    2.  Find the top-level variables that are defined but not
;        assigned, give them local names, generate a DEFINE-INLINE
;        for each of the top-level procedures, and macro-expand
;        each form again.
;    3.  Wrap the whole mess in an appropriate LET and recompute
;        the referencing information by copying it.
;
; Note that macros get expanded twice, and that all DEFINE-SYNTAX
; macros are considered local to the forms.

; FIXME: Need to turn off warning messages.

(define (pass1-block forms . rest)
  
  (define (part1)
    (set! pass1-block-compiling? #t)
    (set! pass1-block-assignments '())
    (set! pass1-block-inlines '())
    (set! renaming-counter 0)
    (let ((env0 (syntactic-copy global-syntactic-environment))
          (bmode (benchmark-mode))
          (wmode (issue-warnings))
          (defined '()))
      (define (make-toplevel-definition id exp)
        (cond ((memq id defined)
               (set! pass1-block-assignments
                     (cons id pass1-block-assignments)))
              ((or (constant? exp)
                   (and (lambda? exp)
                        (list? (lambda.args exp))))
               (set! defined (cons id defined))))
        (make-begin
         (list (make-assignment id exp)
               (make-constant id))))
      (benchmark-mode #f)
      (issue-warnings #f)
      (for-each (lambda (form)
                  (desugar-definitions form
                                       global-syntactic-environment
                                       make-toplevel-definition))
                forms)
      (set! global-syntactic-environment env0)
      (benchmark-mode bmode)
      (issue-warnings wmode)
      (part2 (filter (lambda (id)
                       (not (memq id pass1-block-assignments)))
                     (reverse defined)))))
  
  (define (part2 defined)
    (set! pass1-block-compiling? #f)
    (set! pass1-block-assignments '())
    (set! pass1-block-inlines '())
    (set! renaming-counter 0)
    (let* ((rename (make-rename-procedure))
           (alist (map (lambda (id)
                         (cons id (rename id)))
                       defined))
           (definitions0 '())    ; for constants
           (definitions1 '()))   ; for lambda expressions
      (define (make-toplevel-definition id exp)
        (if (lambda? exp)
            (doc.name-set! (lambda.doc exp) id))
        (let ((probe (assq id alist)))
          (if probe
              (let ((id1 (cdr probe)))
                (cond ((constant? exp)
                       (set! definitions0
                             (cons (make-assignment id exp)
                                   definitions0))
                       (make-constant id))
                      ((lambda? exp)
                       (set! definitions1
                             (cons (make-assignment id1 exp)
                                   definitions1))
                       (make-assignment
                        id
                        (make-lambda (lambda.args exp)
                                     '() ; no definitions
                                     '() ; R
                                     '() ; F
                                     '() ; G
                                     '() ; decls
                                     (lambda.doc exp)
                                     (make-call
                                      (make-variable id1)
                                      (map make-variable
                                           (lambda.args exp))))))
                      (else
                       (m-error "Inconsistent macro expansion"
                                (make-readable exp)))))
              (make-assignment id exp))))
      (let ((env0 (syntactic-copy global-syntactic-environment))
            (bmode (benchmark-mode))
            (wmode (issue-warnings)))
        (issue-warnings #f)
        (for-each (lambda (pair)
                    (let ((id0 (car pair))
                          (id1 (cdr pair)))
                      (syntactic-bind-globally!
                       id0
                       (make-inline-denotation
                        id0
                        (lambda (exp rename compare)
                          ; Deliberately non-hygienic!
                          (cons id1 (cdr exp)))
                        global-syntactic-environment))
                      (set! pass1-block-inlines
                            (cons id0 pass1-block-inlines))))
                  alist)
        (benchmark-mode #f)
        (issue-warnings wmode)
        (let ((forms
               (do ((forms forms (cdr forms))
                    (newforms '()
                              (cons (desugar-definitions
                                     (car forms)
                                     global-syntactic-environment
                                     make-toplevel-definition)
                                    newforms)))
                   ((null? forms)
                    (reverse newforms)))))
          (benchmark-mode bmode)
          (set! global-syntactic-environment env0)
          (part3 alist definitions0 definitions1 forms)))))
  
  (define (part3 alist definitions0 definitions1 forms)
    (set! pass1-block-compiling? #f)
    (set! pass1-block-assignments '())
    (set! pass1-block-inlines '())
    (let* ((constnames0 (map assignment.lhs definitions0))
           (constnames1 (map (lambda (id0)
                               (cdr (assq id0 alist)))
                             constnames0))
           (procnames1 (map assignment.lhs definitions1)))
      (copy-exp
       (make-call
        (make-lambda
         constnames1
         '() ; no definitions
         '() ; R
         '() ; F
         '() ; G
         '() ; decls
         #f  ; doc
         (make-begin
          (list
           (make-begin
            (cons (make-constant #f)
                  (reverse
                   (map (lambda (id)
                          (make-assignment id (make-variable (cdr (assq id alist)))))
                        constnames0))))
           (make-call
            (make-lambda
             constnames0
             '() ; no definitions
             '() ; R
             '() ; F
             '() ; G
             '() ; decls
             #f  ; doc
             (make-call
              (make-lambda
               (map assignment.lhs definitions1)
               '() ; no definitions
               '() ; R
               '() ; F
               '() ; G
               '() ; decls
               #f  ; doc
               (make-begin (cons (make-constant #f)
                                 (append definitions1 forms))))
              (map (lambda (ignored) (make-unspecified))
                   definitions1)))
            (map make-variable constnames1))
           )))
        (map assignment.rhs definitions0)))))
  
  (set! source-file-name #f)
  (set! source-file-position #f)
  (if (not (null? rest))
      (begin (set! source-file-name (car rest))
             (if (not (null? (cdr rest)))
                 (set! source-file-position (cadr rest)))))
  (part1))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 7 June 1999.
;
; Support for intraprocedural value numbering:
;     set of available expressions
;     miscellaneous
;
; The set of available expressions is represented as a
; mutable abstract data type Available with these operations:
;
; make-available-table:                                    -> Available
; copy-available-table: Available                          -> Available
; available-expression: Available x Expr                   -> (symbol + {#f})
; available-variable:   Available x symbol                 -> Expr
; available-extend!:    Available x symbol x Expr x Killer ->
; available-kill!:      Available x Killer                 ->
;
; where Expr is of the form
;
; Expr  -->  W
;         |  (W_0 W_1 ...)
;
; W  -->  (quote K)
;      |  (begin I)
;
; and Killer is a fixnum, as defined later in this file.
;
; (make-available-table)
;     returns an empty table of available expressions.
; (copy-available-table available)
;     copies the given table.
; (available-expression available E)
;     returns the name of E if it is available in the table, else #f.
; (available-variable available T)
;     returns a constant or variable to use in place of T, else #f.
; (available-extend! available T E K)
;     adds the binding (T E) to the table, with Killer K.
;     If E is a variable and this binding is never killed, then copy
;         propagation will replace uses of T by uses of E; otherwise
;         commoning will replace uses of E by uses of T, until the
;         binding is killed.
; (available-kill! available K)
;     removes all bindings whose Killer intersects K.
;
; (available-extend! available T E K) is very fast if the previous
; operation on the table was (available-expression available E).

; Implementation.
;
; Quick and dirty.
; The available expressions are represented as a vector of 2 association
; lists.  The first list is used for common subexpression elimination,
; and the second is used for copy and constant propagation.
;
; Each element of the first list is a binding of
; a symbol T to an expression E, with killer K,
; represented by the list (E T K).
;
; Each element of the second list is a binding of
; a symbol T to an expression E, with killer K,
; represented by the list (T E K).
; The expression E will be a constant or variable.

(define (make-available-table)
  (vector '() '()))

(define (copy-available-table available)
  (vector (vector-ref available 0)
          (vector-ref available 1)))

(define (available-expression available E)
  (let ((binding (assoc E (vector-ref available 0))))
    (if binding
        (cadr binding)
        #f)))

(define (available-variable available T)
  (let ((binding (assq T (vector-ref available 1))))
    (if binding
        (cadr binding)
        #f)))

(define (available-extend! available T E K)
  (cond ((constant? E)
         (vector-set! available
                      1
                      (cons (list T E K)
                            (vector-ref available 1))))
        ((and (variable? E)
              (eq? K available:killer:none))
         (vector-set! available
                      1
                      (cons (list T E K)
                            (vector-ref available 1))))
        (else
         (vector-set! available
                      0
                      (cons (list E T K)
                            (vector-ref available 0))))))

(define (available-kill! available K)
  (vector-set! available
               0
               (filter (lambda (binding)
                         (zero?
                          (logand K
                                  (caddr binding))))
                       (vector-ref available 0)))
  (vector-set! available
               1
               (filter (lambda (binding)
                         (zero?
                          (logand K
                                  (caddr binding))))
                       (vector-ref available 1))))

(define (available-intersect! available0 available1 available2)
  (vector-set! available0
               0
               (intersection (vector-ref available1 0)
                             (vector-ref available2 0)))
  (vector-set! available0
               1
               (intersection (vector-ref available1 1)
                             (vector-ref available2 1))))

; The Killer concrete data type, represented as a fixnum.
;
; The set of side effects that can kill an available expression
; are a subset of
;
; assignments to global variables
; uses of SET-CAR!
; uses of SET-CDR!
; uses of STRING-SET!
; uses of VECTOR-SET!
;
; This list is not complete.  If we were trying to perform common
; subexpression elimination on calls to PEEK-CHAR, for example,
; then those calls would be killed by reads.

(define available:killer:globals   2)
(define available:killer:car       4)
(define available:killer:cdr       8)
(define available:killer:string   16) ; also bytevectors etc
(define available:killer:vector   32) ; also structures etc
(define available:killer:cell     64)
(define available:killer:io      128)
(define available:killer:none      0) ; none of the above
(define available:killer:all    1022) ; all of the above

(define available:killer:immortal  0) ; never killed
(define available:killer:dead   1023) ; never available



(define (available:killer-combine k1 k2)
  (logior k1 k2))

; Miscellaneous.

; A simple lambda expression has no internal definitions at its head
; and no declarations aside from A-normal form.

(define (simple-lambda? L)
  (and (null? (lambda.defs L))
       (every? (lambda (decl)
                 (eq? decl A-normal-form-declaration))
               (lambda.decls L))))

; A real call is a call whose procedure expression is
; neither a lambda expression nor a primop.

(define (real-call? E)
  (and (call? E)
       (let ((proc (call.proc E)))
         (and (not (lambda? proc))
              (or (not (variable? proc))
                  (let ((f (variable.name proc)))
                    (or (not (integrate-usual-procedures))
                        (not (prim-entry f)))))))))

(define (prim-call E)
  (and (call? E)
       (let ((proc (call.proc E)))
         (and (variable? proc)
              (integrate-usual-procedures)
              (prim-entry (variable.name proc))))))

(define (no-side-effects? E)
  (or (constant? E)
      (variable? E)
      (lambda? E)
      (and (conditional? E)
           (no-side-effects? (if.test E))
           (no-side-effects? (if.then E))
           (no-side-effects? (if.else E)))
      (and (call? E)
           (let ((proc (call.proc E)))
             (and (variable? proc)
                  (integrate-usual-procedures)
                  (let ((entry (prim-entry (variable.name proc))))
                    (and entry
                         (not (eq? available:killer:dead
                                   (prim-lives-until entry))))))))))

; Given a local variable, the expression within its scope, and
; a list of local variables that are known to be used only once,
; returns #t if the variable is used only once.
;
; The purpose of this routine is to recognize temporaries that
; may once have had two or more uses because of CSE, but now have
; only one use because of further CSE followed by dead code elimination.

(define (temporary-used-once? T E used-once)
  (cond ((call? E)
         (let ((proc (call.proc E))
               (args (call.args E)))
           (or (and (lambda? proc)
                    (not (memq T (lambda.F proc)))
                    (and (pair? args)
                         (null? (cdr args))
                         (temporary-used-once? T (car args) used-once)))
               (do ((exprs (cons proc (call.args E))
                           (cdr exprs))
                    (n     0
                           (let ((exp (car exprs)))
                             (cond ((constant? exp)
                                    n)
                                   ((variable? exp)
                                    (if (eq? T (variable.name exp))
                                        (+ n 1)
                                        n))
                                   (else
                                    ; Terminate the loop and return #f.
                                    2)))))
                   ((or (null? exprs)
                        (> n 1))
                    (= n 1))))))
        (else
         (memq T used-once))))

; Register bindings.

(define (make-regbinding lhs rhs use)
  (list lhs rhs use))

(define (regbinding.lhs x) (car x))
(define (regbinding.rhs x) (cadr x))
(define (regbinding.use x) (caddr x))

; Given a list of register bindings, an expression E and its free variables F,
; returns two values:
;     E with the register bindings wrapped around it
;     the free variables of the wrapped expression

(define (wrap-with-register-bindings regbindings E F)
  (if (null? regbindings)
      (values E F)
      (let* ((regbinding (car regbindings))
             (R (regbinding.lhs regbinding))
             (x (regbinding.rhs regbinding)))
        (wrap-with-register-bindings
         (cdr regbindings)
         (make-call (make-lambda (list R)
                                 '()
                                 '()
                                 F
                                 F
                                 (list A-normal-form-declaration)
                                 #f
                                 E)
                    (list (make-variable x)))
         (union (list x)
                (difference F (list R)))))))

; Returns two values:
;   the subset of regbindings that have x as their right hand side
;   the rest of regbindings

(define (register-bindings regbindings x)
  (define (loop regbindings to-x others)
    (cond ((null? regbindings)
           (values to-x others))
          ((eq? x (regbinding.rhs (car regbindings)))
           (loop (cdr regbindings)
                 (cons (car regbindings) to-x)
                 others))
          (else
           (loop (cdr regbindings)
                 to-x
                 (cons (car regbindings) others)))))
  (loop regbindings '() '()))

; This procedure is called when the compiler can tell that an assertion
; is never true.

(define (declaration-error E)
  (if (issue-warnings)
      (begin (display "WARNING: Assertion is false: ")
             (write (make-readable E #t))
             (newline))))
; Representations, which form a subtype hierarchy.
;
; <rep>  ::=  <fixnum>  |  (<fixnum> <datum> ...)
;
; (<rep> <datum> ...) is a subtype of <rep>, but the non-fixnum
; representations are otherwise interpreted by arbitrary code.

(define *nreps* 0)
(define *rep-encodings* '())
(define *rep-decodings* '())
(define *rep-subtypes* '())
(define *rep-joins* (make-bytevector 0))
(define *rep-meets* (make-bytevector 0))
(define *rep-joins-special* '#())
(define *rep-meets-special* '#())

(define (representation-error msg . stuff)
  (apply error
         (if (string? msg)
             (string-append "Bug in flow analysis: " msg)
             msg)
         stuff))

(define (symbol->rep sym)
  (let ((probe (assq sym *rep-encodings*)))
    (if probe
        (cdr probe)
        (let ((rep *nreps*))
          (set! *nreps* (+ *nreps* 1))
          (if (> *nreps* 255)
              (representation-error "Too many representation types"))
          (set! *rep-encodings*
                (cons (cons sym rep)
                      *rep-encodings*))
          (set! *rep-decodings*
                (cons (cons rep sym)
                      *rep-decodings*))
          rep))))

(define (rep->symbol rep)
  (if (pair? rep)
      (cons (rep->symbol (car rep)) (cdr rep))
      (let ((probe (assv rep *rep-decodings*)))
        (if probe
            (cdr probe)
            'unknown))))

(define (representation-table table)
  (map (lambda (row)
         (map (lambda (x)
                (if (list? x)
                    (map symbol->rep x)
                    x))
              row))
       table))

; DEFINE-SUBTYPE is how representation types are defined.

(define (define-subtype sym1 sym2)
  (let* ((rep2 (symbol->rep sym2))
         (rep1 (symbol->rep sym1)))
    (set! *rep-subtypes*
          (cons (cons rep1 rep2)
                *rep-subtypes*))
    sym1))

; COMPUTE-TYPE-STRUCTURE! must be called before DEFINE-INTERSECTION.

(define (define-intersection sym1 sym2 sym3)
  (let ((rep1 (symbol->rep sym1))
        (rep2 (symbol->rep sym2))
        (rep3 (symbol->rep sym3)))
    (representation-aset! *rep-meets* rep1 rep2 rep3)
    (representation-aset! *rep-meets* rep2 rep1 rep3)))

;

(define (representation-aref bv i j)
  (bytevector-ref bv (+ (* *nreps* i) j)))

(define (representation-aset! bv i j x)
  (bytevector-set! bv (+ (* *nreps* i) j) x))

(define (compute-unions!)
  
  ; Always define a bottom element.
  
  (for-each (lambda (sym)
              (define-subtype 'bottom sym))
            (map car *rep-encodings*))
  
  (let* ((debugging? #f)
         (n *nreps*)
         (n^2 (* n n))
         (matrix (make-bytevector n^2)))
    
    ; This code assumes there will always be a top element.
    
    (define (lub rep1 rep2 subtype?)
      (do ((i 0 (+ i 1))
           (bounds '()
                   (if (and (subtype? rep1 i)
                            (subtype? rep2 i))
                       (cons i bounds)
                       bounds)))
          ((= i n)
           (car (twobit-sort subtype? bounds)))))
    
    (define (join i j)
      (lub i j (lambda (rep1 rep2)
                 (= 1 (representation-aref matrix rep1 rep2)))))
    
    (define (compute-transitive-closure!)
      (let ((changed? #f))
        (define (loop)
          (do ((i 0 (+ i 1)))
              ((= i n))
              (do ((k 0 (+ k 1)))
                  ((= k n))
                  (do ((j 0 (+ j 1))
                       (sum 0
                            (logior sum
                                    (logand
                                     (representation-aref matrix i j)
                                     (representation-aref matrix j k)))))
                      ((= j n)
                       (if (> sum 0)
                           (let ((x (representation-aref matrix i k)))
                             (if (zero? x)
                                 (begin
                                  (set! changed? #t)
                                  (representation-aset! matrix i k 1)))))))))
          (if changed?
              (begin (set! changed? #f)
                     (loop))))
        (loop)))
    
    (define (compute-joins!)
      (let ((default (lambda (x y)
                       (error "Compiler bug: special meet or join" x y))))
        (set! *rep-joins-special* (make-vector n default))
        (set! *rep-meets-special* (make-vector n default)))
      (set! *rep-joins* (make-bytevector n^2))
      (set! *rep-meets* (make-bytevector n^2))
      (do ((i 0 (+ i 1)))
          ((= i n))
          (do ((j 0 (+ j 1)))
              ((= j n))
              (representation-aset! *rep-joins*
                                    i
                                    j
                                    (join i j)))))
    
    (do ((i 0 (+ i 1)))
        ((= i n))
        (do ((j 0 (+ j 1)))
            ((= j n))
            (representation-aset! matrix i j 0))
        (representation-aset! matrix i i 1))
    (for-each (lambda (subtype)
                (let ((rep1 (car subtype))
                      (rep2 (cdr subtype)))
                  (representation-aset! matrix rep1 rep2 1)))
              *rep-subtypes*)
    (compute-transitive-closure!)
    (if debugging?
        (do ((i 0 (+ i 1)))
            ((= i n))
            (do ((j 0 (+ j 1)))
                ((= j n))
                (write-char #\space)
                (write (representation-aref matrix i j)))
            (newline)))
    (compute-joins!)
    (set! *rep-subtypes* '())))

; Intersections are not dual to unions because a conservative analysis
; must always err on the side of the larger subtype.
; COMPUTE-UNIONS! must be called before COMPUTE-INTERSECTIONS!.

(define (compute-intersections!)
  (let ((n *nreps*))
    
    (define (meet i j)
      (let ((k (representation-union i j)))
        (if (= i k)
            j
            i)))
    
    (do ((i 0 (+ i 1)))
        ((= i n))
        (do ((j 0 (+ j 1)))
            ((= j n))
            (representation-aset! *rep-meets*
                                  i
                                  j
                                  (meet i j))))))

(define (compute-type-structure!)
  (compute-unions!)
  (compute-intersections!))

(define (representation-subtype? rep1 rep2)
  (equal? rep2 (representation-union rep1 rep2)))

(define (representation-union rep1 rep2)
  (if (fixnum? rep1)
      (if (fixnum? rep2)
          (representation-aref *rep-joins* rep1 rep2)
          (representation-union rep1 (car rep2)))
      (if (fixnum? rep2)
          (representation-union (car rep1) rep2)
          (let ((r1 (car rep1))
                (r2 (car rep2)))
            (if (= r1 r2)
                ((vector-ref *rep-joins-special* r1) rep1 rep2)
                (representation-union r1 r2))))))

(define (representation-intersection rep1 rep2)
  (if (fixnum? rep1)
      (if (fixnum? rep2)
          (representation-aref *rep-meets* rep1 rep2)
          (representation-intersection rep1 (car rep2)))
      (if (fixnum? rep2)
          (representation-intersection (car rep1) rep2)
          (let ((r1 (car rep1))
                (r2 (car rep2)))
            (if (= r1 r2)
                ((vector-ref *rep-meets-special* r1) rep1 rep2)
                (representation-intersection r1 r2))))))

; For debugging.

(define (display-unions-and-intersections)
  (let* ((column-width 10)
         (columns/row (quotient 80 column-width)))
    
    (define (display-symbol sym)
      (let* ((s (symbol->string sym))
             (n (string-length s)))
        (if (< n column-width)
            (begin (display s)
                   (display (make-string (- column-width n) #\space)))
            (begin (display (substring s 0 (- column-width 1)))
                   (write-char #\space)))))
    
    ; Display columns i to n.
    
    (define (display-matrix f i n)
      (display (make-string column-width #\space))
      (do ((i i (+ i 1)))
          ((= i n))
          (display-symbol (rep->symbol i)))
      (newline)
      (newline)
      (do ((k 0 (+ k 1)))
          ((= k *nreps*))
          (display-symbol (rep->symbol k))
          (do ((i i (+ i 1)))
              ((= i n))
              (display-symbol (rep->symbol (f k i))))
          (newline))
      (newline)
      (newline))
    
    (display "Unions:")
    (newline)
    (newline)
    
    (do ((i 0 (+ i columns/row)))
        ((>= i *nreps*))
        (display-matrix representation-union
                        i
                        (min *nreps* (+ i columns/row))))
    
    (display "Intersections:")
    (newline)
    (newline)
    
    (do ((i 0 (+ i columns/row)))
        ((>= i *nreps*))
        (display-matrix representation-intersection
                        i
                        (min *nreps* (+ i columns/row))))))

; Operations that can be specialized.
;
; Format: (<name> (<arg-rep> ...) <specific-name>)

(define (rep-specific? f rs)
  (rep-match f rs rep-specific caddr))

; Operations whose result has some specific representation.
;
; Format: (<name> (<arg-rep> ...) (<result-rep>))

(define (rep-result? f rs)
  (rep-match f rs rep-result caaddr))

; Unary predicates that give information about representation.
;
; Format: (<name> <rep-if-true> <rep-if-false>)

(define (rep-if-true f rs)
  (rep-match f rs rep-informing caddr))

(define (rep-if-false f rs)
  (rep-match f rs rep-informing cadddr))

; Given the name of an integrable primitive,
; the representations of its arguments,
; a representation table, and a selector function
; finds the most type-specific row of the table that matches both
; the name of the primitive and the representations of its arguments,
; and returns the result of applying the selector to that row.
; If no row matches, then REP-MATCH returns #f.
;
; FIXME:  This should be more efficient, and should prefer the most
; specific matches.

(define (rep-match f rs table selector)
  (let ((n (length rs)))
    (let loop ((entries table))
      (cond ((null? entries)
             #f)
            ((eq? f (car (car entries)))
             (let ((rs0 (cadr (car entries))))
               (if (and (= n (length rs0))
                        (every? (lambda (r1+r2)
                                  (let ((r1 (car r1+r2))
                                        (r2 (cdr r1+r2)))
                                    (representation-subtype? r1 r2)))
                                (map cons rs rs0)))
                   (selector (car entries))
                   (loop (cdr entries)))))
            (else
             (loop (cdr entries)))))))

; Abstract interpretation with respect to types and constraints.
; Returns a representation type.

(define (aeval E types constraints)
  (cond ((call? E)
         (let ((proc (call.proc E)))
           (if (variable? proc)
               (let* ((op (variable.name proc))
                      (argtypes (map (lambda (E)
                                       (aeval E types constraints))
                                     (call.args E)))
                      (type (rep-result? op argtypes)))
                 (if type
                     type
                     rep:object))
               rep:object)))
        ((variable? E)
         (representation-typeof (variable.name E) types constraints))
        ((constant? E)
         (representation-of-value (constant.value E)))
        (else
         rep:object)))

; If x has representation type t0 in the hash table,
; and some further constraints
;
;     x = (op y1 ... yn)
;     x : t1
;      ...
;     x : tk
;
; then
;
;     typeof (x) = op (typeof (y1), ..., typeof (yn))
;                  &  t0  &  t1  &  ...  &  tk
;
; where & means intersection and op is the abstraction of op.
;
; Also if T : true and T = E then E may give information about
; the types of other variables.  Similarly for T : false.

(define (representation-typeof name types constraints)
  (let ((t0 (hashtable-fetch types name rep:object))
        (cs (hashtable-fetch (constraints.table constraints) name '())))
    (define (loop type cs)
      (if (null? cs)
          type
          (let* ((c (car cs))
                 (cs (cdr cs))
                 (E (constraint.rhs c)))
            (cond ((constant? E)
                   (loop (representation-intersection type
                                                      (constant.value E))
                         cs))
                  ((call? E)
                   (loop (representation-intersection
                          type (aeval E types constraints))
                         cs))
                  (else
                   (loop type cs))))))
    (loop t0 cs)))

; Constraints.
;
; The constraints used by this analysis consist of type constraints
; together with the available expressions used for commoning.
;
; (T E      K)   T = E     until killed by an effect in K
; (T '<rep> K)   T : <rep> until killed by an effect in K

(define (make-constraint T E K)
  (list T E K))

(define (constraint.lhs c)
  (car c))

(define (constraint.rhs c)
  (cadr c))

(define (constraint.killer c)
  (caddr c))

(define (make-type-constraint T type K)
  (make-constraint T
                   (make-constant type)
                   K))

; If the new constraint is of the form T = E until killed by K,
; then there shouldn't be any prior constraints.
;
; Otherwise the new constraint is of the form T : t until killed by K.
; Suppose the prior constraints are
;     T = E  until killed by K
;     T : t1 until killed by K1
;      ...
;     T : tn until killed by Kn
;
; If there exists i such that ti is a subtype of t and Ki a subset of K,
; then the new constraint adds no new information and should be ignored.
; Otherwise compute t' = t1 & ... & tn and K' = K1 | ... | Kn, where
; & indicates intersection and | indicates union.
; If K = K' then add the new constraint T : t' until killed by K;
; otherwise add two new constraints:
;     T : t' until killed by K'
;     T : t  until killed by K

(define (constraints-add! types constraints new)
  (let* ((debugging? #f)
         (T (constraint.lhs new))
         (E (constraint.rhs new))
         (K (constraint.killer new))
         (cs (constraints-for-variable constraints T)))
    
    (define (loop type K cs newcs)
      (if (null? cs)
          (cons (make-type-constraint T type K) newcs)
          (let* ((c2 (car cs))
                 (cs (cdr cs))
                 (E2 (constraint.rhs c2))
                 (K2 (constraint.killer c2)))
            (if (constant? E2)
                (let* ((type2 (constant.value E2))
                       (type3 (representation-intersection type type2)))
                  (cond ((eq? type2 type3)
                         (if (= K2 (logand K K2))
                             (append newcs cs)
                             (loop (representation-intersection type type2)
                                   (available:killer-combine K K2)
                                   cs
                                   (cons c2 newcs))))
                        ((representation-subtype? type type3)
                         (if (= K (logand K K2))
                             (loop type K cs newcs)
                             (loop type K cs (cons c2 newcs))))
                        (else
                         (loop type3
                               (available:killer-combine K K2)
                               cs
                               (cons c2 newcs)))))
                (let* ((op (variable.name (call.proc E2)))
                       (args (call.args E2))
                       (argtypes (map (lambda (exp)
                                        (aeval exp types constraints))
                                      args)))
                  (cond ((representation-subtype? type rep:true)
                         (let ((reps (rep-if-true op argtypes)))
                           (if reps
                               (record-new-reps! args argtypes reps K2))))
                        ((representation-subtype? type rep:false)
                         (let ((reps (rep-if-false op argtypes)))
                           (if reps
                               (record-new-reps! args argtypes reps K2)))))
                  (loop type K cs (cons c2 newcs)))))))
    
    (define (record-new-reps! args argtypes reps K2)
      (if debugging?
          (begin (write (list (map make-readable args)
                              (map rep->symbol argtypes)
                              (map rep->symbol reps)))
                 (newline)))
      (for-each (lambda (arg type0 type1)
                  (if (not (representation-subtype? type0 type1))
                      (if (variable? arg)
                          (let ((name (variable.name arg)))
                            ; FIXME:  In this context, a variable
                            ; should always be local so the hashtable
                            ; operation isn't necessary.
                            (if (hashtable-get types name)
                                (constraints-add!
                                 types
                                 constraints
                                 (make-type-constraint
                                  name
                                  type1 
                                  (available:killer-combine K K2)))
                                (cerror
                                 "Compiler bug: unexpected global: "
                                 name))))))
                args argtypes reps))
    
    (if (not (zero? K))
        (constraints-add-killedby! constraints T K))
    
    (let* ((table (constraints.table constraints))
           (cs (hashtable-fetch table T '())))
      (cond ((constant? E)
             ; It's a type constraint.
             (let ((type (constant.value E)))
               (if debugging?
                   (begin (display T)
                          (display " : ")
                          (display (rep->symbol type))
                          (newline)))
               (let ((cs (loop type K cs '())))
                 (hashtable-put! table T cs)
                 constraints)))
            (else
             (if debugging?
                 (begin (display T)
                        (display " = ")
                        (display (make-readable E #t))
                        (newline)))
             (if (not (null? cs))
                 (begin
                  (display "Compiler bug: ")
                  (write T)
                  (display " has unexpectedly nonempty constraints")
                  (newline)))
             (hashtable-put! table T (list (list T E K)))
             constraints)))))

; Sets of constraints.
;
; The set of constraints is represented as (<hashtable> <killedby>),
; where <hashtable> is a hashtable mapping variables to lists of
; constraints as above, and <killedby> is a vector mapping basic killers
; to lists of variables that need to be examined for constraints that
; are killed by that basic killer.

(define number-of-basic-killers
  (do ((i 0 (+ i 1))
       (k 1 (+ k k)))
      ((> k available:killer:dead)
       i)))

(define (constraints.table  constraints) (car constraints))
(define (constraints.killed constraints) (cadr constraints))

(define (make-constraints-table)
  (list (make-hashtable symbol-hash assq)
        (make-vector number-of-basic-killers '())))

(define (copy-constraints-table constraints)
  (list (hashtable-copy (constraints.table constraints) #t)
        (list->vector (vector->list (constraints.killed constraints)))))

(define (constraints-for-variable constraints T)
  (hashtable-fetch (constraints.table constraints) T '()))

(define (constraints-add-killedby! constraints T K0)
  (if (not (zero? K0))
      (let ((v (constraints.killed constraints)))
        (do ((i 0 (+ i 1))
             (k 1 (+ k k)))
            ((= i number-of-basic-killers))
            (if (not (zero? (logand k K0)))
                (vector-set! v i (cons T (vector-ref v i))))))))

(define (constraints-kill! constraints K)
  (if (not (zero? K))
      (let ((table (constraints.table constraints))
            (killed (constraints.killed constraints)))
        (define (examine! T)
          (let ((cs (filter (lambda (c)
                              (zero? (logand (constraint.killer c) K)))
                            (hashtable-fetch table T '()))))
            (if (null? cs)
                (hashtable-remove! table T)
                (hashtable-put! table T cs))))
        (do ((i 0 (+ i 1))
             (j 1 (+ j j)))
            ((= i number-of-basic-killers))
            (if (not (zero? (logand j K)))
                (begin (for-each examine! (vector-ref killed i))
                       (vector-set! killed i '())))))))

(define (constraints-intersect! constraints0 constraints1 constraints2)
  (let ((table0 (constraints.table constraints0))
        (table1 (constraints.table constraints1))
        (table2 (constraints.table constraints2)))
    (if (eq? table0 table1)
        ; FIXME:  Which is more efficient: to update the killed vector,
        ; or not to update it?  Both are safe.
        (hashtable-for-each (lambda (T cs)
                              (if (not (null? cs))
                                  (hashtable-put!
                                   table0
                                   T
                                   (cs-intersect
                                    (hashtable-fetch table2 T '())
                                    cs))))
                            table1)
        ; This case shouldn't ever happen, so it can be slow.
        (begin
         (constraints-intersect! constraints0 constraints0 constraints1)
         (constraints-intersect! constraints0 constraints0 constraints2)))))

(define (cs-intersect cs1 cs2)
  (define (loop cs init rep Krep)
    (if (null? cs)
        (values init rep Krep)
        (let* ((c (car cs))
               (cs (cdr cs))
               (E2 (constraint.rhs c))
               (K2 (constraint.killer c)))
          (cond ((constant? E2)
                 (loop cs
                       init
                       (representation-intersection rep (constant.value E2))
                       (available:killer-combine Krep K2)))
                ((call? E2)
                 (if init
                     (begin (display "Compiler bug in cs-intersect")
                            (break))
                     (loop cs c rep Krep)))
                (else
                 (error "Compiler bug in cs-intersect"))))))
  (call-with-values
   (lambda ()
     (loop cs1 #f rep:object available:killer:none))
   (lambda (c1 rep1 Krep1)
     (call-with-values
      (lambda ()
        (loop cs2 #f rep:object available:killer:none))
      (lambda (c2 rep2 Krep2)
        (let ((c (if (equal? c1 c2) c1 #f))
              (rep (representation-union rep1 rep2))
              (Krep (available:killer-combine Krep1 Krep2)))
          (if (eq? rep rep:object)
              (if c (list c) '())
              (let ((T (constraint.lhs (car cs1))))
                (if c
                    (list c (make-type-constraint T rep Krep))
                    (list (make-type-constraint T rep Krep)))))))))))
; DO NOT EDIT THIS FILE. Edit the config file and rerun "config".

(define $gc.ephemeral 0)
(define $gc.tenuring 1)
(define $gc.full 2)
(define $mstat.wallocated-hi 0)
(define $mstat.wallocated-lo 1)
(define $mstat.wcollected-hi 2)
(define $mstat.wcollected-lo 3)
(define $mstat.wcopied-hi 4)
(define $mstat.wcopied-lo 5)
(define $mstat.gctime 6)
(define $mstat.wlive 7)
(define $mstat.gc-last-gen 8)
(define $mstat.gc-last-type 9)
(define $mstat.generations 10)
(define $mstat.g-gc-count 0)
(define $mstat.g-prom-count 1)
(define $mstat.g-gctime 2)
(define $mstat.g-wlive 3)
(define $mstat.g-np-youngp 4)
(define $mstat.g-np-oldp 5)
(define $mstat.g-np-j 6)
(define $mstat.g-np-k 7)
(define $mstat.g-alloc 8)
(define $mstat.g-target 9)
(define $mstat.g-promtime 10)
(define $mstat.remsets 11)
(define $mstat.r-apool 0)
(define $mstat.r-upool 1)
(define $mstat.r-ahash 2)
(define $mstat.r-uhash 3)
(define $mstat.r-hrec-hi 4)
(define $mstat.r-hrec-lo 5)
(define $mstat.r-hrem-hi 6)
(define $mstat.r-hrem-lo 7)
(define $mstat.r-hscan-hi 8)
(define $mstat.r-hscan-lo 9)
(define $mstat.r-wscan-hi 10)
(define $mstat.r-wscan-lo 11)
(define $mstat.r-ssbrec-hi 12)
(define $mstat.r-ssbrec-lo 13)
(define $mstat.r-np-p 14)
(define $mstat.fflushed-hi 12)
(define $mstat.fflushed-lo 13)
(define $mstat.wflushed-hi 14)
(define $mstat.wflushed-lo 15)
(define $mstat.stk-created 16)
(define $mstat.frestored-hi 17)
(define $mstat.frestored-lo 18)
(define $mstat.words-heap 19)
(define $mstat.words-remset 20)
(define $mstat.words-rts 21)
(define $mstat.swb-assign 22)
(define $mstat.swb-lhs-ok 23)
(define $mstat.swb-rhs-const 24)
(define $mstat.swb-not-xgen 25)
(define $mstat.swb-trans 26)
(define $mstat.rtime 27)
(define $mstat.stime 28)
(define $mstat.utime 29)
(define $mstat.minfaults 30)
(define $mstat.majfaults 31)
(define $mstat.np-remsetp 32)
(define $mstat.max-heap 33)
(define $mstat.promtime 34)
(define $mstat.wmoved-hi 35)
(define $mstat.wmoved-lo 36)
(define $mstat.vsize 37)
(define $g.reg0 12)
(define $r.reg8 44)
(define $r.reg9 48)
(define $r.reg10 52)
(define $r.reg11 56)
(define $r.reg12 60)
(define $r.reg13 64)
(define $r.reg14 68)
(define $r.reg15 72)
(define $r.reg16 76)
(define $r.reg17 80)
(define $r.reg18 84)
(define $r.reg19 88)
(define $r.reg20 92)
(define $r.reg21 96)
(define $r.reg22 100)
(define $r.reg23 104)
(define $r.reg24 108)
(define $r.reg25 112)
(define $r.reg26 116)
(define $r.reg27 120)
(define $r.reg28 124)
(define $r.reg29 128)
(define $r.reg30 132)
(define $r.reg31 136)
(define $g.stkbot 180)
(define $g.gccnt 420)
(define $m.alloc 1024)
(define $m.alloci 1032)
(define $m.gc 1040)
(define $m.addtrans 1048)
(define $m.stkoflow 1056)
(define $m.stkuflow 1072)
(define $m.creg 1080)
(define $m.creg-set! 1088)
(define $m.add 1096)
(define $m.subtract 1104)
(define $m.multiply 1112)
(define $m.quotient 1120)
(define $m.remainder 1128)
(define $m.divide 1136)
(define $m.modulo 1144)
(define $m.negate 1152)
(define $m.numeq 1160)
(define $m.numlt 1168)
(define $m.numle 1176)
(define $m.numgt 1184)
(define $m.numge 1192)
(define $m.zerop 1200)
(define $m.complexp 1208)
(define $m.realp 1216)
(define $m.rationalp 1224)
(define $m.integerp 1232)
(define $m.exactp 1240)
(define $m.inexactp 1248)
(define $m.exact->inexact 1256)
(define $m.inexact->exact 1264)
(define $m.make-rectangular 1272)
(define $m.real-part 1280)
(define $m.imag-part 1288)
(define $m.sqrt 1296)
(define $m.round 1304)
(define $m.truncate 1312)
(define $m.apply 1320)
(define $m.varargs 1328)
(define $m.typetag 1336)
(define $m.typetag-set 1344)
(define $m.break 1352)
(define $m.eqv 1360)
(define $m.partial-list->vector 1368)
(define $m.timer-exception 1376)
(define $m.exception 1384)
(define $m.singlestep 1392)
(define $m.syscall 1400)
(define $m.bvlcmp 1408)
(define $m.enable-interrupts 1416)
(define $m.disable-interrupts 1424)
(define $m.alloc-bv 1432)
(define $m.global-ex 1440)
(define $m.invoke-ex 1448)
(define $m.global-invoke-ex 1456)
(define $m.argc-ex 1464)
; DO NOT EDIT THIS FILE. Edit the config file and rerun "config".

(define $r.g0 0)
(define $r.g1 1)
(define $r.g2 2)
(define $r.g3 3)
(define $r.g4 4)
(define $r.g5 5)
(define $r.g6 6)
(define $r.g7 7)
(define $r.o0 8)
(define $r.o1 9)
(define $r.o2 10)
(define $r.o3 11)
(define $r.o4 12)
(define $r.o5 13)
(define $r.o6 14)
(define $r.o7 15)
(define $r.l0 16)
(define $r.l1 17)
(define $r.l2 18)
(define $r.l3 19)
(define $r.l4 20)
(define $r.l5 21)
(define $r.l6 22)
(define $r.l7 23)
(define $r.i0 24)
(define $r.i1 25)
(define $r.i2 26)
(define $r.i3 27)
(define $r.i4 28)
(define $r.i5 29)
(define $r.i6 30)
(define $r.i7 31)
(define $r.result $r.o0)
(define $r.argreg2 $r.o1)
(define $r.argreg3 $r.o2)
(define $r.stkp $r.o3)
(define $r.stklim $r.i0)
(define $r.tmp1 $r.o4)
(define $r.tmp2 $r.o5)
(define $r.tmp0 $r.g1)
(define $r.e-top $r.i0)
(define $r.e-limit $r.o3)
(define $r.timer $r.i4)
(define $r.millicode $r.i7)
(define $r.globals $r.i7)
(define $r.reg0 $r.l0)
(define $r.reg1 $r.l1)
(define $r.reg2 $r.l2)
(define $r.reg3 $r.l3)
(define $r.reg4 $r.l4)
(define $r.reg5 $r.l5)
(define $r.reg6 $r.l6)
(define $r.reg7 $r.l7)
; DO NOT EDIT THIS FILE. Edit the config file and rerun "config".

(define $ex.car 0)
(define $ex.cdr 1)
(define $ex.setcar 2)
(define $ex.setcdr 3)
(define $ex.add 10)
(define $ex.sub 11)
(define $ex.mul 12)
(define $ex.div 13)
(define $ex.lessp 14)
(define $ex.lesseqp 15)
(define $ex.equalp 16)
(define $ex.greatereqp 17)
(define $ex.greaterp 18)
(define $ex.quotient 19)
(define $ex.remainder 20)
(define $ex.modulo 21)
(define $ex.logior 22)
(define $ex.logand 23)
(define $ex.logxor 24)
(define $ex.lognot 25)
(define $ex.lsh 26)
(define $ex.rsha 27)
(define $ex.rshl 28)
(define $ex.e2i 29)
(define $ex.i2e 30)
(define $ex.exactp 31)
(define $ex.inexactp 32)
(define $ex.round 33)
(define $ex.trunc 34)
(define $ex.zerop 35)
(define $ex.neg 36)
(define $ex.abs 37)
(define $ex.realpart 38)
(define $ex.imagpart 39)
(define $ex.vref 40)
(define $ex.vset 41)
(define $ex.vlen 42)
(define $ex.pref 50)
(define $ex.pset 51)
(define $ex.plen 52)
(define $ex.sref 60)
(define $ex.sset 61)
(define $ex.slen 62)
(define $ex.bvref 70)
(define $ex.bvset 71)
(define $ex.bvlen 72)
(define $ex.bvlref 80)
(define $ex.bvlset 81)
(define $ex.bvllen 82)
(define $ex.vlref 90)
(define $ex.vlset 91)
(define $ex.vllen 92)
(define $ex.typetag 100)
(define $ex.typetagset 101)
(define $ex.apply 102)
(define $ex.argc 103)
(define $ex.vargc 104)
(define $ex.nonproc 105)
(define $ex.undef-global 106)
(define $ex.dump 107)
(define $ex.dumpfail 108)
(define $ex.timer 109)
(define $ex.unsupported 110)
(define $ex.int2char 111)
(define $ex.char2int 112)
(define $ex.mkbvl 113)
(define $ex.mkvl 114)
(define $ex.char<? 115)
(define $ex.char<=? 116)
(define $ex.char=? 117)
(define $ex.char>? 118)
(define $ex.char>=? 119)
(define $ex.bvfill 120)
(define $ex.enable-interrupts 121)
(define $ex.keyboard-interrupt 122)
(define $ex.arithmetic-exception 123)
(define $ex.global-invoke 124)
(define $ex.fx+ 140)
(define $ex.fx- 141)
(define $ex.fx-- 142)
(define $ex.fx= 143)
(define $ex.fx< 144)
(define $ex.fx<= 145)
(define $ex.fx> 146)
(define $ex.fx>= 147)
(define $ex.fxpositive? 148)
(define $ex.fxnegative? 149)
(define $ex.fxzero? 150)
(define $ex.fx* 151)
; DO NOT EDIT THIS FILE. Edit the config file and rerun "config".

(define $tag.tagmask 7)
(define $tag.pair-tag 1)
(define $tag.vector-tag 3)
(define $tag.bytevector-tag 5)
(define $tag.procedure-tag 7)
(define $imm.vector-header 162)
(define $imm.bytevector-header 194)
(define $imm.procedure-header 254)
(define $imm.true 6)
(define $imm.false 2)
(define $imm.null 10)
(define $imm.unspecified 278)
(define $imm.eof 534)
(define $imm.undefined 790)
(define $imm.character 38)
(define $tag.vector-typetag 0)
(define $tag.rectnum-typetag 4)
(define $tag.ratnum-typetag 8)
(define $tag.symbol-typetag 12)
(define $tag.port-typetag 16)
(define $tag.structure-typetag 20)
(define $tag.bytevector-typetag 0)
(define $tag.string-typetag 4)
(define $tag.flonum-typetag 8)
(define $tag.compnum-typetag 12)
(define $tag.bignum-typetag 16)
(define $hdr.port 178)
(define $hdr.struct 182)
(define $p.codevector -3)
(define $p.constvector 1)
(define $p.linkoffset 5)
(define $p.reg0 5)
(define $p.codeoffset -1)
; Copyright 1991 William Clinger
;
; Relatively target-independent information for Twobit's backend.
;
; 24 April 1999 / wdc
;
; Most of the definitions in this file can be extended or overridden by
; target-specific definitions.

(define twobit-sort
  (lambda (less? list) (compat:sort list less?)))

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.
; The expansion of quasiquote uses .cons and .list directly, so these
; should not be changed willy-nilly.
; Others may be used directly by a DEFINE-INLINE.

(define name:CHECK!  '.check!)
(define name:CONS '.cons)
(define name:LIST '.list)
(define name:MAKE-CELL '.make-cell)
(define name:CELL-REF '.cell-ref)
(define name:CELL-SET! '.cell-set!)
(define name:IGNORED (string->symbol "IGNORED"))
(define name:CAR '.car)
(define name:CDR '.cdr)

;(begin (eval `(define ,name:CONS cons))
;       (eval `(define ,name:LIST list))
;       (eval `(define ,name:MAKE-CELL list))
;       (eval `(define ,name:CELL-REF car))
;       (eval `(define ,name:CELL-SET! set-car!)))

; If (INTEGRATE-USUAL-PROCEDURES) is true, then control optimization
; recognizes calls to these procedures.

(define name:NOT 'not)
(define name:MEMQ 'memq)
(define name:MEMV 'memv)

; If (INTEGRATE-USUAL-PROCEDURES) is true, then control optimization
; recognizes calls to these procedures and also creates calls to them.

(define name:EQ? 'eq?)
(define name:EQV? 'eqv?)

; Control optimization creates calls to these procedures,
; which do not need to check their arguments.

(define name:FIXNUM?       'fixnum?)
(define name:CHAR?         'char?)
(define name:SYMBOL?       'symbol?)
(define name:FX<           '<:fix:fix)
(define name:FX-           'fx-)                   ; non-checking version
(define name:CHAR->INTEGER 'char->integer)         ; non-checking version
(define name:VECTOR-REF    'vector-ref:trusted)


; Constant folding.
; Prototype, will probably change in the future.

(define (constant-folding-entry name)
  (assq name $usual-constant-folding-procedures$))

(define constant-folding-predicates cadr)
(define constant-folding-folder caddr)

(define $usual-constant-folding-procedures$
  (let ((always? (lambda (x) #t))
        (charcode? (lambda (n)
                     (and (number? n)
                          (exact? n)
                          (<= 0 n)
                          (< n 128))))
        (ratnum? (lambda (n)
                   (and (number? n)
                        (exact? n)
                        (rational? n))))
        ; smallint? is defined later.
        (smallint? (lambda (n) (smallint? n))))
    `(
      ; This makes some assumptions about the host system.
      
      (integer->char (,charcode?) ,integer->char)
      (char->integer (,char?) ,char->integer)
      (zero? (,ratnum?) ,zero?)
      (< (,ratnum? ,ratnum?) ,<)
      (<= (,ratnum? ,ratnum?) ,<=)
      (= (,ratnum? ,ratnum?) ,=)
      (>= (,ratnum? ,ratnum?) ,>=)
      (> (,ratnum? ,ratnum?) ,>)
      (+ (,ratnum? ,ratnum?) ,+)
      (- (,ratnum? ,ratnum?) ,-)
      (* (,ratnum? ,ratnum?) ,*)
      (-- (,ratnum?) ,(lambda (x) (- 0 x)))
      (eq? (,always? ,always?) ,eq?)
      (eqv? (,always? ,always?) ,eqv?)
      (equal? (,always? ,always?) ,equal?)
      (memq (,always? ,list?) ,memq)
      (memv (,always? ,list?) ,memv)
      (member (,always? ,list?) ,member)
      (assq (,always? ,list?) ,assq)
      (assv (,always? ,list?) ,assv)
      (assoc (,always? ,list?) ,assoc)
      (length (,list?) ,length)
      (fixnum? (,smallint?) ,smallint?)
      (=:fix:fix  (,smallint? ,smallint?) ,=)
      (<:fix:fix  (,smallint? ,smallint?) ,<)
      (<=:fix:fix (,smallint? ,smallint?) ,<=)
      (>:fix:fix  (,smallint? ,smallint?) ,>)
      (>=:fix:fix (,smallint? ,smallint?) ,>=)
      )))

(begin '
       (define (.check! flag exn . args)
         (if (not flag)
             (apply error "Runtime check exception: " exn args)))
       #t)

; Order matters.  If f and g are both inlined, and the definition of g
; uses f, then f should be defined before g.

(for-each pass1
          `(

(define-inline car
  (syntax-rules ()
   ((car x0)
    (let ((x x0))
      (.check! (pair? x) ,$ex.car x)
      (car:pair x)))))
   
(define-inline cdr
  (syntax-rules ()
   ((car x0)
    (let ((x x0))
      (.check! (pair? x) ,$ex.cdr x)
      (cdr:pair x)))))

(define-inline vector-length
  (syntax-rules ()
   ((vector-length v0)
    (let ((v v0))
      (.check! (vector? v) ,$ex.vlen v)
      (vector-length:vec v)))))
   
(define-inline vector-ref
  (syntax-rules ()
   ((vector-ref v0 i0)
    (let ((v v0)
          (i i0))
      (.check! (fixnum? i) ,$ex.vref v i)
      (.check! (vector? v) ,$ex.vref v i)
      (.check! (<:fix:fix i (vector-length:vec v)) ,$ex.vref v i)
      (.check! (>=:fix:fix i 0) ,$ex.vref  v i)
      (vector-ref:trusted v i)))))
   
(define-inline vector-set!
  (syntax-rules ()
   ((vector-set! v0 i0 x0)
    (let ((v v0)
          (i i0)
          (x x0))
      (.check! (fixnum? i) ,$ex.vset v i x)
      (.check! (vector? v) ,$ex.vset v i x)
      (.check! (<:fix:fix i (vector-length:vec v)) ,$ex.vset v i x)
      (.check! (>=:fix:fix i 0) ,$ex.vset v i x)
      (vector-set!:trusted v i x)))))
   
; This transformation must make sure the entire list is freshly
; allocated when an argument to LIST returns more than once.

(define-inline list
  (syntax-rules ()
   ((list)
    '())
   ((list ?e)
    (cons ?e '()))
   ((list ?e1 ?e2 ...)
    (let* ((t1 ?e1)
           (t2 (list ?e2 ...)))
      (cons t1 t2)))))

; This transformation must make sure the entire list is freshly
; allocated when an argument to VECTOR returns more than once.

(define-inline vector
  (syntax-rules ()
   ((vector)
    '#())
   ((vector ?e)
    (make-vector 1 ?e))
   ((vector ?e1 ?e2 ...)
    (letrec-syntax
      ((vector-aux1
        (... (syntax-rules ()
              ((vector-aux1 () ?n ?exps ?indexes ?temps)
               (vector-aux2 ?n ?exps ?indexes ?temps))
              ((vector-aux1 (?exp1 ?exp2 ...) ?n ?exps ?indexes ?temps)
               (vector-aux1 (?exp2 ...)
                            (+ ?n 1)
                            (?exp1 . ?exps)
                            (?n . ?indexes)
                            (t . ?temps))))))
       (vector-aux2
        (... (syntax-rules ()
              ((vector-aux2 ?n (?exp1 ?exp2 ...) (?n1 ?n2 ...) (?t1 ?t2 ...))
               (let* ((?t1 ?exp1)
                      (?t2 ?exp2)
                      ...
                      (v (make-vector ?n ?t1)))
                 (vector-set! v ?n2 ?t2)
                 ...
                 v))))))
      (vector-aux1 (?e1 ?e2 ...) 0 () () ())))))

(define-inline cadddr
  (syntax-rules ()
   ((cadddr ?e)
    (car (cdr (cdr (cdr ?e)))))))

(define-inline cddddr
  (syntax-rules ()
   ((cddddr ?e)
    (cdr (cdr (cdr (cdr ?e)))))))

(define-inline cdddr
  (syntax-rules ()
   ((cdddr ?e)
    (cdr (cdr (cdr ?e))))))

(define-inline caddr
  (syntax-rules ()
   ((caddr ?e)
    (car (cdr (cdr ?e))))))

(define-inline cddr
  (syntax-rules ()
   ((cddr ?e)
    (cdr (cdr ?e)))))

(define-inline cdar
  (syntax-rules ()
   ((cdar ?e)
    (cdr (car ?e)))))

(define-inline cadr
  (syntax-rules ()
   ((cadr ?e)
    (car (cdr ?e)))))

(define-inline caar
  (syntax-rules ()
   ((caar ?e)
    (car (car ?e)))))

(define-inline make-vector
  (syntax-rules ()
   ((make-vector ?n)
    (make-vector ?n '()))))

(define-inline make-string
  (syntax-rules ()
   ((make-string ?n)
    (make-string ?n #\space))))

(define-inline =
  (syntax-rules ()
   ((= ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (= ?e1 t)
           (= t ?e3 ?e4 ...))))))

(define-inline <
  (syntax-rules ()
   ((< ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (< ?e1 t)
           (< t ?e3 ?e4 ...))))))

(define-inline >
  (syntax-rules ()
   ((> ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (> ?e1 t)
           (> t ?e3 ?e4 ...))))))

(define-inline <=
  (syntax-rules ()
   ((<= ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (<= ?e1 t)
           (<= t ?e3 ?e4 ...))))))

(define-inline >=
  (syntax-rules ()
   ((>= ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (>= ?e1 t)
           (>= t ?e3 ?e4 ...))))))

(define-inline +
  (syntax-rules ()
   ((+)
    0)
   ((+ ?e)
    ?e)
   ((+ ?e1 ?e2 ?e3 ?e4 ...)
    (+ (+ ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline *
  (syntax-rules ()
   ((*)
    1)
   ((* ?e)
    ?e)
   ((* ?e1 ?e2 ?e3 ?e4 ...)
    (* (* ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline -
  (syntax-rules ()
   ((- ?e)
    (- 0 ?e))
   ((- ?e1 ?e2 ?e3 ?e4 ...)
    (- (- ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline /
  (syntax-rules ()
   ((/ ?e)
    (/ 1 ?e))
   ((/ ?e1 ?e2 ?e3 ?e4 ...)
    (/ (/ ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline abs
  (syntax-rules ()
   ((abs ?z)
    (let ((temp ?z))
      (if (< temp 0)
          (-- temp)
          temp)))))

(define-inline negative?
  (syntax-rules ()
   ((negative? ?x)
    (< ?x 0))))

(define-inline positive?
  (syntax-rules ()
   ((positive? ?x)
    (> ?x 0))))

(define-inline eqv?
  (transformer
   (lambda (exp rename compare)
     (let ((arg1 (cadr exp))
           (arg2 (caddr exp)))
       (define (constant? exp)
         (or (boolean? exp)
             (char? exp)
             (and (pair? exp)
                  (= (length exp) 2)
                  (identifier? (car exp))
                  (compare (car exp) (rename 'quote))
                  (symbol? (cadr exp)))))
       (if (or (constant? arg1)
               (constant? arg2))
           (cons (rename 'eq?) (cdr exp))
           exp)))))

(define-inline memq
  (syntax-rules (quote)
   ((memq ?expr '(?datum ...))
    (letrec-syntax
      ((memq0
        (... (syntax-rules (quote)
              ((memq0 '?xx '(?d ...))
               (let ((t1 '(?d ...)))
                 (memq1 '?xx t1 (?d ...))))
              ((memq0 ?e '(?d ...))
               (let ((t0 ?e)
                     (t1 '(?d ...)))
                 (memq1 t0 t1 (?d ...)))))))
       (memq1
        (... (syntax-rules ()
              ((memq1 ?t0 ?t1 ())
               #f)
              ((memq1 ?t0 ?t1 (?d1 ?d2 ...))
               (if (eq? ?t0 '?d1)
                   ?t1
                   (let ((?t1 (cdr ?t1)))
                     (memq1 ?t0 ?t1 (?d2 ...)))))))))
      (memq0 ?expr '(?datum ...))))))

(define-inline memv
  (transformer
   (lambda (exp rename compare)
     (let ((arg1 (cadr exp))
           (arg2 (caddr exp)))
       (if (or (boolean? arg1)
               (fixnum? arg1)
               (char? arg1)
               (and (pair? arg1)
                    (= (length arg1) 2)
                    (identifier? (car arg1))
                    (compare (car arg1) (rename 'quote))
                    (symbol? (cadr arg1)))
               (and (pair? arg2)
                    (= (length arg2) 2)
                    (identifier? (car arg2))
                    (compare (car arg2) (rename 'quote))
                    (every1? (lambda (x)
                               (or (boolean? x)
                                   (fixnum? x)
                                   (char? x)
                                   (symbol? x)))
                             (cadr arg2))))
           (cons (rename 'memq) (cdr exp))
           exp)))))

(define-inline assv
  (transformer
   (lambda (exp rename compare)
     (let ((arg1 (cadr exp))
           (arg2 (caddr exp)))
       (if (or (boolean? arg1)
               (char? arg1)
               (and (pair? arg1)
                    (= (length arg1) 2)
                    (identifier? (car arg1))
                    (compare (car arg1) (rename 'quote))
                    (symbol? (cadr arg1)))
               (and (pair? arg2)
                    (= (length arg2) 2)
                    (identifier? (car arg2))
                    (compare (car arg2) (rename 'quote))
                    (every1? (lambda (y)
                               (and (pair? y)
                                    (let ((x (car y)))
                                      (or (boolean? x)
                                          (char? x)
                                          (symbol? x)))))
                             (cadr arg2))))
           (cons (rename 'assq) (cdr exp))
           exp)))))

(define-inline map
  (syntax-rules (lambda)
   ((map ?proc ?exp1 ?exp2 ...)
    (letrec-syntax
      ((loop
        (... (syntax-rules (lambda)
              ((loop 1 () (?y1 ?y2 ...) ?f ?exprs)
               (loop 2 (?y1 ?y2 ...) ?f ?exprs))
              ((loop 1 (?a1 ?a2 ...) (?y2 ...) ?f ?exprs)
               (loop 1 (?a2 ...) (y1 ?y2 ...) ?f ?exprs))
              
              ((loop 2 ?ys (lambda ?formals ?body) ?exprs)
               (loop 3 ?ys (lambda ?formals ?body) ?exprs))
              ((loop 2 ?ys (?f1 . ?f2) ?exprs)
               (let ((f (?f1 . ?f2)))
                 (loop 3 ?ys f ?exprs)))
              ; ?f must be a constant or variable.
              ((loop 2 ?ys ?f ?exprs)
               (loop 3 ?ys ?f ?exprs))
              
              ((loop 3 (?y1 ?y2 ...) ?f (?e1 ?e2 ...))
               (do ((?y1 ?e1 (cdr ?y1))
                    (?y2 ?e2 (cdr ?y2))
                    ...
                    (results '() (cons (?f (car ?y1) (car ?y2) ...)
                                       results)))
                   ((or (null? ?y1) (null? ?y2) ...)
                    (reverse results))))))))
      
      (loop 1 (?exp1 ?exp2 ...) () ?proc (?exp1 ?exp2 ...))))))

(define-inline for-each
  (syntax-rules (lambda)
   ((for-each ?proc ?exp1 ?exp2 ...)
    (letrec-syntax
      ((loop
        (... (syntax-rules (lambda)
              ((loop 1 () (?y1 ?y2 ...) ?f ?exprs)
               (loop 2 (?y1 ?y2 ...) ?f ?exprs))
              ((loop 1 (?a1 ?a2 ...) (?y2 ...) ?f ?exprs)
               (loop 1 (?a2 ...) (y1 ?y2 ...) ?f ?exprs))
              
              ((loop 2 ?ys (lambda ?formals ?body) ?exprs)
               (loop 3 ?ys (lambda ?formals ?body) ?exprs))
              ((loop 2 ?ys (?f1 . ?f2) ?exprs)
               (let ((f (?f1 . ?f2)))
                 (loop 3 ?ys f ?exprs)))
              ; ?f must be a constant or variable.
              ((loop 2 ?ys ?f ?exprs)
               (loop 3 ?ys ?f ?exprs))
              
              ((loop 3 (?y1 ?y2 ...) ?f (?e1 ?e2 ...))
               (do ((?y1 ?e1 (cdr ?y1))
                    (?y2 ?e2 (cdr ?y2))
                    ...)
                   ((or (null? ?y1) (null? ?y2) ...)
                    (if #f #f))
                   (?f (car ?y1) (car ?y2) ...)))))))
      
      (loop 1 (?exp1 ?exp2 ...) () ?proc (?exp1 ?exp2 ...))))))

))

(define extended-syntactic-environment
  (syntactic-copy global-syntactic-environment))

(define (make-extended-syntactic-environment)
  (syntactic-copy extended-syntactic-environment))

; MacScheme machine assembly instructions.

(define instruction.op car)
(define instruction.arg1 cadr)
(define instruction.arg2 caddr)
(define instruction.arg3 cadddr)

; Opcode table.

(define *mnemonic-names* '())           ; For readify-lap
(begin
 '
 (define *last-reserved-mnemonic* 32767)	; For consistency check
 '
 (define make-mnemonic
   (let ((count 0))
     (lambda (name)
       (set! count (+ count 1))
       (if (= count *last-reserved-mnemonic*)
           (error "Error in make-mnemonic: conflict: " name))
       (set! *mnemonic-names* (cons (cons count name) *mnemonic-names*))
       count)))
 '
 (define (reserved-mnemonic name value)
   (if (and (> value 0) (< value *last-reserved-mnemonic*))
       (set! *last-reserved-mnemonic* value))
   (set! *mnemonic-names* (cons (cons value name) *mnemonic-names*))
   value)
 #t)

(define make-mnemonic
   (let ((count 0))
     (lambda (name)
       (set! count (+ count 1))
       (set! *mnemonic-names* (cons (cons count name) *mnemonic-names*))
       count)))

(define (reserved-mnemonic name ignored)
  (make-mnemonic name))

(define $.linearize (reserved-mnemonic '.linearize -1))  ; unused?
(define $.label (reserved-mnemonic '.label 63))
(define $.proc (reserved-mnemonic '.proc 62))    ; proc entry point
(define $.cont (reserved-mnemonic '.cont 61))    ; return point
(define $.align (reserved-mnemonic '.align 60))  ; align code stream
(define $.asm (reserved-mnemonic '.asm 59))      ; in-line native code
(define $.proc-doc                               ; internal def proc info
  (reserved-mnemonic '.proc-doc 58))
(define $.end                                    ; end of code vector
  (reserved-mnemonic '.end 57))                  ; (asm internal)
(define $.singlestep                             ; insert singlestep point
  (reserved-mnemonic '.singlestep 56))           ; (asm internal)
(define $.entry (reserved-mnemonic '.entry 55))  ; procedure entry point 
                                                 ; (asm internal)

(define $op1 (make-mnemonic 'op1))               ; op      prim
(define $op2 (make-mnemonic 'op2))               ; op2     prim,k
(define $op3 (make-mnemonic 'op3))               ; op3     prim,k1,k2
(define $op2imm (make-mnemonic 'op2imm))         ; op2imm  prim,x
(define $const (make-mnemonic 'const))           ; const   x
(define $global (make-mnemonic 'global))         ; global  x
(define $setglbl (make-mnemonic 'setglbl))       ; setglbl x
(define $lexical (make-mnemonic 'lexical))       ; lexical m,n
(define $setlex (make-mnemonic 'setlex))         ; setlex  m,n
(define $stack (make-mnemonic 'stack))           ; stack   n
(define $setstk (make-mnemonic 'setstk))         ; setstk  n
(define $load (make-mnemonic 'load))             ; load    k,n
(define $store (make-mnemonic 'store))           ; store   k,n
(define $reg (make-mnemonic 'reg))               ; reg     k
(define $setreg (make-mnemonic 'setreg))         ; setreg  k
(define $movereg (make-mnemonic 'movereg))       ; movereg k1,k2
(define $lambda (make-mnemonic 'lambda))         ; lambda  x,n,doc
(define $lexes (make-mnemonic 'lexes))           ; lexes   n,doc
(define $args= (make-mnemonic 'args=))           ; args=   k
(define $args>= (make-mnemonic 'args>=))         ; args>=  k
(define $invoke (make-mnemonic 'invoke))         ; invoke  k
(define $save (make-mnemonic 'save))             ; save    L,k
(define $setrtn (make-mnemonic 'setrtn))         ; setrtn  L
(define $restore (make-mnemonic 'restore))       ; restore n    ; deprecated
(define $pop (make-mnemonic 'pop))               ; pop     k
(define $popstk (make-mnemonic 'popstk))         ; popstk       ; for students
(define $return (make-mnemonic 'return))         ; return
(define $mvrtn (make-mnemonic 'mvrtn))           ; mvrtn        ; NYI
(define $apply (make-mnemonic 'apply))           ; apply
(define $nop (make-mnemonic 'nop))               ; nop
(define $jump (make-mnemonic 'jump))             ; jump    m,o
(define $skip (make-mnemonic 'skip))             ; skip    L    ; forward
(define $branch (make-mnemonic 'branch))         ; branch  L
(define $branchf (make-mnemonic 'branchf))       ; branchf L
(define $check (make-mnemonic 'check))           ; check   k1,k2,k3,L
(define $trap (make-mnemonic 'trap))             ; trap    k1,k2,k3,exn

; A peephole optimizer may define more instructions in some
; target-specific file.

; eof
; Copyright 1991 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Larceny -- target-specific information for Twobit's SPARC backend.
;
; 11 June 1999 / wdc

; The maximum number of fixed arguments that may be followed by a rest
; argument.  This limitation is removed by the macro expander.

(define @maxargs-with-rest-arg@ 30)

; The number of MacScheme machine registers.
; (They do not necessarily correspond to hardware registers.)

(define *nregs* 32)
(define *lastreg* (- *nregs* 1))
(define *fullregs* (quotient *nregs* 2))

; The number of argument registers that are represented by hardware
; registers.

(define *nhwregs* 8)

; Variable names that indicate register targets.

(define *regnames*
  (do ((alist '() (cons (cons (string->symbol
                               (string-append ".REG" (number->string r)))
                              r)
                        alist))
       (r (- *nhwregs* 1) (- r 1)))
      ((<= r 0)
       alist)))

; A non-inclusive upper bound for the instruction encodings.

(define *number-of-mnemonics* 72)

; Integrable procedures and procedure-specific source code transformations.
; Every integrable procedure that takes a varying number of arguments must
; supply a transformation procedure to map calls into the fixed arity
; required by the MacScheme machine instructions.

; The table of integrable procedures.
; Each entry is a list of the following items:
;
;    procedure name
;    arity (or -1 for special primops like .check!)
;    procedure name to be used by the disassembler
;    predicate for immediate operands (or #f)
;    primop code in the MacScheme machine (not used by Larceny)
;    the effects that kill this primop's result
;    the effects of this primop that kill available expressions

(define (prim-entry name)
  (assq name $usual-integrable-procedures$))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry)
  (car (cddddr entry)))

; This predicate returns #t iff its argument will be represented
; as a fixnum on the target machine.

(define smallint?
  (let* ((least (- (expt 2 29)))
         (greatest (- (- least) 1)))
    (lambda (x)
      (and (number? x)
           (exact? x)
           (integer? x)
           (<= least x greatest)))))

(define (sparc-imm? x)
  (and (fixnum? x)
       (<= -1024 x 1023)))

(define (sparc-eq-imm? x)
  (or (sparc-imm? x)
      (eq? x #t)
      (eq? x #f)
      (eq? x '())))

(define (valid-typetag? x)
  (and (fixnum? x)
       (<= 0 x 7)))

(define (fixnum-primitives) #t)
(define (flonum-primitives) #t)

; The table of primitives has been extended with
; kill information used for commoning.

(define (prim-lives-until entry)
  (list-ref entry 5))

(define (prim-kills entry)
  (list-ref entry 6))

(define $usual-integrable-procedures$
  (let ((:globals  available:killer:globals)
        (:car      available:killer:car)
        (:cdr      available:killer:cdr)
        (:string   available:killer:string)
        (:vector   available:killer:vector)
        (:cell     available:killer:cell)
        (:io       available:killer:io)
        (:none     available:killer:none)     ; none of the above
        (:all      available:killer:all)      ; all of the above
        (:immortal available:killer:immortal) ; never killed
        (:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate    ignored  killed     kills
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect

  `((break            0 break            #f             3 ,:dead     ,:all)
    (creg             0 creg             #f             7 ,:dead     ,:all)
    (unspecified      0 unspecified      #f            -1 ,:dead     ,:none)
    (undefined        0 undefined        #f             8 ,:dead     ,:none)
    (eof-object       0 eof-object       #f            -1 ,:dead     ,:none)
    (enable-interrupts 1 enable-interrupts #f          -1 ,:dead     ,:all)
    (disable-interrupts 0 disable-interrupts #f        -1 ,:dead     ,:all)

    (typetag          1 typetag          #f          #x11 ,:dead     ,:none)
    (not              1 not              #f          #x18 ,:immortal ,:none)
    (null?            1 null?            #f          #x19 ,:immortal ,:none)
    (pair?            1 pair?            #f          #x1a ,:immortal ,:none)
    (eof-object?      1 eof-object?      #f            -1 ,:immortal ,:none)
    (port?            1 port?            #f            -1 ,:dead     ,:none)
    (structure?       1 structure?       #f            -1 ,:dead     ,:none)
    (car              1 car              #f          #x1b ,:car      ,:none)
    (,name:CAR        1 car              #f          #x1b ,:car      ,:none)
    (cdr              1 cdr              #f          #x1c ,:cdr      ,:none)
    (,name:CDR        1 cdr              #f          #x1c ,:cdr      ,:none)
    (symbol?          1 symbol?          #f          #x1f ,:immortal ,:none)
    (number?          1 complex?         #f          #x20 ,:immortal ,:none)
    (complex?         1 complex?         #f          #x20 ,:immortal ,:none)
    (real?            1 rational?        #f          #x21 ,:immortal ,:none)
    (rational?        1 rational?        #f          #x21 ,:immortal ,:none)
    (integer?         1 integer?         #f          #x22 ,:immortal ,:none)
    (fixnum?          1 fixnum?          #f          #x23 ,:immortal ,:none)
    (flonum?          1 flonum?          #f            -1 ,:immortal ,:none)
    (compnum?         1 compnum?         #f            -1 ,:immortal ,:none)
    (exact?           1 exact?           #f          #x24 ,:immortal ,:none)
    (inexact?         1 inexact?         #f          #x25 ,:immortal ,:none)
    (exact->inexact   1 exact->inexact   #f          #x26 ,:immortal ,:none)
    (inexact->exact   1 inexact->exact   #f          #x27 ,:immortal ,:none)
    (round            1 round            #f          #x28 ,:immortal ,:none)
    (truncate         1 truncate         #f          #x29 ,:immortal ,:none)
    (zero?            1 zero?            #f          #x2c ,:immortal ,:none)
    (--               1 --               #f          #x2d ,:immortal ,:none)
    (lognot           1 lognot           #f          #x2f ,:immortal ,:none)
    (real-part        1 real-part        #f          #x3e ,:immortal ,:none)
    (imag-part        1 imag-part        #f          #x3f ,:immortal ,:none)
    (char?            1 char?            #f          #x40 ,:immortal ,:none)
    (char->integer    1 char->integer    #f          #x41 ,:immortal ,:none)
    (integer->char    1 integer->char    #f          #x42 ,:immortal ,:none)
    (string?          1 string?          #f          #x50 ,:immortal ,:none)
    (string-length    1 string-length    #f          #x51 ,:immortal ,:none)
    (vector?          1 vector?          #f          #x52 ,:immortal ,:none)
    (vector-length    1 vector-length    #f          #x53 ,:immortal ,:none)
    (bytevector?      1 bytevector?      #f          #x54 ,:immortal ,:none)
    (bytevector-length 1 bytevector-length #f        #x55 ,:immortal ,:none)
    (bytevector-fill! 2 bytevector-fill! #f            -1 ,:dead     ,:string)
    (make-bytevector  1 make-bytevector  #f          #x56 ,:dead     ,:none)
    (procedure?       1 procedure?       #f          #x58 ,:immortal ,:none)
    (procedure-length 1 procedure-length #f          #x59 ,:dead     ,:none)
    (make-procedure   1 make-procedure   #f          #x5a ,:dead     ,:none)
    (creg-set!        1 creg-set!        #f          #x71 ,:dead     ,:none)
    (,name:MAKE-CELL  1 make-cell        #f          #x7e ,:dead     ,:none)
    (,name:CELL-REF   1 cell-ref         #f          #x7f ,:cell     ,:none)
    (,name:CELL-SET!  2 cell-set!        #f          #xdf ,:dead     ,:cell)
    (typetag-set!     2 typetag-set! ,valid-typetag? #xa0 ,:dead     ,:all)
    (eq?              2 eq?           ,sparc-eq-imm? #xa1 ,:immortal ,:none)
    (eqv?             2 eqv?             #f          #xa2 ,:immortal ,:none)
    (cons             2 cons             #f          #xa8 ,:dead     ,:none)
    (,name:CONS       2 cons             #f          #xa8 ,:dead     ,:none)
    (set-car!         2 set-car!         #f          #xa9 ,:dead     ,:car)
    (set-cdr!         2 set-cdr!         #f          #xaa ,:dead     ,:cdr)
    (+                2 +                ,sparc-imm? #xb0 ,:immortal ,:none)
    (-                2 -                ,sparc-imm? #xb1 ,:immortal ,:none)
    (*                2 *                ,sparc-imm? #xb2 ,:immortal ,:none)
    (/                2 /                #f          #xb3 ,:immortal ,:none)
    (quotient         2 quotient         #f          #xb4 ,:immortal ,:none)
    (<                2 <                ,sparc-imm? #xb5 ,:immortal ,:none)
    (<=               2 <=               ,sparc-imm? #xb6 ,:immortal ,:none)
    (=                2 =                ,sparc-imm? #xb7 ,:immortal ,:none)
    (>                2 >                ,sparc-imm? #xb8 ,:immortal ,:none)
    (>=               2 >=               ,sparc-imm? #xb9 ,:immortal ,:none)
    (logand           2 logand           #f          #xc0 ,:immortal ,:none)
    (logior           2 logior           #f          #xc1 ,:immortal ,:none)
    (logxor           2 logxor           #f          #xc2 ,:immortal ,:none)
    (lsh              2 lsh              #f          #xc3 ,:immortal ,:none)
    (rsha             2 rsha             #f            -1 ,:immortal ,:none)
    (rshl             2 rshl             #f            -1 ,:immortal ,:none)
    (rot              2 rot              #f          #xc4 ,:immortal ,:none)
    (make-string      2 make-string      #f            -1 ,:dead     ,:none)
    (string-ref       2 string-ref       ,sparc-imm? #xd1 ,:string   ,:none)
    (string-set!      3 string-set!      ,sparc-imm?   -1 ,:dead     ,:string)
    (make-vector      2 make-vector      #f          #xd2 ,:dead     ,:none)
    (vector-ref       2 vector-ref       ,sparc-imm? #xd3 ,:vector   ,:none)
    (bytevector-ref   2 bytevector-ref   ,sparc-imm? #xd5 ,:string   ,:none)
    (procedure-ref    2 procedure-ref    #f          #xd7 ,:dead     ,:none)
    (char<?           2 char<?           ,char?      #xe0 ,:immortal ,:none)
    (char<=?          2 char<=?          ,char?      #xe1 ,:immortal ,:none)
    (char=?           2 char=?           ,char?      #xe2 ,:immortal ,:none)
    (char>?           2 char>?           ,char?      #xe3 ,:immortal ,:none)
    (char>=?          2 char>=?          ,char?      #xe4 ,:immortal ,:none)
    
    (sys$partial-list->vector 2 sys$partial-list->vector #f -1 ,:dead ,:all)
    (vector-set!      3 vector-set!      #f          #xf1 ,:dead     ,:vector)
    (bytevector-set!  3 bytevector-set!  #f          #xf2 ,:dead     ,:string)
    (procedure-set!   3 procedure-set!   #f          #xf3 ,:dead     ,:all)
    (bytevector-like? 1 bytevector-like? #f            -1 ,:immortal ,:none)
    (vector-like?     1 vector-like?     #f            -1 ,:immortal ,:none)
    (bytevector-like-ref 2 bytevector-like-ref #f      -1 ,:string   ,:none)
    (bytevector-like-set! 3 bytevector-like-set! #f    -1 ,:dead     ,:string)
    (sys$bvlcmp       2 sys$bvlcmp       #f            -1 ,:dead     ,:all)
    (vector-like-ref  2 vector-like-ref  #f            -1 ,:vector   ,:none)
    (vector-like-set! 3 vector-like-set! #f            -1 ,:dead     ,:vector)
    (vector-like-length 1 vector-like-length #f        -1 ,:immortal ,:none)
    (bytevector-like-length 1 bytevector-like-length #f -1 ,:immortal ,:none)
    (remainder        2 remainder        #f            -1 ,:immortal ,:none)
    (sys$read-char    1 sys$read-char    #f            -1 ,:dead     ,:io)
    (gc-counter       0 gc-counter       #f            -1 ,:dead     ,:none)
    ,@(if (fixnum-primitives)
	  `((most-positive-fixnum
                          0 most-positive-fixnum
                                         #f            -1 ,:immortal ,:none)
	    (most-negative-fixnum
                          0 most-negative-fixnum
                                         #f            -1 ,:immortal ,:none)
	    (fx+          2 fx+          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx-          2 fx-          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx--         1 fx--         #f            -1 ,:immortal ,:none)
	    (fx*          2 fx*          #f            -1 ,:immortal ,:none)
	    (fx=          2 fx=          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx<          2 fx<          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx<=         2 fx<=         ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx>          2 fx>          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx>=         2 fx>=         ,sparc-imm?   -1 ,:immortal ,:none)
	    (fxzero?      1 fxzero?      #f            -1 ,:immortal ,:none)
	    (fxpositive?  1 fxpositive?  #f            -1 ,:immortal ,:none)
	    (fxnegative?  1 fxnegative?  #f            -1 ,:immortal ,:none))
	  '())
    ,@(if (flonum-primitives)
          `((fl+          2 +            #f            -1 ,:immortal ,:none)
	    (fl-          2 -            #f            -1 ,:immortal ,:none)
	    (fl--         1 --           #f            -1 ,:immortal ,:none)
	    (fl*          2 *            #f            -1 ,:immortal ,:none)
	    (fl=          2 =            #f            -1 ,:immortal ,:none)
	    (fl<          2 <            #f            -1 ,:immortal ,:none)
	    (fl<=         2 <=           #f            -1 ,:immortal ,:none)
	    (fl>          2 >            #f            -1 ,:immortal ,:none)
	    (fl>=         2 >=           #f            -1 ,:immortal ,:none))
          '())

    ; Added for CSE, representation analysis.

    (,name:CHECK!    -1 check!           #f            -1 ,:dead     ,:none)
    (vector-length:vec 1 vector-length:vec #f          -1 ,:immortal ,:none)
    (vector-ref:trusted 2 vector-ref:trusted ,sparc-imm? -1 ,:vector   ,:none)
    (vector-set!:trusted 3 vector-set!:trusted #f      -1 ,:dead     ,:vector)
    (car:pair         1 car:pair         #f            -1 ,:car      ,:none)
    (cdr:pair         1 cdr:pair         #f            -1 ,:cdr      ,:none)
    (=:fix:fix        2 =:fix:fix        ,sparc-imm?   -1 ,:immortal ,:none)
    (<:fix:fix        2 <:fix:fix        ,sparc-imm?   -1 ,:immortal ,:none)
    (<=:fix:fix       2 <=:fix:fix       ,sparc-imm?   -1 ,:immortal ,:none)
    (>=:fix:fix       2 >=:fix:fix       ,sparc-imm?   -1 ,:immortal ,:none)
    (>:fix:fix        2 >:fix:fix        ,sparc-imm?   -1 ,:immortal ,:none)
    
    ; Not yet implemented.

    (+:idx:idx        2 +:idx:idx        #f            -1 ,:immortal ,:none)
    (+:fix:fix        2 +:idx:idx        #f            -1 ,:immortal ,:none)
    (+:exi:exi        2 +:idx:idx        #f            -1 ,:immortal ,:none)
    (+:flo:flo        2 +:idx:idx        #f            -1 ,:immortal ,:none)
    (=:flo:flo        2 =:flo:flo        #f            -1 ,:immortal ,:none)
    (=:obj:flo        2 =:obj:flo        #f            -1 ,:immortal ,:none)
    (=:flo:obj        2 =:flo:obj        #f            -1 ,:immortal ,:none)
    )))

; Not used by the Sparc assembler; for information only.

(define $immediate-primops$
  '((typetag-set! #x80)
    (eq? #x81)
    (+ #x82)
    (- #x83)
    (< #x84)
    (<= #x85)
    (= #x86)
    (> #x87)
    (>= #x88)
    (char<? #x89)
    (char<=? #x8a)
    (char=? #x8b)
    (char>? #x8c)
    (char>=? #x8d)
    (string-ref #x90)
    (vector-ref #x91)
    (bytevector-ref #x92)
    (bytevector-like-ref -1)
    (vector-like-ref -1)
    (fx+ -1)
    (fx- -1)
    (fx-- -1)
    (fx= -1)
    (fx< -1)
    (fx<= -1)
    (fx> -1)
    (fx>= -1)))

; Operations introduced by peephole optimizer.

(define $reg/op1/branchf                  ; reg/op1/branchf    prim,k1,L
  (make-mnemonic 'reg/op1/branchf))
(define $reg/op2/branchf                  ; reg/op2/branchf    prim,k1,k2,L
  (make-mnemonic 'reg/op2/branchf))
(define $reg/op2imm/branchf               ; reg/op2imm/branchf prim,k1,x,L
  (make-mnemonic 'reg/op2imm/branchf))
(define $reg/op1/check             ; reg/op1/check      prim,k1,k2,k3,k4,exn
  (make-mnemonic 'reg/op1/check))
(define $reg/op2/check             ; reg/op2/check      prim,k1,k2,k3,k4,k5,exn
  (make-mnemonic 'reg/op2/check))
(define $reg/op2imm/check          ; reg/op2imm/check   prim,k1,x,k2,k3,k4,exn
  (make-mnemonic 'reg/op2imm/check))
(define $reg/op1/setreg                   ; reg/op1/setreg     prim,k1,kr
  (make-mnemonic 'reg/op1/setreg))
(define $reg/op2/setreg                   ; reg/op2/setreg     prim,k1,k2,kr
  (make-mnemonic 'reg/op2/setreg))
(define $reg/op2imm/setreg                ; reg/op2imm/setreg  prim,k1,x,kr
  (make-mnemonic 'reg/op2imm/setreg))
(define $reg/branchf                      ; reg/branchf        k, L
  (make-mnemonic 'reg/branchf))
(define $reg/return                       ; reg/return         k
  (make-mnemonic 'reg/return))
(define $reg/setglbl                      ; reg/setglbl        k,x
  (make-mnemonic 'reg/setglbl))
(define $reg/op3                          ; reg/op3            prim,k1,k2,k3
  (make-mnemonic 'reg/op3))
(define $const/setreg                     ; const/setreg       const,k
  (make-mnemonic 'const/setreg))
(define $const/return                     ; const/return       const
  (make-mnemonic 'const/return))
(define $global/setreg                    ; global/setreg      x,k
  (make-mnemonic 'global/setreg))
(define $setrtn/branch                    ; setrtn/branch      L,doc
  (make-mnemonic 'setrtn/branch))
(define $setrtn/invoke                    ; setrtn/invoke      L
  (make-mnemonic 'setrtn/invoke))
(define $global/invoke                    ; global/invoke      global,n
  (make-mnemonic 'global/invoke))

; misc

(define $cons     'cons)
(define $car:pair 'car)
(define $cdr:pair 'cdr)

; eof
; Target-specific representations.
;
; A few of these representation types must be specified for every target:
;     rep:object
;     rep:procedure
;     rep:true
;     rep:false
;     rep:bottom

(define-subtype 'true       'object)      ; values that count as true
(define-subtype 'eqtype     'object)      ; can use EQ? instead of EQV?
(define-subtype 'nonpointer 'eqtype)      ; can omit write barrier
(define-subtype 'eqtype1    'eqtype)      ; eqtypes excluding #f
(define-subtype 'boolean    'nonpointer)
(define-subtype 'truth      'eqtype1)     ; { #t }
(define-subtype 'truth      'boolean)
(define-subtype 'false      'boolean)     ; { #f }
(define-subtype 'eqtype1    'true)  
(define-subtype 'procedure  'true)
(define-subtype 'vector     'true)
(define-subtype 'bytevector 'true)
(define-subtype 'string     'true)
(define-subtype 'pair       'true)
(define-subtype 'emptylist  'eqtype1)
(define-subtype 'emptylist  'nonpointer)
(define-subtype 'symbol     'eqtype1)
(define-subtype 'char       'eqtype1)
(define-subtype 'char       'nonpointer)
(define-subtype 'number     'true)
(define-subtype 'inexact    'number)
(define-subtype 'flonum     'inexact)
(define-subtype 'integer    'number)
(define-subtype 'exact      'number)
(define-subtype 'exactint   'integer)
(define-subtype 'exactint   'exact)
(define-subtype 'fixnum     'exactint)
(define-subtype '!fixnum    'fixnum)      ; 0 <= n
(define-subtype 'fixnum!    'fixnum)      ; n <= largest index
(define-subtype 'index      '!fixnum)
(define-subtype 'index      'fixnum!)
(define-subtype 'zero       'index)
(define-subtype 'fixnum     'eqtype1)
(define-subtype 'fixnum     'nonpointer)

(compute-type-structure!)

; If the intersection of rep1 and rep2 is known precisely,
; but neither is a subtype of the other, then their intersection
; should be declared explicitly.
; Otherwise a conservative approximation will be used.

(define-intersection 'true 'eqtype 'eqtype1)
(define-intersection 'true 'boolean 'truth)
(define-intersection 'exact 'integer 'exactint)
(define-intersection '!fixnum 'fixnum! 'index)

;(display-unions-and-intersections)

; Parameters.

(define rep:min_fixnum (- (expt 2 29)))
(define rep:max_fixnum (- (expt 2 29) 1))
(define rep:max_index  (- (expt 2 24) 1))

; The representations we'll recognize for now.

(define rep:object       (symbol->rep 'object))
(define rep:true         (symbol->rep 'true))
(define rep:truth        (symbol->rep 'truth))
(define rep:false        (symbol->rep 'false))
(define rep:boolean      (symbol->rep 'boolean))
(define rep:pair         (symbol->rep 'pair))
(define rep:symbol       (symbol->rep 'symbol))
(define rep:number       (symbol->rep 'number))
(define rep:zero         (symbol->rep 'zero))
(define rep:index        (symbol->rep 'index))
(define rep:fixnum       (symbol->rep 'fixnum))
(define rep:exactint     (symbol->rep 'exactint))
(define rep:flonum       (symbol->rep 'flonum))
(define rep:exact        (symbol->rep 'exact))
(define rep:inexact      (symbol->rep 'inexact))
(define rep:integer      (symbol->rep 'integer))
;(define rep:real         (symbol->rep 'real))
(define rep:char         (symbol->rep 'char))
(define rep:string       (symbol->rep 'string))
(define rep:vector       (symbol->rep 'vector))
(define rep:procedure    (symbol->rep 'procedure))
(define rep:bottom       (symbol->rep 'bottom))

; Given the value of a quoted constant, return its representation.

(define (representation-of-value x)
  (cond ((boolean? x)
         (if x
             rep:truth
             rep:false))
        ((pair? x)
         rep:pair)
        ((symbol? x)
         rep:symbol)
        ((number? x)
         (cond ((and (exact? x)
                     (integer? x))
                (cond ((zero? x)
                       rep:zero)
                      ((<= 0 x rep:max_index)
                       rep:index)
                      ((<= rep:min_fixnum
                           x
                           rep:max_fixnum)
                       rep:fixnum)
                      (else
                       rep:exactint)))
               ((and (inexact? x)
                     (real? x))
                rep:flonum)
               (else
                ; We're not tracking other numbers yet.
                rep:number)))
        ((char? x)
         rep:char)
        ((string? x)
         rep:string)
        ((vector? x)
         rep:vector)
        ; Everything counts as true except for #f.
        (else
         rep:true)))

; Tables that express the representation-specific operations,
; and the information about representations that are implied
; by certain operations.
; FIXME:  Currently way incomplete, but good enough for testing.

(define rep-specific
  
  (representation-table
   
   ; When the procedure in the first column is called with
   ; arguments described in the middle column, then the procedure
   ; in the last column can be called instead.
   
   '(
    ;(+                  (index index)               +:idx:idx)
    ;(+                  (fixnum fixnum)             +:fix:fix)
    ;(-                  (index index)               -:idx:idx)
    ;(-                  (fixnum fixnum)             -:fix:fix)
     
     (=                  (fixnum fixnum)             =:fix:fix)
     (<                  (fixnum fixnum)             <:fix:fix)
     (<=                 (fixnum fixnum)             <=:fix:fix)
     (>                  (fixnum fixnum)             >:fix:fix)
     (>=                 (fixnum fixnum)             >=:fix:fix)
     
    ;(+                  (flonum flonum)             +:flo:flo)
    ;(-                  (flonum flonum)             -:flo:flo)
    ;(=                  (flonum flonum)             =:flo:flo)
    ;(<                  (flonum flonum)             <:flo:flo)
    ;(<=                 (flonum flonum)             <=:flo:flo)
    ;(>                  (flonum flonum)             >:flo:flo)
    ;(>=                 (flonum flonum)             >=:flo:flo)
     
    ;(vector-set!:trusted (vector fixnum nonpointer) vector-set!:trusted:imm)
     )))

(define rep-result
  
  (representation-table
   
   ; When the procedure in the first column is called with
   ; arguments described in the middle column, then the result
   ; is described by the last column.
   
   '((fixnum?           (fixnum)                    (truth))
     (vector?           (vector)                    (truth))
     (<=                (zero !fixnum)              (truth))
     (>=                (!fixnum zero)              (truth))
     (<=:fix:fix        (zero !fixnum)              (truth))
     (>=:fix:fix        (!fixnum zero)              (truth))
     
     (+                 (index index)               (!fixnum))
     (+                 (fixnum fixnum)             (exactint))
     (-                 (index index)               (fixnum!))
     (-                 (fixnum fixnum)             (exactint))
     
     (+                 (flonum flonum)             (flonum))
     (-                 (flonum flonum)             (flonum))
     
    ;(+:idx:idx         (index index)               (!fixnum))
    ;(-:idx:idx         (index index)               (fixnum!))
    ;(+:fix:fix         (index index)               (exactint))
    ;(+:fix:fix         (fixnum fixnum)             (exactint))
    ;(-:idx:idx         (index index)               (fixnum))
    ;(-:fix:fix         (fixnum fixnum)             (exactint))
     
     (make-vector       (object object)             (vector))
     (vector-length:vec (vector)                    (index))
     (cons              (object object)             (pair))
     
     ; Is it really all that useful to know that the result
     ; of these comparisons is a boolean?
     
     (=                 (number number)             (boolean))
     (<                 (number number)             (boolean))
     (<=                (number number)             (boolean))
     (>                 (number number)             (boolean))
     (>=                (number number)             (boolean))
     
     (=:fix:fix         (fixnum fixnum)             (boolean))
     (<:fix:fix         (fixnum fixnum)             (boolean))
     (<=:fix:fix        (fixnum fixnum)             (boolean))
     (>:fix:fix         (fixnum fixnum)             (boolean))
     (>=:fix:fix        (fixnum fixnum)             (boolean))
     )))

(define rep-informing
  
  (representation-table
   
   ; When the predicate in the first column is called in the test position
   ; of a conditional expression, on arguments described by the second
   ; column, then the arguments are described by the third column if the
   ; predicate returns true, and by the fourth column if the predicate
   ; returns false.
   
   '(
     (fixnum?     (object)           (fixnum)          (object))
     (flonum?     (object)           (flonum)          (object))
     (vector?     (object)           (vector)          (object))
     (pair?       (object)           (pair)            (object))
     
     (=           (exactint index)   (index index)     (exactint index))
     (=           (index exactint)   (index index)     (index exactint))
     (=           (exactint !fixnum) (!fixnum !fixnum) (exactint !fixnum))
     (=           (!fixnum exactint) (!fixnum !fixnum) (!fixnum exactint))
     (=           (exactint fixnum!) (fixnum! fixnum!) (exactint fixnum!))
     (=           (fixnum! exactint) (fixnum! fixnum!) (fixnum! exactint))
     
     (<           (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<           (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<           (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     (<           (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     
     (<=          (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<=          (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     (<=          (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<=          (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     
     (>           (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>           (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>           (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>           (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     
     (>=          (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>=          (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>=          (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>=          (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     
     (=:fix:fix   (exactint index)   (index index)     (exactint index))
     (=:fix:fix   (index exactint)   (index index)     (index exactint))
     (=:fix:fix   (exactint !fixnum) (!fixnum !fixnum) (exactint !fixnum))
     (=:fix:fix   (!fixnum exactint) (!fixnum !fixnum) (!fixnum exactint))
     (=:fix:fix   (exactint fixnum!) (fixnum! fixnum!) (exactint fixnum!))
     (=:fix:fix   (fixnum! exactint) (fixnum! fixnum!) (fixnum! exactint))
     
     (<:fix:fix   (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<:fix:fix   (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     (<:fix:fix   (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<:fix:fix   (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     
     (<=:fix:fix  (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<=:fix:fix  (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     (<=:fix:fix  (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<=:fix:fix  (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     
     (>:fix:fix   (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>:fix:fix   (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>:fix:fix   (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>:fix:fix   (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     
     (>=:fix:fix  (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>=:fix:fix  (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>=:fix:fix  (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>=:fix:fix  (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     )))
; Copyright 1991 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 25 April 1999.
;
; Second pass of the Twobit compiler:
;   single assignment analysis, local source transformations,
;   assignment elimination, and lambda lifting.
; The code for assignment elimination and lambda lifting
; are in a separate file.
;
; This pass operates as a source-to-source transformation on
; expressions written in the subset of Scheme described by the
; following grammar, where the input and output expressions
; satisfy certain additional invariants described below.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F G <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the input only:
;   *  There are no internal definitions.
;   *  No identifier containing an upper case letter is bound anywhere.
;      (Change the "name:..." variables if upper case is preferred.)
;   *  No identifier is bound in more than one place.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.
;
; Invariants that hold for the output only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  Each R contains one entry for every identifier bound in the
;      formal argument list and the internal definition list that
;      precede it.  Each entry contains a list of pointers to all
;      references to the identifier, a list of pointers to all
;      assignments to the identifier, and a list of pointers to all
;      calls to the identifier.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  For each lambda expression, the associated G is a subset of F
;      that contains every identifier that occurs free within some
;      inner lambda expression that escapes, and possibly a few that
;      don't.  (Assignment-elimination does not calculate G exactly.)
;   *  Variables named IGNORED are neither referenced nor assigned.
;   *  Except for constants, the expression does not share structure
;      with the original input or itself, except that the references
;      and assignments in R are guaranteed to share structure with
;      the expression.  Thus the expression may be side effected, and
;      side effects to references or assignments obtained through R
;      are guaranteed to change the references or assignments pointed
;      to by R.

(define (pass2 exp)
  (simplify exp (make-notepad #f)))

; Given an expression and a "notepad" data structure that conveys
; inherited attributes, performs the appropriate optimizations and
; destructively modifies the notepad to record various attributes
; that it synthesizes while traversing the expression.  In particular,
; any nested lambda expressions and any variable references will be
; noted in the notepad.

(define (simplify exp notepad)
  (case (car exp)
    ((quote)    exp)
    ((lambda)   (simplify-lambda exp notepad))
    ((set!)     (simplify-assignment exp notepad))
    ((if)       (simplify-conditional exp notepad))
    ((begin)    (if (variable? exp)
                    (begin (notepad-var-add! notepad (variable.name exp))
                           exp)
                    (simplify-sequential exp notepad)))
    (else       (simplify-call exp notepad))))

; Most optimization occurs here.
; The  right hand sides of internal definitions are simplified,
; as is the body.
; Internal definitions of enclosed lambda expressions may
; then be lifted to this one.
; Single assignment analysis creates internal definitions.
; Single assignment elimination converts single assignments
; to bindings where possible, and renames arguments whose value
; is ignored.
; Assignment elimination then replaces all remaining assigned
; variables by heap-allocated cells.

(define (simplify-lambda exp notepad)
  (notepad-lambda-add! notepad exp)
  (let ((defs (lambda.defs exp))
        (body (lambda.body exp))
        (newnotepad (make-notepad exp)))
    (for-each (lambda (def)
                (simplify-lambda (def.rhs def) newnotepad))
              defs)
    (lambda.body-set! exp (simplify body newnotepad))
    (lambda.F-set! exp (notepad-free-variables newnotepad))
    (lambda.G-set! exp (notepad-captured-variables newnotepad))
    (single-assignment-analysis exp newnotepad)
    (let ((known-lambdas (notepad.nonescaping newnotepad)))
      (for-each (lambda (L)
                  (if (memq L known-lambdas)
                      (lambda-lifting L exp)
                      (lambda-lifting L L)))
                (notepad.lambdas newnotepad))))
  (single-assignment-elimination exp notepad)
  (assignment-elimination exp)
  (if (not (notepad.parent notepad))
      ; This is an outermost lambda expression.
      (lambda-lifting exp exp))
  exp)

; SIMPLIFY-ASSIGNMENT performs this transformation:
;
;    (set! I (begin ... E))
; -> (begin ... (set! I E))

(define (simplify-assignment exp notepad)
  (notepad-var-add! notepad (assignment.lhs exp))
  (let ((rhs (simplify (assignment.rhs exp) notepad)))
    (cond ((begin? rhs)
           (let ((exprs (reverse (begin.exprs rhs))))
             (assignment.rhs-set! exp (car exprs))
             (post-simplify-begin
              (make-begin (reverse (cons exp (cdr exprs))))
              notepad)))
          (else (assignment.rhs-set! exp rhs) exp))))

(define (simplify-sequential exp notepad)
  (let ((exprs (map (lambda (exp) (simplify exp notepad))
                    (begin.exprs exp))))
    (begin.exprs-set! exp exprs)
    (post-simplify-begin exp notepad)))

; Given (BEGIN E0 E1 E2 ...) where the E_i are simplified expressions,
; flattens any nested BEGINs and removes trivial expressions that
; don't appear in the last position.  The second argument is used only
; if a lambda expression is removed.
; This procedure is careful to return E instead of (BEGIN E).
; Fairly harmless bug: a variable reference removed by this procedure
; may remain on the notepad when it shouldn't.

(define (post-simplify-begin exp notepad)
  (let ((unspecified-expression (make-unspecified)))
    ; (flatten exprs '()) returns the flattened exprs in reverse order.
    (define (flatten exprs flattened)
      (cond ((null? exprs) flattened)
            ((begin? (car exprs))
             (flatten (cdr exprs)
                      (flatten (begin.exprs (car exprs)) flattened)))
            (else (flatten (cdr exprs) (cons (car exprs) flattened)))))
    (define (filter exprs filtered)
      (if (null? exprs)
          filtered
          (let ((exp (car exprs)))
            (cond ((constant? exp) (filter (cdr exprs) filtered))
                  ((variable? exp) (filter (cdr exprs) filtered))
                  ((lambda? exp)
                   (notepad.lambdas-set!
                    notepad
                    (remq exp (notepad.lambdas notepad)))
                   (filter (cdr exprs) filtered))
                  ((equal? exp unspecified-expression)
                   (filter (cdr exprs) filtered))
                  (else (filter (cdr exprs) (cons exp filtered)))))))
    (let ((exprs (flatten (begin.exprs exp) '())))
      (begin.exprs-set! exp (filter (cdr exprs) (list (car exprs))))
      (if (null? (cdr (begin.exprs exp)))
          (car (begin.exprs exp))
          exp))))

; SIMPLIFY-CALL performs this transformation:
;
;    (... (begin ... E) ...)
; -> (begin ... (... E ...))
;
; It also takes care of LET transformations.

(define (simplify-call exp notepad)
  (define (loop args newargs exprs)
    (cond ((null? args)
           (finish newargs exprs))
          ((begin? (car args))
           (let ((newexprs (reverse (begin.exprs (car args)))))
             (loop (cdr args)
                   (cons (car newexprs) newargs)
                   (append (cdr newexprs) exprs))))
          (else (loop (cdr args) (cons (car args) newargs) exprs))))
  (define (finish newargs exprs)
    (call.args-set! exp (reverse newargs))
    (let* ((newexp
            (if (lambda? (call.proc exp))
                (simplify-let exp notepad)
                (begin
                 (call.proc-set! exp
                                 (simplify (call.proc exp) notepad))
                 exp)))
           (newexp
            (if (and (call? newexp)
                     (variable? (call.proc newexp)))
                (let* ((procname (variable.name (call.proc newexp)))
                       (args (call.args newexp))
                       (entry
                        (and (not (null? args))
                             (constant? (car args))
                             (integrate-usual-procedures)
                             (every? constant? args)
                             (let ((entry (constant-folding-entry procname)))
                               (and entry
                                    (let ((predicates
                                           (constant-folding-predicates entry)))
                                      (and (= (length args)
                                              (length predicates))
                                           (let loop ((args args)
                                                      (predicates predicates))
                                             (cond ((null? args) entry)
                                                   (((car predicates)
                                                     (constant.value
                                                      (car args)))
                                                    (loop (cdr args)
                                                          (cdr predicates)))
                                                   (else #f))))))))))
                  (if entry
                      (make-constant (apply (constant-folding-folder entry)
                                            (map constant.value args)))
                      newexp))
                newexp)))
      (cond ((and (call? newexp)
                  (begin? (call.proc newexp)))
             (let ((exprs0 (reverse (begin.exprs (call.proc newexp)))))
               (call.proc-set! newexp (car exprs0))
               (post-simplify-begin
                (make-begin (reverse
                             (cons newexp
                                   (append (cdr exprs0) exprs))))
                notepad)))
            ((null? exprs)
             newexp)
            (else
             (post-simplify-begin
              (make-begin (reverse (cons newexp exprs)))
              notepad)))))
  (call.args-set! exp (map (lambda (arg) (simplify arg notepad))
                           (call.args exp)))
  (loop (call.args exp) '() '()))

; SIMPLIFY-LET performs these transformations:
;
;    ((lambda (I_1 ... I_k . I_rest) ---) E1 ... Ek Ek+1 ...)
; -> ((lambda (I_1 ... I_k I_rest) ---) E1 ... Ek (LIST Ek+1 ...))
;
;    ((lambda (I1 I2 ...) (begin D ...) (quote ...) E) L ...)
; -> ((lambda (I2 ...) (begin (define I1 L) D ...) (quote ...) E) ...)
;
; provided I1 is not assigned and each reference to I1 is in call position.
;
;    ((lambda (I1)
;       (begin)
;       (quote ((I1 ((begin I1)) () ())))
;       (begin I1))
;     E1)
;
; -> E1
;
;    ((lambda (I1)
;       (begin)
;       (quote ((I1 ((begin I1)) () ())))
;       (if (begin I1) E2 E3))
;     E1)
;
; -> (if E1 E2 E3)
;
; (Together with SIMPLIFY-CONDITIONAL, this cleans up the output of the OR
; macro and enables certain control optimizations.)
;
;    ((lambda (I1 I2 ...)
;       (begin D ...)
;       (quote (... (I <references> () <calls>) ...) ...)
;       E)
;     K ...)
; -> ((lambda (I2 ...)
;       (begin D' ...)
;       (quote (... ...) ...)
;       E')
;     ...)
;
; where D' ... and E' ... are obtained from D ... and E ...
; by replacing all references to I1 by K.  This transformation
; applies if K is a constant that can be duplicated without changing
; its EQV? behavior.
;
;    ((lambda () (begin) (quote ...) E)) -> E
;
;    ((lambda (IGNORED I2 ...) ---) E1 E2 ...)
; -> (begin E1 ((lambda (I2 ...) ---) E2 ...))
;
; (Single assignment analysis, performed by the simplifier for lambda
; expressions, detects unused arguments and replaces them in the argument
; list by the special identifier IGNORED.)

(define (simplify-let exp notepad)
  (define proc (call.proc exp))
  
  ; Loop1 operates before simplification of the lambda body.
  
  (define (loop1 formals actuals processed-formals processed-actuals)
    (cond ((null? formals)
           (if (not (null? actuals))
               (pass2-error p2error:wna exp))
           (return1 processed-formals processed-actuals))
          ((symbol? formals)
           (return1 (cons formals processed-formals)
                    (cons (make-call-to-LIST actuals) processed-actuals)))
          ((null? actuals)
           (pass2-error p2error:wna exp)
           (return1 processed-formals
                    processed-actuals))
          ((and (lambda? (car actuals))
                (let ((Rinfo (R-lookup (lambda.R proc) (car formals))))
                  (and (null? (R-entry.assignments Rinfo))
                       (= (length (R-entry.references Rinfo))
                          (length (R-entry.calls Rinfo))))))
           (let ((I (car formals))
                 (L (car actuals)))
             (notepad-nonescaping-add! notepad L)
             (lambda.defs-set! proc
               (cons (make-definition I L)
                     (lambda.defs proc)))
             (standardize-known-calls L
                                      (R-entry.calls
                                       (R-lookup (lambda.R proc) I)))
             (lambda.F-set! proc (union (lambda.F proc)
                                        (free-variables L)))
             (lambda.G-set! proc (union (lambda.G proc) (lambda.G L))))
           (loop1 (cdr formals)
                  (cdr actuals)
                  processed-formals
                  processed-actuals))
          ((and (constant? (car actuals))
                (let ((x (constant.value (car actuals))))
                  (or (boolean? x)
                      (number? x)
                      (symbol? x)
                      (char? x))))
           (let* ((I (car formals))
                  (Rinfo (R-lookup (lambda.R proc) I)))
             (if (null? (R-entry.assignments Rinfo))
                 (begin
                  (for-each (lambda (ref)
                              (variable-set! ref (car actuals)))
                            (R-entry.references Rinfo))
                  (lambda.R-set! proc (remq Rinfo (lambda.R proc)))
                  (lambda.F-set! proc (remq I (lambda.F proc)))
                  (lambda.G-set! proc (remq I (lambda.G proc)))
                  (loop1 (cdr formals)
                         (cdr actuals)
                         processed-formals
                         processed-actuals))
                 (loop1 (cdr formals)
                        (cdr actuals)
                        (cons (car formals) processed-formals)
                        (cons (car actuals) processed-actuals)))))
          (else (if (null? actuals)
                    (pass2-error p2error:wna exp))
                (loop1 (cdr formals)
                       (cdr actuals)
                       (cons (car formals) processed-formals)
                       (cons (car actuals) processed-actuals)))))
  
  (define (return1 rev-formals rev-actuals)
    (let ((formals (reverse rev-formals))
          (actuals (reverse rev-actuals)))
      (lambda.args-set! proc formals)
      (if (and (not (null? formals))
               (null? (cdr formals))
               (let* ((x (car formals))
                      (R (lambda.R proc))
                      (refs (references R x)))
                 (and (= 1 (length refs))
                      (null? (assignments R x)))))
          (let ((x (car formals))
                (body (lambda.body proc)))
            (cond ((and (variable? body)
                        (eq? x (variable.name body)))
                   (simplify (car actuals) notepad))
                  ((and (conditional? body)
                        (let ((B0 (if.test body)))
                          (variable? B0)
                          (eq? x (variable.name B0))))
                   (if.test-set! body (car actuals))
                   (simplify body notepad))
                  (else
                   (return1-finish formals actuals))))
          (return1-finish formals actuals))))
  
  (define (return1-finish formals actuals)
    (simplify-lambda proc notepad)
    (loop2 formals actuals '() '() '()))
  
  ; Loop2 operates after simplification of the lambda body.
  
  (define (loop2 formals actuals processed-formals processed-actuals for-effect)
    (cond ((null? formals)
           (return2 processed-formals processed-actuals for-effect))
          ((ignored? (car formals))
           (loop2 (cdr formals)
                  (cdr actuals)
                  processed-formals
                  processed-actuals
                  (cons (car actuals) for-effect)))
          (else (loop2 (cdr formals)
                       (cdr actuals)
                       (cons (car formals) processed-formals)
                       (cons (car actuals) processed-actuals)
                       for-effect))))
  
  (define (return2 rev-formals rev-actuals rev-for-effect)
    (let ((formals (reverse rev-formals))
          (actuals (reverse rev-actuals))
          (for-effect (reverse rev-for-effect)))
      (lambda.args-set! proc formals)
      (call.args-set! exp actuals)
      (let ((exp (if (and (null? actuals)
                          (or (null? (lambda.defs proc))
                              (and (notepad.parent notepad)
                                   (POLICY:LIFT? proc
                                                 (notepad.parent notepad)
                                                 (map (lambda (def) '())
                                                      (lambda.defs proc))))))
                     (begin (for-each (lambda (I)
                                        (notepad-var-add! notepad I))
                                      (lambda.F proc))
                            (if (not (null? (lambda.defs proc)))
                                (let ((parent (notepad.parent notepad))
                                      (defs (lambda.defs proc))
                                      (R (lambda.R proc)))
                                  (lambda.defs-set!
                                    parent
                                    (append defs (lambda.defs parent)))
                                  (lambda.defs-set! proc '())
                                  (lambda.R-set!
                                    parent
                                    (append (map (lambda (def)
                                                   (R-lookup R (def.lhs def)))
                                                 defs)
                                            (lambda.R parent)))))
                            (lambda.body proc))
                     exp)))
        (if (null? for-effect)
            exp
            (post-simplify-begin (make-begin (append for-effect (list exp)))
                                 notepad)))))
  
  (notepad-nonescaping-add! notepad proc)
  (loop1 (lambda.args proc) (call.args exp) '() '()))

; Single assignment analysis performs the transformation
;
;    (lambda (... I ...)
;      (begin D ...)
;      (quote (... (I <references> ((set! I L)) <calls>) ...) ...)
;      (begin (set! I L) E1 ...))
; -> (lambda (... IGNORED ...)
;      (begin (define I L) D ...)
;      (quote (... (I <references> () <calls>) ...) ...)
;      (begin E1 ...))
;
; For best results, pass 1 should sort internal definitions and LETRECs so
; that procedure definitions/bindings come first.
;
; This procedure operates by side effect.

(define (single-assignment-analysis L notepad)
  (let ((formals (lambda.args L))
        (defs (lambda.defs L))
        (R (lambda.R L))
        (body (lambda.body L)))
    (define (finish! exprs escapees)
      (begin.exprs-set! body
                        (append (reverse escapees)
                                exprs))
      (lambda.body-set! L (post-simplify-begin body '())))
    (if (begin? body)
        (let loop ((exprs (begin.exprs body))
                   (escapees '()))
          (let ((first (car exprs)))
            (if (and (assignment? first)
                     (not (null? (cdr exprs))))
                (let ((I (assignment.lhs first))
                      (rhs (assignment.rhs first)))
                  (if (and (lambda? rhs)
                           (local? R I)
                           (= 1 (length (assignments R I))))
                      (if (= (length (calls R I))
                             (length (references R I)))
                          (begin (notepad-nonescaping-add! notepad rhs)
                                 (flag-as-ignored I L)
                                 (lambda.defs-set! L
                                   (cons (make-definition I rhs)
                                         (lambda.defs L)))
                                 (assignments-set! R I '())
                                 (standardize-known-calls
                                  rhs
                                  (R-entry.calls (R-lookup R I)))
                                 (loop (cdr exprs) escapees))
                          (loop (cdr exprs)
                                (cons (car exprs) escapees)))
                      (finish! exprs escapees)))
                (finish! exprs escapees)))))))

(define (standardize-known-calls L calls)
  (let ((formals (lambda.args L)))
    (cond ((not (list? formals))
           (let* ((newformals (make-null-terminated formals))
                  (n (- (length newformals) 1)))
             (lambda.args-set! L newformals)
             (for-each (lambda (call)
                         (if (>= (length (call.args call)) n)
                             (call.args-set!
                              call
                              (append (list-head (call.args call) n)
                                      (list
                                       (make-call-to-LIST
                                        (list-tail (call.args call) n)))))
                             (pass2-error p2error:wna call)))
                       calls)))
          (else (let ((n (length formals)))
                  (for-each (lambda (call)
                              (if (not (= (length (call.args call)) n))
                                  (pass2-error p2error:wna call)))
                            calls))))))
; Copyright 1991 William D Clinger.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 13 November 1998
;
; Second pass of the Twobit compiler, part 2:
;   single assignment elimination, assignment elimination,
;   and lambda lifting.
;
; See part 1 for further documentation.

; Single assignment elimination performs the transformation
;
;    (lambda (... I1 ... In ...)
;      (begin D ...)
;      (begin (set! I1 E1)
;             ...
;             (set! In En)
;             E ...))
; -> (lambda (... IGNORED ... IGNORED ...)
;      (let* ((I1 E1) ... (In En))
;        (begin D ...)
;        (begin E ...)))
;
; provided for each k:
;
;    1.  Ik does not occur in E1, ..., Ek.
;    2.  Either E1 through Ek contain no procedure calls
;        or Ik is not referenced by an escaping lambda expression.
;    3.  Ik is assigned only once.
;
; I doubt whether the third condition is really necessary, but
; dropping it would involve a more complex calculation of the
; revised referencing information.
;
; A more precise description of the transformation:
;
;    (lambda (... I1 ... In ...)
;      (begin (define F1 L1) ...)
;      (quote (... (I1 <references> ((set! I1 E1)) <calls>) ...
;                  (In <references> ((set! In En)) <calls>)
;                  (F1 <references> () <calls>) ...) ...)
;      (begin (set! I1 E1) ... (set! In En) E ...))
; -> (lambda (... IGNORED ... IGNORED ...)
;      (begin)
;      (quote (...) ...)
;      ((lambda (I1)
;         (begin)
;         (quote ((I1 <references> () <calls>)) ...)
;         ...
;           ((lambda (In)
;              (begin (define F1 L1) ...)
;              (quote (... (In <references> () <calls>)
;                          (F1 <references> () <calls>) ...) ...)
;              (begin E ...))
;            En)
;         ...)
;       E1))
;
; For best results, pass 1 should sort internal definitions and LETRECs
; so that procedure definitions/bindings come first, followed by
; definitions/bindings whose right hand side contains no calls,
; followed by definitions/bindings of variables that do not escape,
; followed by all other definitions/bindings.
;
; Pass 1 can't tell which variables escape, however.  Pass 2 can't tell
; which variables escape either until all enclosed lambda expressions
; have been simplified and the first transformation above has been
; performed.  That is why single assignment analysis precedes single
; assignment elimination.  As implemented here, an assignment that does
; not satisfy the conditions above will prevent the transformation from
; being applied to any subsequent assignments.
;
; This procedure operates by side effect.

(define (single-assignment-elimination L notepad)
  
  (if (begin? (lambda.body L))
      
      (let* ((formals (make-null-terminated (lambda.args L)))
             (defined (map def.lhs (lambda.defs L)))
             (escaping (intersection formals
                                     (notepad-captured-variables notepad)))
             (R (lambda.R L)))
        
        ; Given:
        ;    exprs that remain in the body;
        ;    assigns that will be replaced by let* variables;
        ;    call-has-occurred?, a boolean;
        ;    free variables of the assigns;
        ; Performs the transformation described above.
        
        (define (loop exprs assigns call-has-occurred? free)
          (cond ((null? (cdr exprs))
                 (return exprs assigns))
                ((assignment? (car exprs))
                 (let ((I1 (assignment.lhs (car exprs)))
                       (E1 (assignment.rhs (car exprs))))
                   (if (and (memq I1 formals)
                            (= (length (assignments R I1)) 1)
                            (not (and call-has-occurred?
                                      (memq I1 escaping))))
                       (let* ((free-in-E1 (free-variables E1))
                              (newfree (union free-in-E1 free)))
                         (if (or (memq I1 newfree)
                                 (not
                                  (empty-set?
                                   (intersection free-in-E1 defined))))
                             (return exprs assigns)
                             (loop (cdr exprs)
                                   (cons (car exprs) assigns)
                                   (or call-has-occurred?
                                       (might-return-twice? E1))
                                   newfree)))
                       (return exprs assigns))))
                (else (return exprs assigns))))
        
        (define (return exprs assigns)
          (if (not (null? assigns))
              (let ((I (assignment.lhs (car assigns)))
                    (E (assignment.rhs (car assigns)))
                    (defs (lambda.defs L))
                    (F (lambda.F L))
                    (G (lambda.G L)))
                (flag-as-ignored I L)
                (assignments-set! R I '())
                (let ((L2 (make-lambda (list I)
                                       defs
                                       (cons (R-entry R I)
                                             (map (lambda (def)
                                                    (R-entry R (def.lhs def)))
                                                  defs))
                                       F
                                       G
                                       (lambda.decls L)
                                       (lambda.doc L)
                                       (make-begin exprs))))
                  (lambda.defs-set! L '())
                  (for-each (lambda (entry)
                              (lambda.R-set! L (remq entry R)))
                            (lambda.R L2))
                  (return-loop (cdr assigns) (make-call L2 (list E)))))))
        
        (define (return-loop assigns body)
          (if (null? assigns)
              (let ((L3 (call.proc body)))
                (lambda.body-set! L body)
                (lambda-lifting L3 L))
              (let* ((I (assignment.lhs (car assigns)))
                     (E (assignment.rhs (car assigns)))
                     (L3 (call.proc body))
                     (F (remq I (lambda.F L3)))
                     (G (remq I (lambda.G L3))))
                (flag-as-ignored I L)
                (assignments-set! R I '())
                (let ((L2 (make-lambda (list I)
                                       '()
                                       (list (R-entry R I))
                                       F
                                       G
                                       (lambda.decls L)
                                       (lambda.doc L)
                                       body)))
                  (lambda.R-set! L (remq (R-entry R I) R))
                  (lambda-lifting L3 L2)
                  (return-loop (cdr assigns) (make-call L2 (list E)))))))
        
        (loop (begin.exprs (lambda.body L)) '() #f '())))
  
  L)

; Temporary definitions.

(define (free-variables exp)
  (case (car exp)
    ((quote)    '())
    ((lambda)   (difference (lambda.F exp)
                            (make-null-terminated (lambda.args exp))))
    ((set!)     (union (list (assignment.lhs exp))
                       (free-variables (assignment.rhs exp))))
    ((if)       (union (free-variables (if.test exp))
                       (free-variables (if.then exp))
                       (free-variables (if.else exp))))
    ((begin)    (if (variable? exp)
                    (list (variable.name exp))
                    (apply union (map free-variables (begin.exprs exp)))))
    (else       (apply union (map free-variables exp)))))

(define (might-return-twice? exp)
  (case (car exp)
    ((quote)    #f)
    ((lambda)   #f)
    ((set!)     (might-return-twice? (assignment.rhs exp)))
    ((if)       (or (might-return-twice? (if.test exp))
                    (might-return-twice? (if.then exp))
                    (might-return-twice? (if.else exp))))
    ((begin)    (if (variable? exp)
                    #f
                    (some? might-return-twice? (begin.exprs exp))))
    (else       #t)))


; Assignment elimination replaces variables that appear on the left
; hand side of an assignment by data structures.  This is necessary
; to avoid some nasty complications with lambda lifting.
;
; This procedure operates by side effect.

(define (assignment-elimination L)
  (let ((R (lambda.R L)))
    
    ; Given a list of entries, return those for assigned variables.
    
    (define (loop entries assigned)
      (cond ((null? entries)
             (if (not (null? assigned))
                 (eliminate assigned)))
            ((not (null? (R-entry.assignments (car entries))))
             (loop (cdr entries) (cons (car entries) assigned)))
            ((null? (R-entry.references (car entries)))
             (flag-as-ignored (R-entry.name (car entries)) L)
             (loop (cdr entries) assigned))
            (else (loop (cdr entries) assigned))))
    
    ; Given a list of entries for assigned variables I1 ...,
    ; remove the assignments by replacing the body by a LET of the form
    ; ((LAMBDA (V1 ...) ...) (MAKE-CELL I1) ...), by replacing references
    ; by calls to CELL-REF, and by replacing assignments by calls to
    ; CELL-SET!.
    
    (define (eliminate assigned)
      (let* ((oldnames (map R-entry.name assigned))
             (newnames (map generate-new-name oldnames)))
        (let ((augmented-entries (map list newnames assigned))
              (renaming-alist (map cons oldnames newnames))
              (defs (lambda.defs L)))
          (for-each cellify! augmented-entries)
          (for-each (lambda (def)
                      (do ((free (lambda.F (def.rhs def)) (cdr free)))
                          ((null? free))
                          (let ((z (assq (car free) renaming-alist)))
                            (if z
                                (set-car! free (cdr z))))))
                    defs)
          (let ((newbody
                 (make-call
                  (make-lambda (map car augmented-entries)
                               defs
                               (union (map (lambda (def)
                                             (R-entry R (def.lhs def)))
                                           defs)
                                      (map new-reference-info augmented-entries))
                               (union (list name:CELL-REF name:CELL-SET!)
                                      newnames
                                      (difference (lambda.F L) oldnames))
                               (union (list name:CELL-REF name:CELL-SET!)
                                      newnames
                                      (difference (lambda.G L) oldnames))
                               (lambda.decls L)
                               (lambda.doc L)
                               (lambda.body L))
                  (map (lambda (name)
                         (make-call (make-variable name:MAKE-CELL)
                                    (list (make-variable name))))
                       (map R-entry.name assigned)))))
            (lambda.F-set! L (union (list name:MAKE-CELL name:CELL-REF name:CELL-SET!)
                                    (difference (lambda.F L)
                                                (map def.lhs (lambda.defs L)))))
            (lambda.defs-set! L '())
            (for-each update-old-reference-info!
                      (map (lambda (arg)
                             (car (call.args arg)))
                           (call.args newbody)))
            (lambda.body-set! L newbody)
            (lambda-lifting (call.proc newbody) L)))))
    
    (define (generate-new-name name)
      (string->symbol (string-append cell-prefix (symbol->string name))))
    
    ; In addition to replacing references and assignments involving the
    ; old variable by calls to CELL-REF and CELL-SET! on the new, CELLIFY!
    ; uses the old entry to collect the referencing information for the
    ; new variable.
    
    (define (cellify! augmented-entry)
      (let ((newname (car augmented-entry))
            (entry (cadr augmented-entry)))
        (do ((refs (R-entry.references entry)
                   (cdr refs)))
            ((null? refs))
            (let* ((reference (car refs))
                   (newref (make-variable newname)))
              (set-car! reference (make-variable name:CELL-REF))
              (set-car! (cdr reference) newref)
              (set-car! refs newref)))
        (do ((assigns (R-entry.assignments entry)
                      (cdr assigns)))
            ((null? assigns))
            (let* ((assignment (car assigns))
                   (newref (make-variable newname)))
              (set-car! assignment (make-variable name:CELL-SET!))
              (set-car! (cdr assignment) newref)
              (R-entry.references-set! entry
                                       (cons newref
                                             (R-entry.references entry)))))
        (R-entry.assignments-set! entry '())))
    
    ; This procedure creates a brand new entry for a new variable, extracting
    ; the references stored in the old entry by CELLIFY!.
    
    (define (new-reference-info augmented-entry)
      (make-R-entry (car augmented-entry)
                    (R-entry.references (cadr augmented-entry))
                    '()
                    '()))
    
    ; This procedure updates the old entry to reflect the fact that it is
    ; now referenced once and never assigned.
    
    (define (update-old-reference-info! ref)
      (references-set! R (variable.name ref) (list ref))
      (assignments-set! R (variable.name ref) '())
      (calls-set! R (variable.name ref) '()))
    
    (loop R '())))

; Lambda lifting raises internal definitions to outer scopes to avoid
; having to choose between creating a closure or losing tail recursion.
; If L is not #f, then L2 is a lambda expression nested within L.
; Any internal definitions that occur within L2 may be lifted to L
; by adding extra arguments to the defined procedure and to all calls to it.
; Lambda lifting is not a clear win, because the extra arguments could
; easily become more expensive than creating a closure and referring
; to the non-local arguments through the closure.  The heuristics used
; to decide whether to lift a group of internal definitions are isolated
; within the POLICY:LIFT? procedure.

; L2 can be the same as L, so the order of side effects is critical.

(define (lambda-lifting L2 L)
  
  ; The call to sort is optional.  It gets the added arguments into
  ; the same order they appear in the formals list, which is an
  ; advantage for register targeting.
  
  (define (lift L2 L args-to-add)
    (let ((formals (make-null-terminated (lambda.args L2))))
      (do ((defs (lambda.defs L2) (cdr defs))
           (args-to-add args-to-add (cdr args-to-add)))
          ((null? defs))
          (let* ((def (car defs))
                 (entry (R-lookup (lambda.R L2) (def.lhs def)))
                 (calls (R-entry.calls entry))
                 (added (twobit-sort (lambda (x y)
                                       (let ((xx (memq x formals))
                                             (yy (memq y formals)))
                                         (if (and xx yy)
                                             (> (length xx) (length yy))
                                             #t)))
                                     (car args-to-add)))
                 (L3 (def.rhs def)))
            ; The flow equation guarantees that these added arguments
            ; will occur free by the time this round of lifting is done.
            (lambda.F-set! L3 (union added (lambda.F L3)))
            (lambda.args-set! L3 (append added (lambda.args L3)))
            (for-each (lambda (call)
                        (let ((newargs (map make-variable added)))
                          ; The referencing information is made obsolete here!
                          (call.args-set! call
                                          (append newargs (call.args call)))))
                      calls)
            (lambda.R-set! L2 (remq entry (lambda.R L2)))
            (lambda.R-set! L (cons entry (lambda.R L)))
            ))
      (if (not (eq? L2 L))
          (begin
           (lambda.defs-set! L (append (lambda.defs L2) (lambda.defs L)))
           (lambda.defs-set! L2 '())))))
  
  (if L
      (if (not (null? (lambda.defs L2)))
          (let ((args-to-add (compute-added-arguments
                              (lambda.defs L2)
                              (make-null-terminated (lambda.args L2)))))
            (if (POLICY:LIFT? L2 L args-to-add)
                (lift L2 L args-to-add))))))

; Given a list of definitions ((define f1 ...) ...) and a set of formals
; N over which the definitions may be lifted, returns a list of the
; subsets of N that need to be added to each procedure definition
; as new arguments.
;
; Algorithm: Let F_i be the variables that occur free in the body of
; the lambda expression associated with f_i.  Construct the call graph.
; Solve the flow equations
;
;     A_i = (F_i /\ N) \/ (\/ {A_j | A_i calls A_j})
;
; where /\ is intersection and \/ is union.

(define (compute-added-arguments defs formals)
  (let ((procs (map def.lhs defs))
        (freevars (map lambda.F (map def.rhs defs))))
    (let ((callgraph (map (lambda (names)
                            (map (lambda (name)
                                   (position name procs))
                                 (intersection names procs)))
                          freevars))
          (added_0 (map (lambda (names)
                          (intersection names formals))
                        freevars)))
      (vector->list
       (compute-fixedpoint
        (make-vector (length procs) '())
        (list->vector (map (lambda (term0 indexes)
                             (lambda (approximations)
                               (union term0
                                      (apply union
                                             (map (lambda (i)
                                                    (vector-ref approximations i))
                                                  indexes)))))
                           added_0
                           callgraph))
        set-equal?)))))

(define (position x l)
  (cond ((eq? x (car l)) 0)
        (else (+ 1 (position x (cdr l))))))

; Given a vector of starting approximations,
; a vector of functions that compute a next approximation
; as a function of the vector of approximations,
; and an equality predicate,
; returns a vector of fixed points.

(define (compute-fixedpoint v functions equiv?)
  (define (loop i flag)
    (if (negative? i)
        (if flag
            (loop (- (vector-length v) 1) #f)
            v)
        (let ((next_i ((vector-ref functions i) v)))
          (if (equiv? next_i (vector-ref v i))
              (loop (- i 1) flag)
              (begin (vector-set! v i next_i)
                     (loop (- i 1) #t))))))
  (loop (- (vector-length v) 1) #f))


; Given a lambda expression L2, its parent lambda expression
; L (which may be the same as L2, or #f), and a list of the
; lists of arguments that would need to be added to known
; local procedures, returns #t iff lambda lifting should be done.
;
; Here are some heuristics:
;
;   Don't lift if it means adding too many arguments.
;   Don't lift large groups of definitions.
;   In questionable cases it is better to lift to an outer
;     lambda expression that already contains internal
;     definitions than to one that doesn't.
;   It is better not to lift if the body contains a lambda
;     expression that has to be closed anyway.

(define (POLICY:LIFT? L2 L args-to-add)
  (and (lambda-optimizations)
       (not (lambda? (lambda.body L2)))
       (every? (lambda (addlist)
                 (< (length addlist) 6))
               args-to-add)))
; Copyright 1991 William D Clinger (for SIMPLIFY-CONDITIONAL)
; Copyright 1999 William D Clinger (for everything else)
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 11 April 1999.
;
; Some source transformations on IF expressions:
;
; (if '#f E1 E2)                      E2
; (if 'K  E1 E2)                      E1                    K != #f
; (if (if B0 '#f '#f) E1 E2)          (begin B0 E2)
; (if (if B0 '#f 'K ) E1 E2)          (if B0 E2 E1)         K != #f
; (if (if B0 'K  '#f) E1 E2)          (if B0 E1 E2)         K != #f
; (if (if B0 'K1 'K2) E1 E2)          (begin B0 E1)         K1, K2 != #f
; (if (if B0 (if B1 #t #f) B2) E1 E2) (if (if B0 B1 B2) E1 E2)
; (if (if B0 B1 (if B2 #t #f)) E1 E2) (if (if B0 B1 B2) E1 E2)
; (if (if X  X   B0 ) E1 E2)          (if (if X #t B0) E1 E2)   X a variable
; (if (if X  B0  X  ) E1 E2)          (if (if X B0 #f) E1 E2)   X a variable
; (if ((lambda (X)                    (if ((lambda (X)
;        (if X X B2)) B0)                    (if X #t (if B2 #t #f))) B0)
;     E1 E2)                              E1 E2)
; (if (begin ... B0) E1 E2)           (begin ... (if B0 E1 E2))
; (if (not E0) E1 E2)                 (if E0 E2 E1)         not is integrable
;
; FIXME:  Three of the transformations above are intended to clean up
; the output of the OR macro.  It isn't yet clear how well this works.

(define (simplify-conditional exp notepad)
  (define (coercion-to-boolean? exp)
    (and (conditional? exp)
         (let ((E1 (if.then exp))
               (E2 (if.else exp)))
           (and (constant? E1)
                (eq? #t (constant.value E1))
                (constant? E2)
                (eq? #f (constant.value E2))))))
  (if (not (control-optimization))
      (begin (if.test-set! exp (simplify (if.test exp) notepad))
             (if.then-set! exp (simplify (if.then exp) notepad))
             (if.else-set! exp (simplify (if.else exp) notepad))
             exp)
      (let* ((test (if.test exp)))
        (if (and (call? test)
                 (lambda? (call.proc test))
                 (let* ((L (call.proc test))
                        (body (lambda.body L)))
                   (and (conditional? body)
                        (let ((R (lambda.R L))
                              (B0 (if.test body))
                              (B1 (if.then body)))
                          (and (variable? B0)
                               (variable? B1)
                               (let ((x (variable.name B0)))
                                 (and (eq? x (variable.name B1))
                                      (local? R x)
                                      (= 1 (length R))
                                      (= 1 (length (call.args test))))))))))
            (let* ((L (call.proc test))
                   (R (lambda.R L))
                   (body (lambda.body L))
                   (ref (if.then body))
                   (x (variable.name ref))
                   (entry (R-entry R x)))
              (if.then-set! body (make-constant #t))
              (if.else-set! body
                            (make-conditional (if.else body)
                                              (make-constant #t)
                                              (make-constant #f)))
              (R-entry.references-set! entry
                                       (remq ref
                                             (R-entry.references entry)))
              (simplify-conditional exp notepad))
            (let loop ((test (simplify (if.test exp) notepad)))
              (if.test-set! exp test)
              (cond ((constant? test)
                     (simplify (if (constant.value test)
                                   (if.then exp)
                                   (if.else exp))
                               notepad))
                    ((and (conditional? test)
                          (constant? (if.then test))
                          (constant? (if.else test)))
                     (cond ((and (constant.value (if.then test))
                                 (constant.value (if.else test)))
                            (post-simplify-begin
                             (make-begin (list (if.test test)
                                               (simplify (if.then exp)
                                                         notepad)))
                             notepad))
                           ((and (not (constant.value (if.then test)))
                                 (not (constant.value (if.else test))))
                            (post-simplify-begin
                             (make-begin (list (if.test test)
                                               (simplify (if.else exp)
                                                         notepad)))
                             notepad))
                           (else (if (not (constant.value (if.then test)))
                                     (let ((temp (if.then exp)))
                                       (if.then-set! exp (if.else exp))
                                       (if.else-set! exp temp)))
                                 (if.test-set! exp (if.test test))
                                 (loop (if.test exp)))))
                    ((and (conditional? test)
                          (or (coercion-to-boolean? (if.then test))
                              (coercion-to-boolean? (if.else test))))
                     (if (coercion-to-boolean? (if.then test))
                         (if.then-set! test (if.test (if.then test)))
                         (if.else-set! test (if.test (if.else test))))
                     (loop test))
                    ((and (conditional? test)
                          (variable? (if.test test))
                          (let ((x (variable.name (if.test test))))
                            (or (and (variable? (if.then test))
                                     (eq? x (variable.name (if.then test)))
                                     1)
                                (and (variable? (if.else test))
                                     (eq? x (variable.name (if.else test)))
                                     2))))
                     =>
                     (lambda (n)
                       (case n
                         ((1) (if.then-set! test (make-constant #t)))
                         ((2) (if.else-set! test (make-constant #f))))
                       (loop test)))
                    ((begin? test)
                     (let ((exprs (reverse (begin.exprs test))))
                       (if.test-set! exp (car exprs))
                       (post-simplify-begin
                        (make-begin (reverse (cons (loop (car exprs))
                                                   (cdr exprs))))
                        notepad)))
                    ((and (call? test)
                          (variable? (call.proc test))
                          (eq? (variable.name (call.proc test)) name:NOT)
                          (integrable? name:NOT)
                          (integrate-usual-procedures)
                          (= (length (call.args test)) 1))
                     (let ((temp (if.then exp)))
                       (if.then-set! exp (if.else exp))
                       (if.else-set! exp temp))
                     (loop (car (call.args test))))
                    (else
                     (simplify-case exp notepad))))))))

; Given a conditional expression whose test has been simplified,
; simplifies the then and else parts while applying optimizations
; for CASE expressions.
; Precondition: (control-optimization) is true.

(define (simplify-case exp notepad)
  (let ((E0 (if.test exp)))
    (if (and (call? E0)
             (variable? (call.proc E0))
             (let ((name (variable.name (call.proc E0))))
               ; FIXME: Should ensure that the name is integrable,
               ; but MEMQ and MEMV probably aren't according to the
               ; INTEGRABLE? predicate.
               (or (eq? name name:EQ?)
                   (eq? name name:EQV?)
                   (eq? name name:MEMQ)
                   (eq? name name:MEMV)))
             (integrate-usual-procedures)
             (= (length (call.args E0)) 2)
             (variable? (car (call.args E0)))
             (constant? (cadr (call.args E0))))
        (simplify-case-clauses (variable.name (car (call.args E0)))
                               exp
                               notepad)
        (begin (if.then-set! exp (simplify (if.then exp) notepad))
               (if.else-set! exp (simplify (if.else exp) notepad))
               exp))))

; Code generation for case expressions.
;
; A case expression turns into a conditional expression
; of the form
;
; CASE{I}  ::=  E  |  (if (PRED I K) E CASE{I})
; PRED  ::=  memv  |  memq  |  eqv?  |  eq?
;
; The memq and eq? predicates are used when the constant
; is a (list of) boolean, fixnum, char, empty list, or symbol.
; The constants will almost always be of these types.
;
; The first step is to remove duplicated constants and to
; collect all the case clauses, sorting them into the following
; categories based on their simplified list of constants:
;     constants are fixnums
;     constants are characters
;     constants are symbols
;     constants are of mixed or other type
; After duplicated constants have been removed, the predicates
; for these clauses can be tested in any order.

; Given the name of an arbitrary variable, an expression that
; has not yet been simplified or can safely be simplified again,
; and a notepad, returns the expression after simplification.
; If the expression is equivalent to a case expression that dispatches
; on the given variable, then case-optimization will be applied.

(define (simplify-case-clauses var0 E notepad)
  
  (define notepad2 (make-notepad (notepad.parent notepad)))
  
  (define (collect-clauses E fix chr sym other constants)
    (if (not (conditional? E))
        (analyze (simplify E notepad2)
                 fix chr sym other constants)
        (let ((test (simplify (if.test E) notepad2))
              (code (simplify (if.then E) notepad2)))
          (if.test-set! E test)
          (if.then-set! E code)
          (if (not (call? test))
              (finish E fix chr sym other constants)
              (let ((proc (call.proc test))
                    (args (call.args test)))
                (if (not (and (variable? proc)
                              (let ((name (variable.name proc)))
                                ; FIXME: See note above.
                                (or (eq? name name:EQ?)
                                    (eq? name name:EQV?)
                                    (eq? name name:MEMQ)
                                    (eq? name name:MEMV)))
                              (= (length args) 2)
                              (variable? (car args))
                              (eq? (variable.name (car args)) var0)
                              (constant? (cadr args))))
                    (finish E fix chr sym other constants)
                    (let ((pred (variable.name proc))
                          (datum (constant.value (cadr args))))
                      ; FIXME
                      (if (or (and (or (eq? pred name:MEMV)
                                       (eq? pred name:MEMQ))
                                   (not (list? datum)))
                              (and (eq? pred name:EQ?)
                                   (not (eqv-is-ok? datum)))
                              (and (eq? pred name:MEMQ)
                                   (not (every? (lambda (datum)
                                                  (eqv-is-ok? datum))
                                                datum))))
                          (finish E fix chr sym other constants)
                          (call-with-values
                           (lambda ()
                             (remove-duplicates (if (or (eq? pred name:EQV?)
                                                        (eq? pred name:EQ?))
                                                    (list datum)
                                                    datum)
                                                constants))
                           (lambda (data constants)
                             (let ((clause (list data code))
                                   (E2 (if.else E)))
                               (cond ((every? smallint? data)
                                      (collect-clauses E2
                                                       (cons clause fix)
                                                       chr
                                                       sym
                                                       other
                                                       constants))
                                     ((every? char? data)
                                      (collect-clauses E2
                                                       fix
                                                       (cons clause chr)
                                                       sym
                                                       other
                                                       constants))
                                     ((every? symbol? data)
                                      (collect-clauses E2
                                                       fix
                                                       chr
                                                       (cons clause sym)
                                                       other
                                                       constants))
                                     (else
                                      (collect-clauses E2
                                                       fix
                                                       chr
                                                       sym
                                                       (cons clause other)
                                                       constants))))))))))))))
  
  (define (remove-duplicates data set)
    (let loop ((originals data)
               (data '())
               (set set))
      (if (null? originals)
          (values data set)
          (let ((x (car originals))
                (originals (cdr originals)))
            (if (memv x set)
                (loop originals data set)
                (loop originals (cons x data) (cons x set)))))))
  
  (define (finish E fix chr sym other constants)
    (if.else-set! E (simplify (if.else E) notepad2))
    (analyze E fix chr sym other constants))
  
  (define (analyze default fix chr sym other constants)
    (notepad-var-add! notepad2 var0)
    (for-each (lambda (L)
                (notepad-lambda-add! notepad L))
              (notepad.lambdas notepad2))
    (for-each (lambda (L)
                (notepad-nonescaping-add! notepad L))
              (notepad.nonescaping notepad2))
    (for-each (lambda (var)
                (notepad-var-add! notepad var))
              (append (list name:FIXNUM?
                            name:CHAR?
                            name:SYMBOL?
                            name:FX<
                            name:FX-
                            name:CHAR->INTEGER
                            name:VECTOR-REF)
                      (notepad.vars notepad2)))
    (analyze-clauses (notepad.vars notepad2)
                     var0
                     default
                     (reverse fix)
                     (reverse chr)
                     (reverse sym)
                     (reverse other)
                     constants))
  
  (collect-clauses E '() '() '() '() '()))

; Returns true if EQ? and EQV? behave the same on x.

(define (eqv-is-ok? x)
  (or (smallint? x)
      (char? x)
      (symbol? x)
      (boolean? x)))

; Returns true if EQ? and EQV? behave the same on x.

(define (eq-is-ok? x)
  (eqv-is-ok? x))

; Any case expression that dispatches on a variable var0 and whose
; constants are disjoint can be compiled as
;
; (let ((n (cond ((eq? var0 'K1) ...)   ; miscellaneous constants
;                ...
;                ((fixnum? var0)
;                 <dispatch-on-fixnum>)
;                ((char? var0)
;                 <dispatch-on-char>)
;                ((symbol? var0)
;                 <dispatch-on-symbols>)
;                (else 0))))
;   <dispatch-on-case-number>)
;
; where the <dispatch-on-case-number> uses binary search within
; the interval [0, p+1), where p is the number of non-default cases.
;
; On the SPARC, sequential search is faster if there are fewer than
; 8 constants, and sequential search uses less than half the space
; if there are fewer than 10 constants.  Most target machines should
; similar, so I'm hard-wiring this constant.
; FIXME:  The hardwired constant is annoying.

(define (analyze-clauses F var0 default fix chr sym other constants)
  (cond ((or (and (null? fix)
                  (null? chr))
             (< (length constants) 12))
         (implement-clauses-by-sequential-search var0
                                                 default
                                                 (append fix chr sym other)))
        (else
         (implement-clauses F var0 default fix chr sym other constants))))

; Implements the general technique described above.

(define (implement-clauses F var0 default fix chr sym other constants)
  (let* ((name:n ((make-rename-procedure) 'n))
         ; Referencing information is destroyed by pass 2.
         (entry (make-R-entry name:n '() '() '()))
         (F (union (make-set (list name:n)) F))
         (L (make-lambda
             (list name:n)
             '()
             '()  ; entry
             F
             '()
             '()
             #f
             (implement-case-dispatch
              name:n
              (cons default
                    (map cadr
                         ; The order here must match the order
                         ; used by IMPLEMENT-DISPATCH.
                         (append other fix chr sym)))))))
    (make-call L
               (list (implement-dispatch 0
                                         var0
                                         (map car other)
                                         (map car fix)
                                         (map car chr)
                                         (map car sym))))))

(define (implement-case-dispatch var0 exprs)
  (implement-intervals var0
                       (map (lambda (n code)
                              (list n (+ n 1) code))
                            (iota (length exprs))
                            exprs)))

; Given the number of prior clauses,
; the variable on which to dispatch,
; a list of constant lists for mixed or miscellaneous clauses,
; a list of constant lists for the fixnum clauses,
; a list of constant lists for the character clauses, and
; a list of constant lists for the symbol clauses,
; returns code that computes the index of the selected clause.
; The mixed/miscellaneous clauses must be tested first because
; Twobit's SMALLINT? predicate might not be true of all fixnums
; on the target machine, which means that Twobit might classify
; some fixnums as miscellaneous.

(define (implement-dispatch prior var0 other fix chr sym)
  (cond ((not (null? other))
         (implement-dispatch-other
          (implement-dispatch (+ prior (length other))
                              var0 fix chr sym '())
          prior var other))
        ((not (null? fix))
         (make-conditional (make-call (make-variable name:FIXNUM?)
                                      (list (make-variable var0)))
                           (implement-dispatch-fixnum prior var0 fix)
                           (implement-dispatch (+ prior (length fix))
                                               var0 '() chr sym other)))
        ((not (null? chr))
         (make-conditional (make-call (make-variable name:CHAR?)
                                      (list (make-variable var0)))
                           (implement-dispatch-char prior var0 chr)
                           (implement-dispatch (+ prior (length chr))
                                               var0 fix '() sym other)))
        ((not (null? sym))
         (make-conditional (make-call (make-variable name:SYMBOL?)
                                      (list (make-variable var0)))
                           (implement-dispatch-symbol prior var0 sym)
                           (implement-dispatch (+ prior (length sym))
                                               var0 fix chr '() other)))
        (else
         (make-constant 0))))

; The value of var0 will be known to be a fixnum.
; Can use table lookup, binary search, or sequential search.
; FIXME: Never uses sequential search, which is best when
; there are only a few constants, with gaps between them.

(define (implement-dispatch-fixnum prior var0 lists)
  
  (define (calculate-intervals n lists)
    (define (loop n lists intervals)
      (if (null? lists)
          (twobit-sort (lambda (interval1 interval2)
                         (< (car interval1) (car interval2)))
                       intervals)
          (let ((constants (twobit-sort < (car lists))))
            (loop (+ n 1)
                  (cdr lists)
                  (append (extract-intervals n constants)
                          intervals)))))
    (loop n lists '()))
  
  (define (extract-intervals n constants)
    (if (null? constants)
        '()
        (let ((k0 (car constants)))
          (do ((constants (cdr constants) (cdr constants))
               (k1 (+ k0 1) (+ k1 1)))
              ((or (null? constants)
                   (not (= k1 (car constants))))
               (cons (list k0 k1 (make-constant n))
                     (extract-intervals n constants)))))))
  
  (define (complete-intervals intervals)
    (cond ((null? intervals)
           intervals)
          ((null? (cdr intervals))
           intervals)
          (else
           (let* ((i1 (car intervals))
                  (i2 (cadr intervals))
                  (end1 (cadr i1))
                  (start2 (car i2))
                  (intervals (complete-intervals (cdr intervals))))
             (if (= end1 start2)
                 (cons i1 intervals)
                 (cons i1
                       (cons (list end1 start2 (make-constant 0))
                             intervals)))))))
  
  (let* ((intervals (complete-intervals
                     (calculate-intervals (+ prior 1) lists)))
         (lo (car (car intervals)))
         (hi (car (car (reverse intervals))))
         (p (length intervals)))
    (make-conditional
     (make-call (make-variable name:FX<)
                (list (make-variable var0)
                      (make-constant lo)))
     (make-constant 0)
     (make-conditional
      (make-call (make-variable name:FX<)
                 (list (make-variable var0)
                       (make-constant (+ hi 1))))
      ; The static cost of table lookup is about hi - lo words.
      ; The static cost of binary search is about 5 SPARC instructions
      ; per interval.
      (if (< (- hi lo) (* 5 p))
          (implement-table-lookup var0 (+ prior 1) lists lo hi)
          (implement-intervals var0 intervals))
      (make-constant 0)))))

(define (implement-dispatch-char prior var0 lists)
  (let* ((lists (map (lambda (constants)
                       (map compat:char->integer constants))
                     lists))
         (name:n ((make-rename-procedure) 'n))
         ; Referencing information is destroyed by pass 2.
         ;(entry (make-R-entry name:n '() '() '()))
         (F (list name:n name:EQ? name:FX< name:FX- name:VECTOR-REF))
         (L (make-lambda
             (list name:n)
             '()
             '()  ; entry
             F
             '()
             '()
             #f
             (implement-dispatch-fixnum prior name:n lists))))
    (make-call L
               (make-call (make-variable name:CHAR->INTEGER)
                          (list (make-variable var0))))))

(define (implement-dispatch-symbol prior var0 lists)
  (implement-dispatch-other (make-constant 0) prior var0 lists))

(define (implement-dispatch-other default prior var0 lists)
  (if (null? lists)
      default
      (let* ((constants (car lists))
             (lists (cdr lists))
             (n (+ prior 1)))
      (make-conditional (make-call-to-memv var0 constants)
                        (make-constant n)
                        (implement-dispatch-other default n var0 lists)))))

(define (make-call-to-memv var0 constants)
  (cond ((null? constants)
         (make-constant #f))
        ((null? (cdr constants))
         (make-call-to-eqv var0 (car constants)))
        (else
         (make-conditional (make-call-to-eqv var0 (car constants))
                           (make-constant #t)
                           (make-call-to-memv var0 (cdr constants))))))

(define (make-call-to-eqv var0 constant)
  (make-call (make-variable
              (if (eq-is-ok? constant)
                  name:EQ?
                  name:EQV?))
             (list (make-variable var0)
                   (make-constant constant))))

; Given a variable whose value is known to be a fixnum,
; the clause index for the first fixnum clause,
; an ordered list of lists of constants for fixnum-only clauses,
; and the least and greatest constants in those lists,
; returns code for a table lookup.

(define (implement-table-lookup var0 index lists lo hi)
  (let ((v (make-vector (+ 1 (- hi lo)) 0)))
    (do ((index index (+ index 1))
         (lists lists (cdr lists)))
        ((null? lists))
        (for-each (lambda (k)
                    (vector-set! v (- k lo) index))
                  (car lists)))
    (make-call (make-variable name:VECTOR-REF)
               (list (make-constant v)
                     (make-call (make-variable name:FX-)
                                (list (make-variable var0)
                                      (make-constant lo)))))))

; Given a variable whose value is known to lie within the
; half-open interval [m0, mk), and an ordered complete
; list of intervals of the form
;
;     ((m0 m1 code0)
;      (m1 m2 code1)
;      ...
;      (m{k-1} mk code{k-1})
;     )
;
; returns an expression that finds the unique i such that
; var0 lies within [mi, m{i+1}), and then executes code{i}.

(define (implement-intervals var0 intervals)
  (if (null? (cdr intervals))
      (caddr (car intervals))
      (let ((n (quotient (length intervals) 2)))
        (do ((n n (- n 1))
             (intervals1 '() (cons (car intervals2) intervals1))
             (intervals2 intervals (cdr intervals2)))
            ((zero? n)
             (let ((intervals1 (reverse intervals1))
                   (m (car (car intervals2))))
               (make-conditional (make-call (make-variable name:FX<)
                                            (list
                                             (make-variable var0)
                                             (make-constant m)))
                                 (implement-intervals var0 intervals1)
                                 (implement-intervals var0 intervals2))))))))

; The brute force approach.
; Given the variable on which the dispatch is being performed, and
; actual (simplified) code for the default clause and
; for all other clauses,
; returns code to perform the dispatch by sequential search.

(define *memq-threshold* 20)
(define *memv-threshold* 4)

(define (implement-clauses-by-sequential-search var0 default clauses)
  (if (null? clauses)
      default
      (let* ((case1 (car clauses))
             (clauses (cdr clauses))
             (constants1 (car case1))
             (code1 (cadr case1)))
        (make-conditional (make-call-to-memv var0 constants1)
                          code1
                          (implement-clauses-by-sequential-search
                           var0 default clauses)))))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 13 April 1999.
;
; The tail and non-tail call graphs of known and unknown procedures.
;
; Given an expression E returned by pass 2 of Twobit,
; returns a list of the following form:
;
; ((#t     L ()     <tailcalls> <nontailcalls> <size> #f)
;  (<name> L <vars> <tailcalls> <nontailcalls> <size> #f)
;  ...)
;
; where
;
; Each L is a lambda expression that occurs within E
; as either an escaping lambda expression or as a known
; procedure.  If L is a known procedure, then <name> is
; its name; otherwise <name> is #f.
;
; <vars> is a list of the non-global variables within whose
; scope L occurs.
;
; <tailcalls> is a complete list of names of known local procedures
; that L calls tail-recursively, disregarding calls from other known
; procedures or escaping lambda expressions that occur within L.
;
; <nontailcalls> is a complete list of names of known local procedures
; that L calls non-tail-recursively, disregarding calls from other
; known procedures or escaping lambda expressions that occur within L.
;
; <size> is a measure of the size of L, including known procedures
; and escaping lambda expressions that occur within L.

(define (callgraphnode.name x) (car x))
(define (callgraphnode.code x) (cadr x))
(define (callgraphnode.vars x) (caddr x))
(define (callgraphnode.tailcalls x) (cadddr x))
(define (callgraphnode.nontailcalls x) (car (cddddr x)))
(define (callgraphnode.size x) (cadr (cddddr x)))
(define (callgraphnode.info x) (caddr (cddddr x)))

(define (callgraphnode.size! x v) (set-car! (cdr (cddddr x)) v) #f)
(define (callgraphnode.info! x v) (set-car! (cddr (cddddr x)) v) #f)

(define (callgraph exp)
  
  ; Returns (union (list x) z).
  
  (define (adjoin x z)
    (if (memq x z)
        z
        (cons x z)))
  
  (let ((result '()))
    
    ; Given a <name> as described above, a lambda expression, a list
    ; of variables that are in scope, and a list of names of known
    ; local procedure that are in scope, computes an entry for L and
    ; entries for any nested known procedures or escaping lambda
    ; expressions, and adds them to the result.
    
    (define (add-vertex! name L vars known)
      
      (let ((tailcalls '())
            (nontailcalls '())
            (size 0))
        
        ; Given an expression, a list of variables that are in scope,
        ; a list of names of known local procedures that are in scope,
        ; and a boolean indicating whether the expression occurs in a
        ; tail context, adds any tail or non-tail calls to known
        ; procedures that occur within the expression to the list
        ; variables declared above.
        
        (define (graph! exp vars known tail?)
          (set! size (+ size 1))
          (case (car exp)
            
            ((quote)    #f)
            
            ((lambda)   (add-vertex! #f exp vars known)
                        (set! size
                              (+ size
                                 (callgraphnode.size (car result)))))
            
            ((set!)     (graph! (assignment.rhs exp) vars known #f))
            
            ((if)       (graph! (if.test exp) vars known #f)
                        (graph! (if.then exp) vars known tail?)
                        (graph! (if.else exp) vars known tail?))
            
            ((begin)    (if (not (variable? exp))
                            (do ((exprs (begin.exprs exp) (cdr exprs)))
                                ((null? (cdr exprs))
                                 (graph! (car exprs) vars known tail?))
                                (graph! (car exprs) vars known #f))))
            
            (else       (let ((proc (call.proc exp)))
                          (cond ((variable? proc)
                                 (let ((name (variable.name proc)))
                                   (if (memq name known)
                                       (if tail?
                                           (set! tailcalls
                                                 (adjoin name tailcalls))
                                           (set! nontailcalls
                                                 (adjoin name nontailcalls))))))
                                 ((lambda? proc)
                                  (graph-lambda! proc vars known tail?))
                                 (else
                                  (graph! proc vars known #f)))
                          (for-each (lambda (exp)
                                      (graph! exp vars known #f))
                                    (call.args exp))))))
        
        (define (graph-lambda! L vars known tail?)
          (let* ((defs (lambda.defs L))
                 (newknown (map def.lhs defs))
                 (vars (append newknown
                               (make-null-terminated
                                (lambda.args L))
                               vars))
                 (known (append newknown known)))
            (for-each (lambda (def)
                        (add-vertex! (def.lhs def)
                                     (def.rhs def)
                                     vars
                                     known)
                        (set! size
                              (+ size
                                 (callgraphnode.size (car result)))))
                      defs)
            (graph! (lambda.body L) vars known tail?)))
        
        (graph-lambda! L vars known #t)
        
        (set! result
              (cons (list name L vars tailcalls nontailcalls size #f)
                    result))))
    
    (add-vertex! #t
                 (make-lambda '() '() '() '() '() '() '() exp)
                 '()
                 '())
    result))

; Displays the callgraph, for debugging.

(define (view-callgraph g)
  (for-each (lambda (entry)
              (let ((name (callgraphnode.name entry))
                    (exp  (callgraphnode.code entry))
                    (vars (callgraphnode.vars entry))
                    (tail (callgraphnode.tailcalls entry))
                    (nt   (callgraphnode.nontailcalls entry))
                    (size (callgraphnode.size entry)))
                (cond ((symbol? name)
                       (write name))
                      (name
                       (display "TOP LEVEL EXPRESSION"))
                      (else
                       (display "ESCAPING LAMBDA EXPRESSION")))
                (display ":")
                (newline)
                (display "Size: ")
                (write size)
                (newline)
                ;(newline)
                ;(display "Variables in scope: ")
                ;(write vars)
                ;(newline)
                (display "Tail calls:     ")
                (write tail)
                (newline)
                (display "Non-tail calls: ")
                (write nt)
                (newline)
                ;(newline)
                ;(pretty-print (make-readable exp))
                ;(newline)
                ;(newline)
                (newline)))
            g))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 14 April 1999.
;
; Inlining of known local procedures.
;
; First find the known and escaping procedures and compute the call graph.
;
; If a known local procedure is not called at all, then delete its code.
;
; If a known local procedure is called exactly once,
; then inline its code at the call site and delete the
; known local procedure.  Change the size of the code
; at the call site by adding the size of the inlined code.
;
; Divide the remaining known and escaping procedures into categories:
;     1.  makes no calls to known local procedures
;     2.  known procedures that call known procedures;
;         within this category, try to sort so that procedures do not
;         call procedures that come later in the sequence; or sort by
;         number of calls and/or size
;     3.  escaping procedures that call known procedures
;
; Approve each procedure in category 1 for inlining if its code size
; is less than some threshold.
;
; For each procedure in categories 2 and 3, traverse its code, inlining
; where it seems like a good idea.  The compiler should be more aggressive
; about inlining non-tail calls than tail calls because:
;
;     Inlining a non-tail call can eliminate a stack frame
;     or expose the inlined code to loop optimizations.
;
;     The main reason for inlining a tail call is to enable
;     intraprocedural optimizations or to unroll a loop.
;
; After inlining has been performed on a known local procedure,
; then approve it for inlining if its size is less than some threshold.
;
; FIXME:
; This strategy avoids infinite unrolling, but it also avoids finite
; unrolling of loops.

; Parameters to control inlining.
; These can be tuned later.

(define *tail-threshold* 10)
(define *nontail-threshold* 20)
(define *multiplier* 300)

; Given a callgraph, performs inlining of known local procedures
; by side effect.  The original expression must then be copied to
; reinstate Twobit's invariants.

; FIXME:  This code doesn't yet do the right thing with known local
; procedures that aren't called or are called in exactly one place.

(define (inline-using-callgraph! g)
  (let ((known (make-hashtable))
        (category2 '())
        (category3 '()))
    (for-each (lambda (node)
                (let ((name (callgraphnode.name node))
                      (tcalls (callgraphnode.tailcalls node))
                      (ncalls (callgraphnode.nontailcalls node)))
                  (if (symbol? name)
                      (hashtable-put! known name node))
                  (if (and (null? tcalls)
                           (null? ncalls))
                      (if (< (callgraphnode.size node)
                             *nontail-threshold*)
                          (callgraphnode.info! node #t))
                      (if (symbol? name)
                          (set! category2 (cons node category2))
                          (set! category3 (cons node category3))))))
              g)
    (set! category2 (twobit-sort (lambda (x y)
                                   (< (callgraphnode.size x)
                                      (callgraphnode.size y)))
                                 category2))
    (for-each (lambda (node)
                (inline-node! node known))
              category2)
    (for-each (lambda (node)
                (inline-node! node known))
              category3)
    ; FIXME:
    ; Inlining destroys the callgraph, so maybe this cleanup is useless.
    (hashtable-for-each (lambda (name node) (callgraphnode.info! node #f))
                        known)))

; Given a node of the callgraph and a hash table of nodes for
; known local procedures, performs inlining by side effect.

(define (inline-node! node known)
  (let* ((debugging? #f)
         (name (callgraphnode.name node))
         (exp (callgraphnode.code node))
         (size0 (callgraphnode.size node))
         (budget (quotient (* (- *multiplier* 100) size0) 100))
         (tail-threshold *tail-threshold*)
         (nontail-threshold *nontail-threshold*))
    
    ; Given an expression,
    ; a boolean indicating whether the expression is in a tail context,
    ; a list of procedures that should not be inlined,
    ; and a size budget,
    ; performs inlining by side effect and returns the unused budget.
    
    (define (inline exp tail? budget)
        (if (positive? budget)
            
            (case (car exp)
              
              ((quote lambda)
               budget)
              
              ((set!)
               (inline (assignment.rhs exp) #f budget))
              
              ((if)
               (let* ((budget (inline (if.test exp) #f budget))
                      (budget (inline (if.then exp) tail? budget))
                      (budget (inline (if.else exp) tail? budget)))
                 budget))
              
              ((begin)
               (if (variable? exp)
                   budget
                   (do ((exprs (begin.exprs exp) (cdr exprs))
                        (budget budget
                                (inline (car exprs) #f budget)))
                       ((null? (cdr exprs))
                        (inline (car exprs) tail? budget)))))
              
              (else
               (let ((budget (do ((exprs (call.args exp) (cdr exprs))
                                  (budget budget
                                          (inline (car exprs) #f budget)))
                                 ((null? exprs)
                                  budget))))
                 (let ((proc (call.proc exp)))
                   (cond ((variable? proc)
                          (let* ((procname (variable.name proc))
                                 (procnode (hashtable-get known procname)))
                            (if procnode
                                (let ((size (callgraphnode.size procnode))
                                      (info (callgraphnode.info procnode)))
                                  (if (and info
                                           (<= size budget)
                                           (<= size
                                               (if tail?
                                                   tail-threshold
                                                   nontail-threshold)))
                                      (begin
                                       (if debugging?
                                           (begin
                                            (display "    Inlining ")
                                            (write (variable.name proc))
                                            (newline)))
                                       (call.proc-set!
                                        exp
                                        (copy-exp
                                         (callgraphnode.code procnode)))
                                       (callgraphnode.size!
                                        node
                                        (+ (callgraphnode.size node) size))
                                       (- budget size))
                                      (begin
                                       (if (and #f debugging?)
                                           (begin
                                            (display "    Declining to inline ")
                                            (write (variable.name proc))
                                            (newline)))
                                       budget)))
                                budget)))
                         ((lambda? proc)
                          (inline (lambda.body proc) tail? budget))
                         (else
                          (inline proc #f budget)))))))
            -1))
    
    (if (and #f debugging?)
        (begin
         (display "Processing ")
         (write name)
         (newline)))
    
    (let ((budget (inline (if (lambda? exp)
                              (lambda.body exp)
                              exp)
                          #t
                          budget)))
      (if (and (negative? budget)
               debugging?)
          ; This shouldn't happen very often.
          (begin (display "Ran out of inlining budget for ")
                 (write (callgraphnode.name node))
                 (newline)))
      (if (<= (callgraphnode.size node) nontail-threshold)
          (callgraphnode.info! node #t))
      #f)))

; For testing.

(define (test-inlining test0)
  (begin (define exp0 (begin (display "Compiling...")
                             (newline)
                             (pass2 (pass1 test0))))
         (define g0 (begin (display "Computing call graph...")
                           (newline)
                           (callgraph exp0))))
  (display "Inlining...")
  (newline)
  (inline-using-callgraph! g0)
  (pretty-print (make-readable (copy-exp exp0))))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 14 April 1999.
;
; Interprocedural constant propagation and folding.
;
; Constant propagation must converge before constant folding can be
; performed.  Constant folding creates more constants that can be
; propagated, so these two optimizations must be iterated, but it
; is safe to stop at any time.
;
; Abstract interpretation for constant folding.
;
; The abstract values are
;     bottom    (represented here by #f)
;     constants (represented by quoted literals)
;     top       (represented here by #t)
;
; Let [[ E ]] be the abstract interpretation of E over that domain
; of abstract values, with respect to some arbitrary set of abstract
; values for local variables.
;
; If a is a global variable or a formal parameter of an escaping
; lambda expression, then [[ a ]] = #t.
;
; If x is the ith formal parameter of a known local procedure f,
; then [[ x ]] = \join_{(f E1 ... En)} [[ Ei ]].
;
; [[ K ]] = K
; [[ L ]] = #t
; [[ (begin E1 ... En) ]] = [[ En ]]
; [[ (set! I E) ]] = #f
;
; If [[ E0 ]] = #t, then [[ (if E0 E1 E2) ]] = [[ E1 ]] \join [[ E2 ]]
; else if [[ E0 ]] = K, then [[ (if E0 E1 E2) ]] = [[ E1 ]]
;                         or [[ (if E0 E1 E2) ]] = [[ E2 ]]
;                       depending upon K
; else [[ (if E0 E1 E2) ]] = #f
;
; If f is a known local procedure with body E,
;     then [[ (f E1 ... En) ]] = [[ E ]]
;
; If g is a foldable integrable procedure, then:
; if there is some i for which [[ Ei ]] = #t,
;     then [[ (g E1 ... En) ]] = #t
; else if [[ E1 ]] = K1, ..., [[ En ]] = Kn,
;     then [[ (g E1 ... En) ]] = (g K1 ... Kn)
; else [[ (g E1 ... En) ]] = #f
;
; Symbolic representations of abstract values.
; (Can be thought of as mappings from abstract environments to
; abstract values.)
;
; <symbolic>     ::=  #t  |  ( <expressions> )
; <expressions>  ::=  <empty>  |  <expression> <expressions>

; Parameter to limit constant propagation and folding.
; This parameter can be tuned later.

(define *constant-propagation-limit* 5)

; Given an expression as output by pass 2, performs constant
; propagation and folding.

(define (constant-propagation exp)
  (define (constant-propagation exp i)
    (if (< i *constant-propagation-limit*)
        (begin
         ;(display "Performing constant propagation and folding...")
         ;(newline)
         (let* ((g (callgraph exp))
                (L (callgraphnode.code (car g)))
                (variables (constant-propagation-using-callgraph g))
                (changed? (constant-folding! L variables)))
           (if changed?
               (constant-propagation (lambda.body L) (+ i 1))
               (lambda.body L))))))
  (constant-propagation exp 0))

; Given a callgraph, returns a hashtable of abstract values for
; all local variables.

(define (constant-propagation-using-callgraph g)
  (let ((debugging? #f)
        (folding? (integrate-usual-procedures))
        (known (make-hashtable))
        (variables (make-hashtable))
        (counter 0))
    
    ; Computes joins of abstract values.
    
    (define (join x y)
      (cond ((boolean? x)
             (if x #t y))
            ((boolean? y)
             (join y x))
            ((equal? x y)
             x)
            (else #t)))
    
    ; Given a <symbolic> and a vector of abstract values,
    ; evaluates the <symbolic> and returns its abstract value.
    
    (define (aeval rep env)
      (cond ((eq? rep #t)
             #t)
            ((null? rep)
             #f)
            ((null? (cdr rep))
             (aeval1 (car rep) env))
            (else
             (join (aeval1 (car rep) env)
                   (aeval (cdr rep) env)))))
    
    (define (aeval1 exp env)
      
      (case (car exp)
        
        ((quote)
         exp)
        
        ((lambda)
         #t)
        
        ((set!)
         #f)
        
        ((begin)
         (if (variable? exp)
             (let* ((name (variable.name exp))
                    (i (hashtable-get variables name)))
               (if i
                   (vector-ref env i)
                   #t))
             (aeval1-error)))
        
        ((if)
         (let* ((val0 (aeval1 (if.test exp) env))
                (val1 (aeval1 (if.then exp) env))
                (val2 (aeval1 (if.else exp) env)))
           (cond ((eq? val0 #t)
                  (join val1 val2))
                 ((pair? val0)
                  (if (constant.value val0)
                      val1
                      val2))
                 (else
                  #f))))
        
        (else
         (do ((exprs (reverse (call.args exp)) (cdr exprs))
              (vals '() (cons (aeval1 (car exprs) env) vals)))
             ((null? exprs)
              (let ((proc (call.proc exp)))
                (cond ((variable? proc)
                       (let* ((procname (variable.name proc))
                              (procnode (hashtable-get known procname))
                              (entry (if folding?
                                         (constant-folding-entry procname)
                                         #f)))
                         (cond (procnode
                                (vector-ref env
                                            (hashtable-get variables
                                                           procname)))
                               (entry
                                ; FIXME: No constant folding
                                #t)
                               (else (aeval1-error)))))
                      (else
                       (aeval1-error)))))))))
    
    (define (aeval1-error)
      (error "Compiler bug: constant propagation (aeval1)"))
    
    ; Combines two <symbolic>s.
    
    (define (combine-symbolic rep1 rep2)
      (cond ((eq? rep1 #t) #t)
            ((eq? rep2 #t) #t)
            (else
             (append rep1 rep2))))
    
    ; Given an expression, returns a <symbolic> that represents
    ; a list of expressions whose abstract values can be joined
    ; to obtain the abstract value of the given expression.
    ; As a side effect, enters local variables into variables.
    
    (define (collect! exp)
      
      (case (car exp)
        
        ((quote)
         (list exp))
        
        ((lambda)
         #t)
        
        ((set!)
         (collect! (assignment.rhs exp))
         '())
        
        ((begin)
         (if (variable? exp)
             (list exp)
             (do ((exprs (begin.exprs exp) (cdr exprs)))
                 ((null? (cdr exprs))
                  (collect! (car exprs)))
                 (collect! (car exprs)))))
        
        ((if)
         (collect! (if.test exp))
         (collect! (if.then exp))
         (collect! (if.else exp))
         #t)
        
        (else
         (do ((exprs (reverse (call.args exp)) (cdr exprs))
              (reps '() (cons (collect! (car exprs)) reps)))
             ((null? exprs)
              (let ((proc (call.proc exp)))
                (define (put-args! args reps)
                  (cond ((pair? args)
                         (let ((v (car args))
                               (rep (car reps)))
                           (hashtable-put! variables v rep)
                           (put-args! (cdr args) (cdr reps))))
                        ((symbol? args)
                         (hashtable-put! variables args #t))
                        (else #f)))
                (cond ((variable? proc)
                       (let* ((procname (variable.name proc))
                              (procnode (hashtable-get known procname))
                              (entry (if folding?
                                         (constant-folding-entry procname)
                                         #f)))
                         (cond (procnode
                                (for-each (lambda (v rep)
                                            (hashtable-put!
                                             variables
                                             v
                                             (combine-symbolic
                                              rep (hashtable-get variables v))))
                                          (lambda.args
                                            (callgraphnode.code procnode))
                                          reps)
                                (list (make-variable procname)))
                               (entry
                                ; FIXME: No constant folding
                                #t)
                               (else #t))))
                      ((lambda? proc)
                       (put-args! (lambda.args proc) reps)
                       (collect! (lambda.body proc)))
                      (else
                       (collect! proc)
                       #t))))))))
    
    (for-each (lambda (node)
                (let* ((name (callgraphnode.name node))
                       (code (callgraphnode.code node))
                       (known? (symbol? name))
                       (rep (if known? '() #t)))
                  (if known?
                      (hashtable-put! known name node))
                  (if (lambda? code)
                      (for-each (lambda (var)
                                  (hashtable-put! variables var rep))
                                (make-null-terminated (lambda.args code))))))
              g)
    
    (for-each (lambda (node)
                (let ((name (callgraphnode.name node))
                      (code (callgraphnode.code node)))
                  (cond ((symbol? name)
                         (hashtable-put! variables
                                         name
                                         (collect! (lambda.body code))))
                        (else
                         (collect! (lambda.body code))))))
              g)
    
    (if (and #f debugging?)
        (begin
         (hashtable-for-each (lambda (v rep)
                               (write v)
                               (display ": ")
                               (write rep)
                               (newline))
                             variables)
         
         (display "----------------------------------------")
         (newline)))
    
    ;(trace aeval aeval1)
    
    (let* ((n (hashtable-size variables))
           (vars (hashtable-map (lambda (v rep) v) variables))
           (reps (map (lambda (v) (hashtable-get variables v)) vars))
           (init (make-vector n #f))
           (next (make-vector n)))
      (do ((i 0 (+ i 1))
           (vars vars (cdr vars))
           (reps reps (cdr reps)))
          ((= i n))
          (hashtable-put! variables (car vars) i)
          (vector-set! next
                       i
                       (let ((rep (car reps)))
                         (lambda (env)
                           (aeval rep env)))))
      (compute-fixedpoint init next equal?)
      (for-each (lambda (v)
                  (let* ((i (hashtable-get variables v))
                         (aval (vector-ref init i)))
                    (hashtable-put! variables v aval)
                    (if (and debugging?
                             (not (eq? aval #t)))
                        (begin (write v)
                               (display ": ")
                               (write aval)
                               (newline)))))
                vars)
      variables)))

; Given a lambda expression, performs constant propagation, folding,
; and simplifications by side effect, using the abstract values in the
; hash table of variables.
; Returns #t if any new constants were created by constant folding,
; otherwise returns #f.

(define (constant-folding! L variables)
  (let ((debugging? #f)
        (msg1 "    Propagating constant value for ")
        (msg2 "    Folding: ")
        (msg3 " ==> ")
        (folding? (integrate-usual-procedures))
        (changed? #f))
    
    ; Given a known lambda expression L, its original formal parameters,
    ; and a list of all calls to L, deletes arguments that are now
    ; ignored because of constant propagation.
    
    (define (delete-ignored-args! L formals0 calls)
      (let ((formals1 (lambda.args L)))
        (for-each (lambda (call)
                    (do ((formals0 formals0 (cdr formals0))
                         (formals1 formals1 (cdr formals1))
                         (args (call.args call)
                               (cdr args))
                         (newargs '()
                                  (if (and (eq? (car formals1) name:IGNORED)
                                           (pair?
                                            (hashtable-get variables
                                                           (car formals0))))
                                      newargs
                                      (cons (car args) newargs))))
                        ((null? formals0)
                         (call.args-set! call (reverse newargs)))))
                  calls)
        (do ((formals0 formals0 (cdr formals0))
             (formals1 formals1 (cdr formals1))
             (formals2 '()
                       (if (and (not (eq? (car formals0)
                                          (car formals1)))
                                (eq? (car formals1) name:IGNORED)
                                (pair?
                                 (hashtable-get variables
                                                (car formals0))))
                           formals2
                           (cons (car formals1) formals2))))
            ((null? formals0)
             (lambda.args-set! L (reverse formals2))))))
    
    (define (fold! exp)
      
      (case (car exp)
        
        ((quote) exp)
        
        ((lambda)
         (let ((Rinfo (lambda.R exp))
               (known (map def.lhs (lambda.defs exp))))
           (for-each (lambda (entry)
                       (let* ((v (R-entry.name entry))
                              (aval (hashtable-fetch variables v #t)))
                         (if (and (pair? aval)
                                  (not (memq v known)))
                             (let ((x (constant.value aval)))
                               (if (or (boolean? x)
                                       (null? x)
                                       (symbol? x)
                                       (number? x)
                                       (char? x)
                                       (and (vector? x)
                                            (zero? (vector-length x))))
                                   (let ((refs (R-entry.references entry)))
                                     (for-each (lambda (ref)
                                                 (variable-set! ref aval))
                                               refs)
                                     ; Do not try to use Rinfo in place of
                                     ; (lambda.R exp) below!
                                     (lambda.R-set!
                                       exp
                                       (remq entry (lambda.R exp)))
                                     (flag-as-ignored v exp)
                                     (if debugging?
                                         (begin (display msg1)
                                                (write v)
                                                (display ": ")
                                                (write aval)
                                                (newline)))))))))
                     Rinfo)
           (for-each (lambda (def)
                       (let* ((name (def.lhs def))
                              (rhs (def.rhs def))
                              (entry (R-lookup Rinfo name))
                              (calls (R-entry.calls entry)))
                         (if (null? calls)
                             (begin (lambda.defs-set!
                                      exp
                                      (remq def (lambda.defs exp)))
                                    ; Do not try to use Rinfo in place of
                                    ; (lambda.R exp) below!
                                    (lambda.R-set!
                                      exp
                                      (remq entry (lambda.R exp))))
                             (let* ((formals0 (append (lambda.args rhs) '()))
                                    (L (fold! rhs))
                                    (formals1 (lambda.args L)))
                               (if (not (equal? formals0 formals1))
                                   (delete-ignored-args! L formals0 calls))))))
                     (lambda.defs exp))
           (lambda.body-set!
             exp
             (fold! (lambda.body exp)))
           exp))
        
        ((set!)
         (assignment.rhs-set! exp (fold! (assignment.rhs exp)))
         exp)
        
        ((begin)
         (if (variable? exp)
             exp
             (post-simplify-begin (make-begin (map fold! (begin.exprs exp)))
                                  (make-notepad #f))))
        
        ((if)
         (let ((exp0 (fold! (if.test exp)))
               (exp1 (fold! (if.then exp)))
               (exp2 (fold! (if.else exp))))
           (if (constant? exp0)
               (let ((newexp (if (constant.value exp0)
                                 exp1
                                 exp2)))
                 (if debugging?
                     (begin (display msg2)
                            (write (make-readable exp))
                            (display msg3)
                            (write (make-readable newexp))
                            (newline)))
                 (set! changed? #t)
                 newexp)
               (make-conditional exp0 exp1 exp2))))
        
        (else
         (let ((args (map fold! (call.args exp)))
               (proc (fold! (call.proc exp))))
           (cond ((and folding?
                       (variable? proc)
                       (every? constant? args)
                       (let ((entry
                              (constant-folding-entry (variable.name proc))))
                         (and entry
                              (let ((preds
                                     (constant-folding-predicates entry)))
                                (and (= (length args) (length preds))
                                     (every?
                                      (lambda (x) x)
                                      (map (lambda (f v) (f v))
                                           (constant-folding-predicates entry)
                                           (map constant.value args))))))))
                  (set! changed? #t)
                  (let ((result
                         (make-constant
                          (apply (constant-folding-folder
                                  (constant-folding-entry
                                   (variable.name proc)))
                                 (map constant.value args)))))
                    (if debugging?
                        (begin (display msg2)
                               (write (make-readable (make-call proc args)))
                               (display msg3)
                               (write result)
                               (newline)))
                    result))
                 ((and (lambda? proc)
                       (list? (lambda.args proc)))
                  ; FIXME: Folding should be done even if there is
                  ; a rest argument.
                  (let loop ((formals (reverse (lambda.args proc)))
                             (actuals (reverse args))
                             (processed-formals '())
                             (processed-actuals '())
                             (for-effect '()))
                    (cond ((null? formals)
                           (lambda.args-set! proc processed-formals)
                           (call.args-set! exp processed-actuals)
                           (let ((call (if (and (null? processed-formals)
                                                (null? (lambda.defs proc)))
                                           (lambda.body proc)
                                           exp)))
                             (if (null? for-effect)
                                 call
                                 (post-simplify-begin
                                  (make-begin
                                   (reverse (cons call for-effect)))
                                  (make-notepad #f)))))
                          ((ignored? (car formals))
                           (loop (cdr formals)
                                 (cdr actuals)
                                 processed-formals
                                 processed-actuals
                                 (cons (car actuals) for-effect)))
                          (else
                           (loop (cdr formals)
                                 (cdr actuals)
                                 (cons (car formals) processed-formals)
                                 (cons (car actuals) processed-actuals)
                                 for-effect)))))
                 (else
                  (call.proc-set! exp proc)
                  (call.args-set! exp args)
                  exp))))))
    
    (fold! L)
    changed?))
; Copyright 1998 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 7 June 1999.
;
; Conversion to A-normal form, with heuristics for
; choosing a good order of evaluation.
;
; This pass operates as a source-to-source transformation on
; expressions written in the subset of Scheme described by the
; following grammar, where the input and output expressions
; satisfy certain additional invariants described below.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F G <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the input only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  For each lambda expression, the associated G is a subset of F
;      that contains every identifier that occurs free within some
;      inner lambda expression that escapes, and possibly a few that
;      don't.  (Assignment-elimination does not calculate G exactly.)
;   *  Variables named IGNORED are neither referenced nor assigned.
;
; Invariants that hold for the output only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  R, F, and G are garbage.
;   *  There are no sequential expressions.
;   *  The output is an expression E with syntax
;
; E  -->  A
;      |  (L)
;      |  (L A)
;
; A  -->  W
;      |  L
;      |  (W_0 W_1 ...)
;      |  (set! I W)
;      |  (if W E1 E2)
;
; W  -->  (quote K)
;      |  (begin I)
;
; In other words:
; An expression is a LET* such that the rhs of every binding is
;     a conditional with the test already evaluated, or
;     an expression that can be evaluated in one step
;         (treating function calls as a single step)
;
; A-normal form corresponds to the control flow graph for a lambda
; expression.

; Algorithm: repeated use of these rules:
;
; (E0 E1 ...)                              ((lambda (T0 T1 ...) (T0 T1 ...))
;                                           E0 E1 ...)
; (set! I E)                               ((lambda (T) (set! I T)) E)
; (if E0 E1 E2)                            ((lambda (T) (if T E1 E2)) E0)
; (begin E0 E1 E2 ...)                     ((lambda (T) (begin E1 E2 ...)) E0)
;
; ((lambda (I1 I2 I3 ...) E)               ((lambda (I1)
;  E1 E2 E3)                                  ((lambda (I2 I3 ...) E)
;                                              E2 E3))
;                                           E1)
;
; ((lambda (I2) E)                         ((lambda (I1)
;  ((lambda (I1) E2)                          ((lambda (I2) E)
;   E1))                                       E2)
;                                           E1)
;
; In other words:
; Introduce a temporary name for every expression except:
;     tail expressions
;     the alternatives of a non-tail conditional
; Convert every LET into a LET*.
; Get rid of LET* on the right hand side of a binding.

; Given an expression E in the representation output by pass 2,
; returns an A-normal form for E in that representation.
; Except for quoted values, the A-normal form does not share
; mutable structure with the original expression E.
;
; KNOWN BUG:
;
; If you call A-normal on a form that has already been converted
; to A-normal form, then the same temporaries will be generated
; twice.  An optional argument lets you specify a different prefix
; for temporaries the second time around.  Example:
;
; (A-normal-form (A-normal-form E ".T")
;                ".U")

; This is the declaration that is used to indicate A-normal form.

(define A-normal-form-declaration (list 'anf))

(define (A-normal-form E . rest)
  
  (define (A-normal-form E)
    (anf-make-let* (anf E '() '())))
  
  ; New temporaries.
  
  (define temp-counter 0)
  
  (define temp-prefix
    (if (or (null? rest)
            (not (string? (car rest))))
        (string-append renaming-prefix "T")
        (car rest)))
  
  (define (newtemp)
    (set! temp-counter (+ temp-counter 1))
    (string->symbol
     (string-append temp-prefix
                    (number->string temp-counter))))
  
  ; Given an expression E as output by pass 2,
  ; a list of surrounding LET* bindings,
  ; and an ordered list of likely register variables,
  ; return a non-empty list of LET* bindings
  ; whose first binding associates a dummy variable
  ; with an A-expression giving the value for E.
  
  (define (anf E bindings regvars)
    (case (car E)
      ((quote)    (anf-bind-dummy E bindings))
      ((begin)    (if (variable? E)
                      (anf-bind-dummy E bindings)
                      (anf-sequential E bindings regvars)))
      ((lambda)   (anf-lambda E bindings regvars))
      ((set!)     (anf-assignment E bindings regvars))
      ((if)       (anf-conditional E bindings regvars))
      (else       (anf-call E bindings regvars))))
  
  (define anf:dummy (string->symbol "RESULT"))
  
  (define (anf-bind-dummy E bindings)
    (cons (list anf:dummy E)
          bindings))
  
  ; Unlike anf-bind-dummy, anf-bind-name and anf-bind convert
  ; their expression argument to A-normal form.
  ; Don't change anf-bind to call anf-bind-name, because that
  ; would name the temporaries in an aesthetically bad order.
  
  (define (anf-bind-name name E bindings regvars)
    (let ((bindings (anf E bindings regvars)))
      (cons (list name (cadr (car bindings)))
            (cdr bindings))))
  
  (define (anf-bind E bindings regvars)
    (let ((bindings (anf E bindings regvars)))
      (cons (list (newtemp) (cadr (car bindings)))
            (cdr bindings))))
  
  (define (anf-result bindings)
    (make-variable (car (car bindings))))
  
  (define (anf-make-let* bindings)
    (define (loop bindings body)
      (if (null? bindings)
          body
          (let ((T1 (car (car bindings)))
                (E1 (cadr (car bindings))))
            (loop (cdr bindings)
                  (make-call (make-lambda (list T1)
                                          '()
                                          '()
                                          '()
                                          '()
                                          (list A-normal-form-declaration)
                                          '()
                                          body)
                             (list E1))))))
    (loop (cdr bindings)
          (cadr (car bindings))))                                  
  
  (define (anf-sequential E bindings regvars)
    (do ((bindings bindings
                   (anf-bind (car exprs) bindings regvars))
         (exprs (begin.exprs E)
                (cdr exprs)))
        ((null? (cdr exprs))
         (anf (car exprs) bindings regvars))))
  
  ; Heuristic: the formal parameters of an escaping lambda or
  ; known local procedure are kept in REG1, REG2, et cetera.
  
  (define (anf-lambda L bindings regvars)
    (anf-bind-dummy
     (make-lambda (lambda.args L)
                  (map (lambda (def)
                         (make-definition
                          (def.lhs def)
                          (A-normal-form (def.rhs def))))
                       (lambda.defs L))
                  '()
                  '()
                  '()
                  (cons A-normal-form-declaration
                        (lambda.decls L))
                  (lambda.doc L)
                  (anf-make-let*
                   (anf (lambda.body L)
                        '()
                        (make-null-terminated (lambda.args L)))))
     bindings))
  
  (define (anf-assignment E bindings regvars)
    (let ((I (assignment.lhs E))
          (E1 (assignment.rhs E)))
      (if (variable? E1)
          (anf-bind-dummy E bindings)
          (let* ((bindings (anf-bind E1 bindings regvars))
                 (T1 (anf-result bindings)))
            (anf-bind-dummy (make-assignment I T1) bindings)))))
  
  (define (anf-conditional E bindings regvars)
    (let ((E0 (if.test E))
          (E1 (if.then E))
          (E2 (if.else E)))
      (if (variable? E0)
          (let ((E1 (anf-make-let* (anf E1 '() regvars)))
                (E2 (anf-make-let* (anf E2 '() regvars))))
            (anf-bind-dummy
             (make-conditional E0 E1 E2)
             bindings))
          (let* ((bindings (anf-bind E0 bindings regvars))
                 (E1 (anf-make-let* (anf E1 '() regvars)))
                 (E2 (anf-make-let* (anf E2 '() regvars))))
            (anf-bind-dummy
             (make-conditional (anf-result bindings) E1 E2)
             bindings)))))
  
  (define (anf-call E bindings regvars)
    (let* ((proc (call.proc E))
           (args (call.args E)))
      
      ; Evaluates the exprs and returns both a list of bindings and
      ; a list of the temporaries that name the results of the exprs.
      ; If rename-always? is true, then temporaries are generated even
      ; for constants and temporaries.
      
      (define (loop exprs bindings names rename-always?)
        (if (null? exprs)
            (values bindings (reverse names))
            (let ((E (car exprs)))
              (if (or rename-always?
                      (not (or (constant? E)
                               (variable? E))))
                  (let* ((bindings
                          (anf-bind (car exprs) bindings regvars)))
                    (loop (cdr exprs)
                          bindings
                          (cons (anf-result bindings) names)
                          rename-always?))
                  (loop (cdr exprs)
                        bindings
                        (cons E names)
                        rename-always?)))))
      
      ; Evaluates the exprs, binding them to the vars, and returns
      ; a list of bindings.
      ;
      ; Although LET variables are likely to be kept in registers,
      ; trying to guess which register will be allocated is likely
      ; to do more harm than good.
      
      (define (let-loop exprs bindings regvars vars)
        (if (null? exprs)
            (if (null? (lambda.defs proc))
                (anf (lambda.body proc)
                     bindings
                     regvars)
                (let ((bindings
                       (anf-bind
                        (make-lambda '()
                                     (lambda.defs proc)
                                     '()
                                     '()
                                     '()
                                     (cons A-normal-form-declaration
                                           (lambda.decls proc))
                                     (lambda.doc proc)
                                     (lambda.body proc))
                        bindings
                        '())))
                  (anf-bind-dummy
                   (make-call (anf-result bindings) '())
                   bindings)))
            (let-loop (cdr exprs)
              (anf-bind-name (car vars)
                             (car exprs)
                             bindings
                             regvars)
              regvars
              (cdr vars))))
      
      (cond ((lambda? proc)
             (let ((formals (lambda.args proc)))
               (if (list? formals)
                   (let* ((pi (anf-order-of-evaluation args regvars #f))
                          (exprs (permute args pi))
                          (names (permute (lambda.args proc) pi)))
                     (let-loop (reverse exprs) bindings regvars (reverse names)))
                   (anf-call (normalize-let E) bindings regvars))))
            
            ((not (variable? proc))
             (let ((pi (anf-order-of-evaluation args regvars #f)))
               (call-with-values
                (lambda () (loop (permute args pi) bindings '() #t))
                (lambda (bindings names)
                  (let ((bindings (anf-bind proc bindings regvars)))
                    (anf-bind-dummy
                     (make-call (anf-result bindings)
                                (unpermute names pi))
                     bindings))))))
            
            ((and (integrate-usual-procedures)
                  (prim-entry (variable.name proc)))
             (let ((pi (anf-order-of-evaluation args regvars #t)))
               (call-with-values
                (lambda () (loop (permute args pi) bindings '() #t))
                (lambda (bindings names)
                  (anf-bind-dummy
                   (make-call proc (unpermute names pi))
                   bindings)))))
            
            ((memq (variable.name proc) regvars)
             (let* ((exprs (cons proc args))
                    (pi (anf-order-of-evaluation
                         exprs
                         (cons name:IGNORED regvars)
                         #f)))
               (call-with-values
                (lambda () (loop (permute exprs pi) bindings '() #t))
                (lambda (bindings names)
                  (let ((names (unpermute names pi)))
                    (anf-bind-dummy
                     (make-call (car names) (cdr names))
                     bindings))))))
            
            (else
             (let ((pi (anf-order-of-evaluation args regvars #f)))
               (call-with-values
                (lambda () (loop (permute args pi) bindings '() #t))
                (lambda (bindings names)
                  (anf-bind-dummy
                   (make-call proc (unpermute names pi))
                   bindings))))))))
  
  ; Given a list of expressions, a list of likely register contents,
  ; and a switch telling whether these are arguments for a primop
  ; or something else (such as the arguments for a real call),
  ; try to choose a good order in which to evaluate the expressions.
  ;
  ; Heuristic:  If none of the expressions is a call to a non-primop,
  ; then parallel assignment optimization gives a good order if the
  ; regvars are right, and should do no worse than a random order if
  ; the regvars are wrong.
  ;
  ; Heuristic:  If the expressions are arguments to a primop, and
  ; none are a call to a non-primop, then the register contents
  ; are irrelevant, and the first argument should be evaluated last.
  ;
  ; Heuristic:  If one or more of the expressions is a call to a
  ; non-primop, then the following should be a good order:
  ;
  ;     expressions that are neither a constant, variable, or a call
  ;     calls to non-primops
  ;     constants and variables
  
  (define (anf-order-of-evaluation exprs regvars for-primop?)
    (define (ordering targets exprs alist)
      (let ((para
             (parallel-assignment targets alist exprs)))
        (or para
            ; Evaluate left to right until a parallel assignment is found.
            (cons (car targets)
                  (ordering (cdr targets)
                            (cdr exprs)
                            alist)))))
    (if (parallel-assignment-optimization)
        (cond ((null? exprs) '())
              ((null? (cdr exprs)) '(0))
              (else
               (let* ((contains-call? #f)
                      (vexprs (list->vector exprs))
                      (vindexes (list->vector
                                 (iota (vector-length vexprs))))
                      (contains-call? #f)
                      (categories
                       (list->vector
                        (map (lambda (E)
                               (cond ((constant? E)
                                      2)
                                     ((variable? E)
                                      2)
                                     ((complicated? E)
                                      (set! contains-call? #t)
                                      1)
                                     (else
                                      0)))
                             exprs))))
                 (cond (contains-call?
                        (twobit-sort (lambda (i j)
                                       (< (vector-ref categories i)
                                          (vector-ref categories j)))
                                     (iota (length exprs))))
                       (for-primop?
                        (reverse (iota (length exprs))))
                       (else
                        (let ((targets (iota (length exprs))))
                          (define (pairup regvars targets)
                            (if (or (null? targets)
                                    (null? regvars))
                                '()
                                (cons (cons (car regvars)
                                            (car targets))
                                      (pairup (cdr regvars)
                                              (cdr targets)))))
                          (ordering targets
                                    exprs
                                    (pairup regvars targets))))))))
        (iota (length exprs))))
  
  (define (permute things pi)
    (let ((v (list->vector things)))
      (map (lambda (i) (vector-ref v i))
           pi)))
  
  (define (unpermute things pi)
    (let* ((v0 (list->vector things))
           (v1 (make-vector (vector-length v0))))
      (do ((pi pi (cdr pi))
           (k 0 (+ k 1)))
          ((null? pi)
           (vector->list v1))
          (vector-set! v1 (car pi) (vector-ref v0 k)))))
  
  ; Given a call whose procedure is a lambda expression that has
  ; a rest argument, return a genuine let expression.
  
  (define (normalize-let-error exp)
    (if (issue-warnings)
        (begin (display "WARNING from compiler: ")
               (display "Wrong number of arguments ")
               (display "to lambda expression")
               (newline)
               (pretty-print (make-readable exp) #t)
               (newline))))
  
  (define (normalize-let exp)
    (let* ((L (call.proc exp)))
      (let loop ((formals (lambda.args L))
                 (args (call.args exp))
                 (newformals '())
                 (newargs '()))
        (cond ((null? formals)
               (if (null? args)
                   (begin (lambda.args-set! L (reverse newformals))
                          (call.args-set! exp (reverse newargs)))
                   (begin (normalize-let-error exp)
                          (loop (list (newtemp))
                                args
                                newformals
                                newargs))))
              ((pair? formals)
               (if (pair? args)
                   (loop (cdr formals)
                         (cdr args)
                         (cons (car formals) newformals)
                         (cons (car args) newargs))
                   (begin (normalize-let-error exp)
                          (loop formals
                                (cons (make-constant 0)
                                      args)
                                newformals
                                newargs))))
              (else
               (loop (list formals)
                     (list (make-call-to-list args))
                     newformals
                     newargs))))))
  
  ; For heuristic use only.
  ; An expression is complicated unless it can probably be evaluated
  ; without saving and restoring any registers, even if it occurs in
  ; a non-tail position.
  
  (define (complicated? exp)
    ; Let's not spend all day on this.
    (let ((budget 10))
      (define (complicated? exp)
        (set! budget (- budget 1))
        (if (zero? budget)
            #t
            (case (car exp)
              ((quote)    #f)
              ((lambda)   #f)
              ((set!)     (complicated? (assignment.rhs exp)))
              ((if)       (or (complicated? (if.test exp))
                              (complicated? (if.then exp))
                              (complicated? (if.else exp))))
              ((begin)    (if (variable? exp)
                              #f
                              (some? complicated?
                                     (begin.exprs exp))))
              (else       (let ((proc (call.proc exp)))
                            (if (and (variable? proc)
                                     (integrate-usual-procedures)
                                     (prim-entry (variable.name proc)))
                                (some? complicated?
                                       (call.args exp))
                                #t))))))
      (complicated? exp)))
  
  (A-normal-form E))
(define (post-simplify-anf L0 T1 E0 E1 free regbindings L2)
  
  (define (return-normally)
    (values (make-call L0 (list E1))
            free
            regbindings))
  
  (return-normally))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 7 June 1999.
;
; Intraprocedural common subexpression elimination, constant propagation,
; copy propagation, dead code elimination, and register targeting.
;
; (intraprocedural-commoning E 'commoning)
;
;     Given an A-normal form E (alpha-converted, with correct free
;     variables and referencing information), returns an optimized
;     A-normal form with correct free variables but incorrect referencing
;     information.
;
; (intraprocedural-commoning E 'target-registers)
;
;     Given an A-normal form E (alpha-converted, with correct free
;     variables and referencing information), returns an A-normal form
;     with correct free variables but incorrect referencing information,
;     and in which MacScheme machine register names are used as temporary
;     variables.  The result is alpha-converted except for register names.
;
; (intraprocedural-commoning E 'commoning 'target-registers)
; (intraprocedural-commoning E)
;
;     Given an A-normal form as described above, returns an optimized
;     form in which register names are used as temporary variables.

; Semantics of .check!:
;
; (.check! b exn x ...) faults with code exn and arguments x ...
; if b is #f.

; The list of argument registers.
; This can't go in pass3commoning.aux.sch because that file must be
; loaded before the target-specific file that defines *nregs*.

(define argument-registers
  (do ((n (- *nregs* 2) (- n 1))
       (regs '()
             (cons (string->symbol
                    (string-append ".REG" (number->string n)))
                   regs)))
      ((zero? n)
       regs)))

(define (intraprocedural-commoning E . flags)
  
  (define target-registers? (or (null? flags) (memq 'target-registers flags)))
  (define commoning? (or (null? flags) (memq 'commoning flags)))
  
  (define debugging? #f)
  
  (call-with-current-continuation
   (lambda (return)
     
     (define (error . stuff)
       (display "Bug detected during intraprocedural optimization")
       (newline)
       (for-each (lambda (s)
                   (display s) (newline))
                 stuff)
       (return (make-constant #f)))
     
     ; Given an expression, an environment, the available expressions,
     ; and an ordered list of likely register variables (used heuristically),
     ; returns the transformed expression and its set of free variables.
     
     (define (scan-body E env available regvars)
       
       ; The local variables are those that are bound by a LET within
       ; this procedure.  The formals of a lambda expression and the
       ; known local procedures are counted as non-global, not local,
       ; because there is no let-binding for a formal that can be
       ; renamed during register targeting.
       ; For each local variable, we keep track of how many times it
       ; is referenced.  This information is not accurate until we
       ; are backing out of the recursion, and does not have to be.
       
       (define local-variables (make-hashtable symbol-hash assq))
       
       (define (local-variable? sym)
         (hashtable-get local-variables sym))
       
       (define (local-variable-not-used? sym)
         (= 0 (hashtable-fetch local-variables sym -1)))
       
       (define (local-variable-used-once? sym)
         (= 1 (hashtable-fetch local-variables sym 0)))
       
       (define (record-local-variable! sym)
         (hashtable-put! local-variables sym 0))
       
       (define (used-local-variable! sym)
         (adjust-local-variable! sym 1))
       
       (define (adjust-local-variable! sym n)
         (let ((m (hashtable-get local-variables sym)))
           (if debugging?
               (if (and m (> m 0))
                   (begin (write (list sym (+ m n)))
                          (newline))))
           (if m
               (hashtable-put! local-variables
                               sym
                               (+ m n)))))
       
       (define (closed-over-local-variable! sym)
         ; Set its reference count to infinity so it won't be optimized away.
         ; FIXME:  One million isn't infinity.
         (hashtable-put! local-variables sym 1000000))
       
       (define (used-variable! sym)
         (used-local-variable! sym))
       
       (define (abandon-expression! E)
         (cond ((variable? E)
                (adjust-local-variable! (variable.name E) -1))
               ((conditional? E)
                (abandon-expression! (if.test E))
                (abandon-expression! (if.then E))
                (abandon-expression! (if.else E)))
               ((call? E)
                (for-each (lambda (exp)
                            (if (variable? exp)
                                (let ((name (variable.name exp)))
                                  (if (local-variable? name)
                                      (adjust-local-variable! name -1)))))
                          (cons (call.proc E)
                                (call.args E))))))
       
       ; Environments are represented as hashtrees.
       
       (define (make-empty-environment)
         (make-hashtree symbol-hash assq))
       
       (define (environment-extend env sym)
         (hashtree-put env sym #t))
       
       (define (environment-extend* env symbols)
         (if (null? symbols)
             env
             (environment-extend* (hashtree-put env (car symbols) #t)
                                  (cdr symbols))))
       
       (define (environment-lookup env sym)
         (hashtree-get env sym))
       
       (define (global? x)
         (cond ((local-variable? x)
                #f)
               ((environment-lookup env x)
                #f)
               (else
                #t)))
       
       ;
       
       (define (available-add! available T E)
         (cond ((constant? E)
                (available-extend! available T E available:killer:immortal))
               ((variable? E)
                (available-extend! available
                                   T
                                   E
                                   (if (global? (variable.name E))
                                       available:killer:globals
                                       available:killer:immortal)))
               (else
                (let ((entry (prim-call E)))
                  (if entry
                      (let ((killer (prim-lives-until entry)))
                        (if (not (eq? killer available:killer:dead))
                            (do ((args (call.args E) (cdr args))
                                 (k killer
                                    (let ((arg (car args)))
                                      (if (and (variable? arg)
                                               (global? (variable.name arg)))
                                          available:killer:globals
                                          k))))
                                ((null? args)
                                 (available-extend!
                                  available
                                  T
                                  E
                                  (logior killer k)))))))))))
       
       ; Given an expression E,
       ; an environment containing all variables that are in scope,
       ; and a table of available expressions,
       ; returns multiple values:
       ;   the transformed E
       ;   the free variables of E
       ;   the register bindings to be inserted; each binding has the form
       ;     (R x (begin R)), where (begin R) is a reference to R.
       ; 
       ; Side effects E.
       
       (define (scan E env available)
         (if (not (call? E))
             (scan-rhs E env available)
             (let ((proc (call.proc E)))
               (if (not (lambda? proc))
                   (scan-rhs E env available)
                   (let ((vars (lambda.args proc)))
                     (cond ((null? vars)
                            (scan-let0 E env available))
                           ((null? (cdr vars))
                            (scan-binding E env available))
                           (else
                            (error (make-readable E)))))))))
       
       ; E has the form of (let ((T1 E1)) E0).
       
       (define (scan-binding E env available)
         (let* ((L (call.proc E))
                (T1 (car (lambda.args L)))
                (E1 (car (call.args E)))
                (E0 (lambda.body L)))
           (record-local-variable! T1)
           (call-with-values
            (lambda () (scan-rhs E1 env available))
            (lambda (E1 F1 regbindings1)
              (available-add! available T1 E1)
              (let* ((env (let ((formals
                                 (make-null-terminated (lambda.args L))))
                            (environment-extend*
                             (environment-extend* env formals)
                             (map def.lhs (lambda.defs L)))))
                     (Fdefs (scan-defs L env available)))
                (call-with-values
                 (lambda () (scan E0 env available))
                 (lambda (E0 F0 regbindings0)
                   (lambda.body-set! L E0)
                   (if target-registers?
                       (scan-binding-phase2
                        L T1 E0 E1 F0 F1 Fdefs regbindings0 regbindings1)
                       (scan-binding-phase3
                        L E0 E1 (union F0 Fdefs)
                                F1 regbindings0 regbindings1)))))))))
       
       ; Given the lambda expression for a let expression that binds
       ; a single variable T1, the transformed body E0 and right hand side E1,
       ; their sets of free variables F0 and F1, the set of free variables
       ; for the internal definitions of L, and the sets of register
       ; bindings that need to be wrapped around E0 and E1, returns the
       ; transformed let expression, its free variables, and register
       ; bindings.
       ;
       ; This phase is concerned exclusively with register bindings,
       ; and is bypassed unless the target-registers flag is specified.
       
       (define (scan-binding-phase2
                L T1 E0 E1 F0 F1 Fdefs regbindings0 regbindings1)
         
         ; T1 can't be a register because we haven't
         ; yet inserted register bindings that high up.
         
         ; Classify the register bindings that need to wrapped around E0:
         ;     1.  those that have T1 as their rhs
         ;     2.  those whose lhs is a register that is likely to hold
         ;         a variable that occurs free in E1
         ;     3.  all others
         
         (define (phase2a)
           (do ((rvars regvars (cdr rvars))
                (regs argument-registers (cdr regs))
                (regs1 '() (if (memq (car rvars) F1)
                               (cons (car regs) regs1)
                               regs1)))
               ((or (null? rvars)
                    (null? regs))
                ; regs1 is the set of registers that are live for E1
                
                (let loop ((regbindings regbindings0)
                           (rb1 '())
                           (rb2 '())
                           (rb3 '()))
                  (if (null? regbindings)
                      (phase2b rb1 rb2 rb3)
                      (let* ((binding (car regbindings))
                             (regbindings (cdr regbindings))
                             (lhs (regbinding.lhs binding))
                             (rhs (regbinding.rhs binding)))
                        (cond ((eq? rhs T1)
                               (loop regbindings
                                     (cons binding rb1)
                                     rb2
                                     rb3))
                              ((memq lhs regs1)
                               (loop regbindings
                                     rb1
                                     (cons binding rb2)
                                     rb3))
                              (else
                               (loop regbindings
                                     rb1
                                     rb2
                                     (cons binding rb3))))))))))
         
         ; Determine which categories of register bindings should be
         ; wrapped around E0.
         ; Always wrap the register bindings in category 2.
         ; If E1 is a conditional or a real call, then wrap category 3.
         ; If T1 might be used more than once, then wrap category 1.
         
         (define (phase2b rb1 rb2 rb3)
           (if (or (conditional? E1)
                   (real-call? E1))
               (phase2c (append rb2 rb3) rb1 '())
               (phase2c rb2 rb1 rb3)))
         
         (define (phase2c towrap rb1 regbindings0)
           (cond ((and (not (null? rb1))
                       (local-variable-used-once? T1))
                  (phase2d towrap rb1 regbindings0))
                 (else
                  (phase2e (append rb1 towrap) regbindings0))))
         
         ; T1 is used only once, and there is a register binding (R T1).
         ; Change T1 to R.
         
         (define (phase2d towrap regbindings-T1 regbindings0)
           (if (not (null? (cdr regbindings-T1)))
               (error "incorrect number of uses" T1))
           (let* ((regbinding (car regbindings-T1))
                  (R (regbinding.lhs regbinding)))
             (lambda.args-set! L (list R))
             (phase2e towrap regbindings0)))
         
         ; Wrap the selected register bindings around E0.
         
         (define (phase2e towrap regbindings0)
           (call-with-values
            (lambda ()
              (wrap-with-register-bindings towrap E0 F0))
            (lambda (E0 F0)
              (let ((F (union Fdefs F0)))
                (scan-binding-phase3
                 L E0 E1 F F1 regbindings0 regbindings1)))))
         
         (phase2a))
       
       ; This phase, with arguments as above, constructs the result.
       
       (define (scan-binding-phase3 L E0 E1 F F1 regbindings0 regbindings1)
         (let* ((args (lambda.args L))
                (T1 (car args))
                (free (union F1 (difference F args)))
                (simple-let? (simple-lambda? L))
                (regbindings 
                 
                 ; At least one of regbindings0 and regbindings1
                 ; is the empty list.
                 
                 (cond ((null? regbindings0)
                        regbindings1)
                       ((null? regbindings1)
                        regbindings0)
                       (else
                        (error 'scan-binding 'regbindings)))))
           (lambda.body-set! L E0)
           (lambda.F-set! L F)
           (lambda.G-set! L F)
           (cond ((and simple-let?
                       (not (memq T1 F))
                       (no-side-effects? E1))
                  (abandon-expression! E1)
                  (values E0 F regbindings0))
                 ((and target-registers?
                       simple-let?
                       (local-variable-used-once? T1))
                  (post-simplify-anf L T1 E0 E1 free regbindings #f))
                 (else
                  (values (make-call L (list E1))
                          free
                          regbindings)))))
       
       (define (scan-let0 E env available)
         (let ((L (call.proc E)))
           (if (simple-lambda? L)
               (scan (lambda.body L) env available)
               (let ((T1 (make-variable name:IGNORED)))
                 (lambda.args-set! L (list T1))
                 (call-with-values
                  (lambda () (scan (make-call L (list (make-constant 0)))
                                   env
                                   available))
                  (lambda (E F regbindings)
                    (lambda.args-set! L '())
                    (values (make-call L '())
                            F
                            regbindings)))))))
       
       ; Optimizes the internal definitions of L and returns their
       ; free variables.
       
       (define (scan-defs L env available)
         (let loop ((defs (lambda.defs L))
                    (newdefs '())
                    (Fdefs '()))
           (if (null? defs)
               (begin (lambda.defs-set! L (reverse newdefs))
                      Fdefs)
               (let ((def (car defs)))
                 (call-with-values
                  (lambda ()
                    (let* ((Ldef (def.rhs def))
                           (Lformals (make-null-terminated (lambda.args Ldef)))
                           (Lenv (environment-extend*
                                  (environment-extend* env Lformals)
                                  (map def.lhs (lambda.defs Ldef)))))
                      (scan Ldef Lenv available)))
                  (lambda (rhs Frhs empty)
                    (if (not (null? empty))
                        (error 'scan-binding 'def))
                    (loop (cdr defs)
                          (cons (make-definition (def.lhs def) rhs)
                                newdefs)
                          (union Frhs Fdefs))))))))
       
       ; Given the right-hand side of a let-binding, an environment,
       ; and a table of available expressions, returns the transformed
       ; expression, its free variables, and the register bindings that
       ; need to be wrapped around it.
       
       (define (scan-rhs E env available)
         
         (cond
          ((constant? E)
           (values E (empty-set) '()))
          
          ((variable? E)
           (let* ((name (variable.name E))
                  (Enew (and commoning?
                             (if (global? name)
                                 (let ((T (available-expression
                                           available E)))
                                   (if T
                                       (make-variable T)
                                       #f))
                                 (available-variable available name)))))
             (if Enew
                 (scan-rhs Enew env available)
                 (begin (used-variable! name)
                        (values E (list name) '())))))
          
          ((lambda? E)
           (let* ((formals (make-null-terminated (lambda.args E)))
                  (env (environment-extend*
                        (environment-extend* env formals)
                        (map def.lhs (lambda.defs E))))
                  (Fdefs (scan-defs E env available)))
             (call-with-values
              (lambda ()
                (let ((available (copy-available-table available)))
                  (available-kill! available available:killer:all)
                  (scan-body (lambda.body E)
                             env
                             available
                             formals)))
              (lambda (E0 F0 regbindings0)
                (call-with-values
                 (lambda ()
                   (wrap-with-register-bindings regbindings0 E0 F0))
                 (lambda (E0 F0)
                   (lambda.body-set! E E0)
                   (let ((F (union Fdefs F0)))
                     (for-each (lambda (x)
                                 (closed-over-local-variable! x))
                               F)
                     (lambda.F-set! E F)
                     (lambda.G-set! E F)
                     (values E
                             (difference F
                                         (make-null-terminated
                                          (lambda.args E)))
                             '()))))))))
          
          ((conditional? E)
           (let ((E0 (if.test E))
                 (E1 (if.then E))
                 (E2 (if.else E)))
             (if (constant? E0)
                 ; FIXME: E1 and E2 might not be a legal rhs,
                 ; so we can't just return the simplified E1 or E2.
                 (let ((E1 (if (constant.value E0) E1 E2)))
                   (call-with-values
                    (lambda () (scan E1 env available))
                    (lambda (E1 F1 regbindings1)
                      (cond ((or (not (call? E1))
                                 (not (lambda? (call.proc E1))))
                             (values E1 F1 regbindings1))
                            (else
                             ; FIXME: Must return a valid rhs.
                             (values (make-conditional
                                      (make-constant #t)
                                      E1
                                      (make-constant 0))
                                     F1
                                     regbindings1))))))
                 (call-with-values
                  (lambda () (scan E0 env available))
                  (lambda (E0 F0 regbindings0)
                    (if (not (null? regbindings0))
                        (error 'scan-rhs 'if))
                    (if (not (eq? E0 (if.test E)))
                        (scan-rhs (make-conditional E0 E1 E2)
                                  env available)
                        (let ((available1
                               (copy-available-table available))
                              (available2
                               (copy-available-table available)))
                          (if (variable? E0)
                              (let ((T0 (variable.name E0)))
                                (available-add!
                                 available2 T0 (make-constant #f)))
                              (error (make-readable E #t)))
                          (call-with-values
                           (lambda () (scan E1 env available1))
                           (lambda (E1 F1 regbindings1)
                             (call-with-values
                              (lambda ()
                                (wrap-with-register-bindings
                                 regbindings1 E1 F1))
                              (lambda (E1 F1)
                                (call-with-values
                                 (lambda () (scan E2 env available2))
                                 (lambda (E2 F2 regbindings2)
                                   (call-with-values
                                    (lambda ()
                                      (wrap-with-register-bindings
                                       regbindings2 E2 F2))
                                    (lambda (E2 F2)
                                      (let ((E (make-conditional
                                                E0 E1 E2))
                                            (F (union F0 F1 F2)))
                                        (available-intersect!
                                         available
                                         available1
                                         available2)
                                        (values E F '())))))))))))))))))
          
          
          ((assignment? E)
           (call-with-values
            (lambda () (scan-rhs (assignment.rhs E) env available))
            (lambda (E1 F1 regbindings1)
              (if (not (null? regbindings1))
                  (error 'scan-rhs 'set!))
              (available-kill! available available:killer:globals)
              (values (make-assignment (assignment.lhs E) E1)
                      (union (list (assignment.lhs E)) F1)
                      '()))))
          
          ((begin? E)
           ; Shouldn't occur in A-normal form.
           (error 'scan-rhs 'begin))
          
          ((real-call? E)
           (let* ((E0 (call.proc E))
                  (args (call.args E))
                  (regcontents (append regvars
                                       (map (lambda (x) #f) args))))
             (let loop ((args args)
                        (regs argument-registers)
                        (regcontents regcontents)
                        (newargs '())
                        (regbindings '())
                        (F (if (variable? E0)
                               (let ((f (variable.name E0)))
                                 (used-variable! f)
                                 (list f))
                               (empty-set))))
               (cond ((null? args)
                      (available-kill! available available:killer:all)
                      (values (make-call E0 (reverse newargs))
                              F
                              regbindings))
                     ((null? regs)
                      (let ((arg (car args)))
                        (loop (cdr args)
                              '()
                              (cdr regcontents)
                              (cons arg newargs)
                              regbindings
                              (if (variable? arg)
                                  (let ((name (variable.name arg)))
                                    (used-variable! name)
                                    (union (list name) F))
                                  F))))
                     ((and commoning?
                           (variable? (car args))
                           (available-variable
                            available
                            (variable.name (car args))))
                      (let* ((name (variable.name (car args)))
                             (Enew (available-variable available name)))
                        (loop (cons Enew (cdr args))
                              regs regcontents newargs regbindings F)))
                     ((and target-registers?
                           (variable? (car args))
                           (let ((x (variable.name (car args))))
                             ; We haven't yet recorded this use.
                             (or (local-variable-not-used? x)
                                 (and (memq x regvars)
                                      (not (eq? x (car regcontents)))))))
                      (let* ((x (variable.name (car args)))
                             (R (car regs))
                             (newarg (make-variable R)))
                        (used-variable! x)
                        (loop (cdr args)
                              (cdr regs)
                              (cdr regcontents)
                              (cons newarg newargs)
                              (cons (make-regbinding R x newarg)
                                    regbindings)
                              (union (list R) F))))
                     (else
                      (let ((E1 (car args)))
                        (loop (cdr args)
                              (cdr regs)
                              (cdr regcontents)
                              (cons E1 newargs)
                              regbindings
                              (if (variable? E1)
                                  (let ((name (variable.name E1)))
                                    (used-variable! name)
                                    (union (list name) F))
                                  F))))))))
          
          ((call? E)
           ; Must be a call to a primop.
           (let* ((E0 (call.proc E))
                  (f0 (variable.name E0)))
             (let loop ((args (call.args E))
                        (newargs '())
                        (F (list f0)))
               (cond ((null? args)
                      (let* ((E (make-call E0 (reverse newargs)))
                             (T (and commoning?
                                     (available-expression
                                      available E))))
                        (if T
                            (begin (abandon-expression! E)
                                   (scan-rhs (make-variable T) env available))
                            (begin
                             (available-kill!
                              available
                              (prim-kills (prim-entry f0)))
                             (cond ((eq? f0 name:CHECK!)
                                    (let ((x (car (call.args E))))
                                      (cond ((not (runtime-safety-checking))
                                             (abandon-expression! E)
                                             ;(values x '() '())
                                             (scan-rhs x env available))
                                            ((variable? x)
                                             (available-add!
                                              available
                                              (variable.name x)
                                              (make-constant #t))
                                             (values E F '()))
                                            ((constant.value x)
                                             (abandon-expression! E)
                                             (values x '() '()))
                                            (else
                                             (declaration-error E)
                                             (values E F '())))))
                                   (else
                                    (values E F '())))))))
                     ((variable? (car args))
                      (let* ((E1 (car args))
                             (x (variable.name E1))
                             (Enew
                              (and commoning?
                                   (available-variable available x))))
                        (if Enew
                            ; All of the arguments are constants or
                            ; variables, so if the variable is replaced
                            ; here it will be replaced throughout the call.
                            (loop (cons Enew (cdr args))
                                  newargs
                                  (remq x F))
                            (begin
                             (used-variable! x)
                             (loop (cdr args)
                                   (cons (car args) newargs)
                                   (union (list x) F))))))
                     (else
                      (loop (cdr args)
                            (cons (car args) newargs)
                            F))))))
          
          (else
           (error 'scan-rhs (make-readable E)))))
       
       (call-with-values
        (lambda () (scan E env available))
        (lambda (E F regbindings)
          (call-with-values
           (lambda () (wrap-with-register-bindings regbindings E F))
           (lambda (E F)
             (values E F '()))))))
     
     (call-with-values
      (lambda ()
        (scan-body E
                   (make-hashtree symbol-hash assq)
                   (make-available-table)
                   '()))
      (lambda (E F regbindings)
        (if (not (null? regbindings))
            (error 'scan-body))
        E)))))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 16 June 1999.
;
; Intraprocedural representation inference.

(define (representation-analysis exp)
  (let* ((debugging? #f)
         (integrate-usual? (integrate-usual-procedures))
         (known (make-hashtable symbol-hash assq))
         (types (make-hashtable symbol-hash assq))
         (g (callgraph exp))
         (schedule (list (callgraphnode.code (car g))))
         (changed? #f)
         (mutate? #f))
    
    ; known is a hashtable that maps the name of a known local procedure
    ; to a list of the form (tv1 ... tvN), where tv1, ..., tvN
    ; are type variables that stand for the representation types of its
    ; arguments.  The type variable that stands for the representation
    ; type of the result of the procedure has the same name as the
    ; procedure itself.
    
    ; types is a hashtable that maps local variables and the names
    ; of known local procedures to an approximation of their
    ; representation type.
    ; For a known local procedure, the representation type is for the
    ; result of the procedure, not the procedure itself.
    
    ; schedule is a stack of work that needs to be done.
    ; Each entry in the stack is either an escaping lambda expression
    ; or the name of a known local procedure.
    
    (define (schedule! job)
      (if (not (memq job schedule))
          (begin (set! schedule (cons job schedule))
                 (if (not (symbol? job))
                     (callgraphnode.info! (lookup-node job) #t)))))
    
    ; Schedules a known local procedure.
    
    (define (schedule-known-procedure! name)
      ; Mark every known procedure that can actually be called.
      (callgraphnode.info! (assq name g) #t)
      (schedule! name))
    
    ; Schedule all code that calls the given known local procedure.
    
    (define (schedule-callers! name)
      (for-each (lambda (node)
                  (if (and (callgraphnode.info node)
                           (or (memq name (callgraphnode.tailcalls node))
                               (memq name (callgraphnode.nontailcalls node))))
                      (let ((caller (callgraphnode.name node)))
                        (if caller
                            (schedule! caller)
                            (schedule! (callgraphnode.code node))))))
                g))
    
    ; Schedules local procedures of a lambda expression.
    
    (define (schedule-local-procedures! L)
      (for-each (lambda (def)
                  (let ((name (def.lhs def)))
                    (if (known-procedure-is-callable? name)
                        (schedule! name))))
                (lambda.defs L)))
    
    ; Returns true iff the given known procedure is known to be callable.
    
    (define (known-procedure-is-callable? name)
      (callgraphnode.info (assq name g)))
    
    ; Sets CHANGED? to #t and returns #t if the type variable's
    ; approximation has changed; otherwise returns #f.
    
    (define (update-typevar! tv type)
      (let* ((type0 (hashtable-get types tv))
             (type0 (or type0
                        (begin (hashtable-put! types tv rep:bottom)
                               rep:bottom)))
             (type1 (representation-union type0 type)))
        (if (eq? type0 type1)
            #f
            (begin (hashtable-put! types tv type1)
                   (set! changed? #t)
                   (if (and debugging? mutate?)
                       (begin (display "******** Changing type of ")
                              (display tv)
                              (display " from ")
                              (display (rep->symbol type0))
                              (display " to ")
                              (display (rep->symbol type1))
                              (newline)))
                   #t))))
    
    ; GIven the name of a known local procedure, returns its code.
    
    (define (lookup-code name)
      (callgraphnode.code (assq name g)))
    
    ; Given a lambda expression, either escaping or the code for
    ; a known local procedure, returns its node in the call graph.
    
    (define (lookup-node L)
      (let loop ((g g))
        (cond ((null? g)
               (error "Unknown lambda expression" (make-readable L #t)))
              ((eq? L (callgraphnode.code (car g)))
               (car g))
              (else
               (loop (cdr g))))))
    
    ; Given: a type variable, expression, and a set of constraints.
    ; Side effects:
    ;     Update the representation types of all variables that are
    ;         bound within the expression.
    ;     Update the representation types of all arguments to known
    ;         local procedures that are called within the expression.
    ;     If the representation type of an argument to a known local
    ;         procedure changes, then schedule that procedure's code
    ;         for analysis.
    ;     Update the constraint set to reflect the constraints that
    ;         hold following execution of the expression.
    ;     If mutate? is true, then transform the expression to rely
    ;         on the representation types that have been inferred.
    ; Return: type of the expression under the current assumptions
    ;     and constraints.
    
    (define (analyze exp constraints)
      
      (if (and #f debugging?)
          (begin (display "Analyzing: ")
                 (newline)
                 (pretty-print (make-readable exp #t))
                 (newline)))
      
      (case (car exp)
        
        ((quote)
         (representation-of-value (constant.value exp)))
        
        ((begin)
         (let* ((name (variable.name exp)))
           (representation-typeof name types constraints)))
        
        ((lambda)
         (schedule! exp)
         rep:procedure)
        
        ((set!)
         (analyze (assignment.rhs exp) constraints)
         (constraints-kill! constraints available:killer:globals)
         rep:object)
        
        ((if)
         (let* ((E0 (if.test exp))
                (E1 (if.then exp))
                (E2 (if.else exp))
                (type0 (analyze E0 constraints)))
           (if mutate?
               (cond ((representation-subtype? type0 rep:true)
                      (if.test-set! exp (make-constant #t)))
                     ((representation-subtype? type0 rep:false)
                      (if.test-set! exp (make-constant #f)))))
           (cond ((representation-subtype? type0 rep:true)
                  (analyze E1 constraints))
                 ((representation-subtype? type0 rep:false)
                  (analyze E2 constraints))
                 ((variable? E0)
                  (let* ((T0 (variable.name E0))
                         (ignored (analyze E0 constraints))
                         (constraints1 (copy-constraints-table constraints))
                         (constraints2 (copy-constraints-table constraints)))
                    (constraints-add! types
                                      constraints1
                                      (make-type-constraint
                                       T0 rep:true available:killer:immortal))
                    (constraints-add! types
                                      constraints2
                                      (make-type-constraint
                                       T0 rep:false available:killer:immortal))
                    (let* ((type1 (analyze E1 constraints1))
                           (type2 (analyze E2 constraints2))
                           (type (representation-union type1 type2)))
                      (constraints-intersect! constraints
                                              constraints1
                                              constraints2)
                      type)))
                 (else
                  (representation-error "Bad ANF" (make-readable exp #t))))))
        
        (else
         (let ((proc (call.proc exp))
               (args (call.args exp)))
           (cond ((lambda? proc)
                  (cond ((null? args)
                         (analyze-let0 exp constraints))
                        ((null? (cdr args))
                         (analyze-let1 exp constraints))
                        (else
                         (error "Compiler bug: pass3rep"))))
                 ((variable? proc)
                  (let* ((procname (variable.name proc)))
                    (cond ((hashtable-get known procname)
                           =>
                           (lambda (vars)
                             (analyze-known-call exp constraints vars)))
                          (integrate-usual?
                           (let ((entry (prim-entry procname)))
                             (if entry
                                 (analyze-primop-call exp constraints entry)
                                 (analyze-unknown-call exp constraints))))
                          (else
                           (analyze-unknown-call exp constraints)))))
                 (else
                  (analyze-unknown-call exp constraints)))))))
    
    (define (analyze-let0 exp constraints)
      (let ((proc (call.proc exp)))
        (schedule-local-procedures! proc)
        (if (null? (lambda.args proc))
            (analyze (lambda.body exp) constraints)
            (analyze-unknown-call exp constraints))))
    
    (define (analyze-let1 exp constraints)
      (let* ((proc (call.proc exp))
             (vars (lambda.args proc)))
        (schedule-local-procedures! proc)
        (if (and (pair? vars)
                 (null? (cdr vars)))
            (let* ((T1 (car vars))
                   (E1 (car (call.args exp))))
              (if (and integrate-usual? (call? E1))
                  (let ((proc (call.proc E1))
                        (args (call.args E1)))
                    (if (variable? proc)
                        (let* ((op (variable.name proc))
                               (entry (prim-entry op))
                               (K1 (if entry
                                       (prim-lives-until entry)
                                       available:killer:dead)))
                          (if (not (= K1 available:killer:dead))
                              ; Must copy the call to avoid problems
                              ; with side effects when mutate? is true.
                              (constraints-add!
                               types
                               constraints
                               (make-constraint T1
                                                (make-call proc args)
                                                K1)))))))
              (update-typevar! T1 (analyze E1 constraints))
              (analyze (lambda.body proc) constraints))
            (analyze-unknown-call exp constraints))))
    
    (define (analyze-primop-call exp constraints entry)
      (let* ((op (prim-opcodename entry))
             (args (call.args exp))
             (argtypes (map (lambda (arg) (analyze arg constraints))
                            args))
             (type (rep-result? op argtypes)))
        (constraints-kill! constraints (prim-kills entry))
        (cond ((and (eq? op 'check!)
                    (variable? (car args)))
               (let ((varname (variable.name (car args))))
                 (if (and mutate?
                          (representation-subtype? (car argtypes) rep:true))
                     (call.args-set! exp
                                     (cons (make-constant #t) (cdr args))))
                 (constraints-add! types
                                   constraints
                                   (make-type-constraint
                                    varname
                                    rep:true
                                    available:killer:immortal))))
              ((and mutate? (rep-specific? op argtypes))
               =>
               (lambda (newop)
                 (call.proc-set! exp (make-variable newop)))))
        (or type rep:object)))
    
    (define (analyze-known-call exp constraints vars)
      (let* ((procname (variable.name (call.proc exp)))
             (args (call.args exp))
             (argtypes (map (lambda (arg) (analyze arg constraints))
                            args)))
        (if (not (known-procedure-is-callable? procname))
            (schedule-known-procedure! procname))
        (for-each (lambda (var type)
                    (if (update-typevar! var type)
                        (schedule-known-procedure! procname)))
                  vars
                  argtypes)
        ; FIXME: We aren't analyzing the effects of known local procedures.
        (constraints-kill! constraints available:killer:all)
        (hashtable-get types procname)))
    
    (define (analyze-unknown-call exp constraints)
      (analyze (call.proc exp) constraints)
      (for-each (lambda (arg) (analyze arg constraints))
                (call.args exp))
      (constraints-kill! constraints available:killer:all)
      rep:object)
    
    (define (analyze-known-local-procedure name)
      (if debugging?
          (begin (display "Analyzing ")
                 (display name)
                 (newline)))
      (let ((L (lookup-code name))
            (constraints (make-constraints-table)))
        (schedule-local-procedures! L)
        (let ((type (analyze (lambda.body L) constraints)))
          (if (update-typevar! name type)
              (schedule-callers! name))
          type)))
    
    (define (analyze-unknown-lambda L)
      (if debugging?
          (begin (display "Analyzing escaping lambda expression")
                 (newline)))
      (schedule-local-procedures! L)
      (let ((vars (make-null-terminated (lambda.args L))))
        (for-each (lambda (var)
                    (hashtable-put! types var rep:object))
                  vars)
        (analyze (lambda.body L)
                 (make-constraints-table))))
    
    ; For debugging.
    
    (define (display-types)
      (hashtable-for-each (lambda (f vars)
                            (write f)
                            (display " : returns ")
                            (write (rep->symbol (hashtable-get types f)))
                            (newline)
                            (for-each (lambda (x)
                                        (display "  ")
                                        (write x)
                                        (display ": ")
                                        (write (rep->symbol
                                                (hashtable-get types x)))
                                        (newline))
                                      vars))
                          known))
    
    (define (display-all-types)
      (let* ((vars (hashtable-map (lambda (x type) x) types))
             (vars (twobit-sort (lambda (var1 var2)
                                  (string<=? (symbol->string var1)
                                             (symbol->string var2)))
                                vars)))
        (for-each (lambda (x)
                    (write x)
                    (display ": ")
                    (write (rep->symbol
                            (hashtable-get types x)))
                    (newline))
                  vars)))
    '
    (if debugging?
        (begin (pretty-print (make-readable (car schedule) #t))
               (newline)))
    (if debugging?
        (view-callgraph g))
    
    (for-each (lambda (node)
                (let* ((name (callgraphnode.name node))
                       (code (callgraphnode.code node))
                       (vars (make-null-terminated (lambda.args code)))
                       (known? (symbol? name))
                       (rep (if known? rep:bottom rep:object)))
                  (callgraphnode.info! node #f)
                  (if known?
                      (begin (hashtable-put! known name vars)
                             (hashtable-put! types name rep)))
                  (for-each (lambda (var)
                              (hashtable-put! types var rep))
                            vars)))
              g)
    
    (let loop ()
      (cond ((not (null? schedule))
             (let ((job (car schedule)))
               (set! schedule (cdr schedule))
               (if (symbol? job)
                   (analyze-known-local-procedure job)
                   (analyze-unknown-lambda job))
               (loop)))
            (changed?
             (set! changed? #f)
             (set! schedule (list (callgraphnode.code (car g))))
             (if debugging?
                 (begin (display-all-types) (newline)))
             (loop))))
    
    (if debugging?
        (display-types))
    
    (set! mutate? #t)
    
    ; We don't want to analyze known procedures that are never called.
    
    (set! schedule
          (cons (callgraphnode.code (car g))
                (map callgraphnode.name
                     (filter (lambda (node)
                               (let* ((name (callgraphnode.name node))
                                      (known? (symbol? name))
                                      (marked?
                                       (known-procedure-is-callable? name)))
                                 (callgraphnode.info! node #f)
                                 (and known? marked?)))
                             g))))
    (let loop ()
      (if (not (null? schedule))
          (let ((job (car schedule)))
            (set! schedule (cdr schedule))
            (if (symbol? job)
                (analyze-known-local-procedure job)
                (analyze-unknown-lambda job))
            (loop))))
    
    (if changed?
        (error "Compiler bug in representation inference"))
    
    (if debugging?
        (pretty-print (make-readable (callgraphnode.code (car g)) #t)))
    
    exp))
; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 11 June 1999.
;
; The third "pass" of the Twobit compiler actually consists of several
; passes, which are related by the common theme of flow analysis:
;   interprocedural inlining of known local procedures
;   interprocedural constant propagation and folding
;   intraprocedural commoning, copy propagation, and dead code elimination
;   representation inference (not yet implemented)
;   register targeting
;
; This pass operates as source-to-source transformations on
; expressions written in the subset of Scheme described by the
; following grammar:
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F G <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the input only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  R, F, and G are garbage.
;   *  Variables named IGNORED are neither referenced nor assigned.
;   *  The expression does not share structure with the original input,
;      but might share structure with itself.
;
; Invariants that hold for the output only:
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  R is garbage.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  If a lambda expression is declared to be in A-normal form (see
;      pass3anormal.sch), then it really is in A-normal form.
;
; The phases of pass 3 interact with the referencing information R
; and the free variables F as follows:
;
; Inlining               ignores R,   ignores F,  destroys R,  destroys F.
; Constant propagation      uses R,   ignores F, preserves R, preserves F.
; Conversion to ANF      ignores R,   ignores F,  destroys R,  destroys F.
; Commoning              ignores R,   ignores F,  destroys R,  computes F.
; Register targeting     ignores R,   ignores F,  destroys R,  computes F.

(define (pass3 exp)
  
  (define (phase1 exp)
    (if (interprocedural-inlining)
        (let ((g (callgraph exp)))
          (inline-using-callgraph! g)
          exp)
        exp))
  
  (define (phase2 exp)
    (if (interprocedural-constant-propagation)
        (constant-propagation (copy-exp exp))
        exp))
  
  (define (phase3 exp)
    (if (common-subexpression-elimination)
        (let* ((exp (if (interprocedural-constant-propagation)
                        exp
                        ; alpha-conversion
                        (copy-exp exp)))
               (exp (A-normal-form exp)))
          (if (representation-inference)
              (intraprocedural-commoning exp 'commoning)
              (intraprocedural-commoning exp)))
        exp))
  
  (define (phase4 exp)
    (if (representation-inference)
        (let ((exp (cond ((common-subexpression-elimination)
                          exp)
                         ((interprocedural-constant-propagation)
                          (A-normal-form exp))
                         (else
                          ; alpha-conversion
                          (A-normal-form (copy-exp exp))))))
          (intraprocedural-commoning
           (representation-analysis exp)))
        exp))
  
  (define (finish exp)
    (if (and (not (interprocedural-constant-propagation))
             (not (common-subexpression-elimination)))
        (begin (compute-free-variables! exp)
               exp)
        ;(make-begin (list (make-constant 'anf) exp))))
        exp))
  
  (define (verify exp)
    (check-referencing-invariants exp 'free)
    exp)
  
  (if (global-optimization)
      (verify (finish (phase4 (phase3 (phase2 (phase1 exp))))))
      (begin (compute-free-variables! exp)
             (verify exp))))
; Copyright 1991 Lightship Software, Incorporated.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 4 June 1999

; Implements the following abstract data types.
;
; labels
;     (init-labels)
;     (make-label)
;     cg-label-counter
;
; assembly streams
;     (make-assembly-stream)
;     (assembly-stream-code as)
;     (gen! as . instruction)
;     (gen-instruction! as instruction)
;     (gen-save! as frame)
;     (gen-restore! as frame)
;     (gen-pop! as frame)
;     (gen-setstk! as frame v)
;     (gen-store! as frame r v)
;     (gen-load! as frame r v)
;     (gen-stack! as frame v)
;
; temporaries
;     (init-temps)
;     (newtemp)
;     (newtemps)
;     newtemp-counter
;
; register environments
;     (cgreg-initial)
;     (cgreg-copy regs)
;     (cgreg-tos regs)
;     (cgreg-liveregs regs)
;     (cgreg-live regs r)
;     (cgreg-vars regs)
;     (cgreg-bind! regs r v)
;     (cgreg-bindregs! regs vars)
;     (cgreg-rename! regs alist)
;     (cgreg-release! regs r)
;     (cgreg-clear! regs)
;     (cgreg-lookup regs var)
;     (cgreg-lookup-reg regs r)
;     (cgreg-join! regs1 regs2)
;
; stack frame environments
;     (cgframe-initial)
;     (cgframe-size-cell frame)
;     (cgframe-size frame)
;     (cgframe-copy frame)
;     (cgframe-join! frame1 frame2)
;     (cgframe-update-stale! frame)
;     (cgframe-used! frame)
;     (cgframe-bind! frame n v instruction)
;     (cgframe-touch! frame v)
;     (cgframe-rename! frame alist)
;     (cgframe-release! frame v)
;     (cgframe-lookup frame v)
;     (cgframe-spilled? frame v)
;
; environments
;     (entry.name entry)
;     (entry.kind entry)
;     (entry.rib entry)
;     (entry.offset entry)
;     (entry.label entry)
;     (entry.regnum entry)
;     (entry.arity entry)
;     (entry.op entry)
;     (entry.imm entry)
;     (cgenv-initial)
;     (cgenv-lookup env id)
;     (cgenv-extend env vars procs)
;     (cgenv-bindprocs env procs)
;     (var-lookup var regs frame env)

; Labels.

(define (init-labels)
  (set! cg-label-counter 1000))

(define (make-label)
  (set! cg-label-counter (+ cg-label-counter 1))
  cg-label-counter)

(define cg-label-counter 1000)

;    an assembly stream into which instructions should be emitted
;    an expression
;    the desired target register ('result, a register number, or '#f)
;    a register environment [cgreg]
;    a stack-frame environment [cgframe]
;      contains size of frame, current top of frame
;    a compile-time environment [cgenv]
;    a flag indicating whether the expression is in tail position

; Assembly streams, into which instructions are emitted by side effect.
; Represented as a list of two things:
;
;     Assembly code, represented as a pair whose car is a nonempty list
;     whose cdr is a possibly empty list of MacScheme machine assembly
;     instructions, and whose cdr is the last pair of the car.
;
;     Any Scheme object that the code generator wants to associate with
;     this code.

(define (make-assembly-stream)
  (let ((code (list (list 0))))
    (set-cdr! code (car code))
    (list code #f)))

(define (assembly-stream-code output)
  (if (local-optimizations)
      (filter-basic-blocks (cdar (car output)))
      (cdar (car output))))

(define (assembly-stream-info output)
  (cadr output))

(define (assembly-stream-info! output x)
  (set-car! (cdr output) x)
  #f)

(define (gen-instruction! output instruction)
  (let ((pair (list instruction))
        (code (car output)))
    (set-cdr! (cdr code) pair)
    (set-cdr! code pair)
    output))

;

(define (gen! output . instruction)
  (gen-instruction! output instruction))

(define (gen-save! output frame t0)
  (let ((size (cgframe-size-cell frame)))
    (gen-instruction! output (cons $save size))
    (gen-store! output frame 0 t0)
    (cgframe:stale-set! frame '())))

(define (gen-restore! output frame)
  (let ((size (cgframe-size-cell frame)))
    (gen-instruction! output (cons $restore size))))

(define (gen-pop! output frame)
  (let ((size (cgframe-size-cell frame)))
    (gen-instruction! output (cons $pop size))))

(define (gen-setstk! output frame tempname)
  (let ((instruction (list $nop $setstk -1)))
    (cgframe-bind! frame tempname instruction)
    (gen-instruction! output instruction)))

(define (gen-store! output frame r tempname)
  (let ((instruction (list $nop $store r -1)))
    (cgframe-bind! frame tempname instruction)
    (gen-instruction! output instruction)))

(define (gen-load! output frame r tempname)
  (cgframe-touch! frame tempname)
  (let ((n (entry.slotnum (cgframe-lookup frame tempname))))
    (gen! output $load r n)))

(define (gen-stack! output frame tempname)
  (cgframe-touch! frame tempname)
  (let ((n (entry.slotnum (cgframe-lookup frame tempname))))
    (gen! output $stack n)))

; Returns a temporary name.
; Temporaries are compared using EQ?, so the use of small
; exact integers as temporary names is implementation-dependent.

(define (init-temps)
  (set! newtemp-counter 5000))

(define (newtemp)
  (set! newtemp-counter
        (+ newtemp-counter 1))
  newtemp-counter)

(define newtemp-counter 5000)

(define (newtemps n)
  (if (zero? n)
      '()
      (cons (newtemp)
            (newtemps (- n 1)))))

; New representation of
; Register environments.
; Represented as a list of three items:
;     an exact integer, one more than the highest index of a live register
;     a mutable vector with *nregs* elements of the form
;         #f        (the register is dead)
;         #t        (the register is live)
;         v         (the register contains variable v)
;         t         (the register contains temporary variable t)
;     a mutable vector of booleans: true if the register might be stale

(define (cgreg-makeregs n v1 v2) (list n v1 v2))

(define (cgreg-liveregs regs)
  (car regs))

(define (cgreg-contents regs)
  (cadr regs))

(define (cgreg-stale regs)
  (caddr regs))

(define (cgreg-liveregs-set! regs n)
  (set-car! regs n)
  regs)

(define (cgreg-initial)
  (let ((v1 (make-vector *nregs* #f))
        (v2 (make-vector *nregs* #f)))
    (cgreg-makeregs 0 v1 v2)))

(define (cgreg-copy regs)
  (let* ((newregs (cgreg-initial))
         (v1a (cgreg-contents regs))
         (v2a (cgreg-stale regs))
         (v1 (cgreg-contents newregs))
         (v2 (cgreg-stale newregs))
         (n (vector-length v1a)))
    (cgreg-liveregs-set! newregs (cgreg-liveregs regs))
    (do ((i 0 (+ i 1)))
        ((= i n)
         newregs)
        (vector-set! v1 i (vector-ref v1a i))
        (vector-set! v2 i (vector-ref v2a i)))))

(define (cgreg-tos regs)
  (- (cgreg-liveregs regs) 1))

(define (cgreg-live regs r)
  (if (eq? r 'result)
      (cgreg-tos regs)
      (max r (cgreg-tos regs))))

(define (cgreg-vars regs)
  (let ((m (cgreg-liveregs regs))
        (v (cgreg-contents regs)))
    (do ((i (- m 1) (- i 1))
         (vars '()
               (cons (vector-ref v i)
                     vars)))
        ((< i 0)
         vars))))

(define (cgreg-bind! regs r t)
  (let ((m (cgreg-liveregs regs))
        (v (cgreg-contents regs)))
    (vector-set! v r t)
    (if (>= r m)
        (cgreg-liveregs-set! regs (+ r 1)))))

(define (cgreg-bindregs! regs vars)
  (do ((m (cgreg-liveregs regs) (+ m 1))
       (v (cgreg-contents regs))
       (vars vars (cdr vars)))
      ((null? vars)
       (cgreg-liveregs-set! regs m)
       regs)
      (vector-set! v m (car vars))))

(define (cgreg-rename! regs alist)
  (do ((i (- (cgreg-liveregs regs) 1) (- i 1))
       (v (cgreg-contents regs)))
      ((negative? i))
      (let ((var (vector-ref v i)))
        (if var
            (let ((probe (assv var alist)))
              (if probe
                  (vector-set! v i (cdr probe))))))))

(define (cgreg-release! regs r)
  (let ((m (cgreg-liveregs regs))
        (v (cgreg-contents regs)))
    (vector-set! v r #f)
    (vector-set! (cgreg-stale regs) r #t)
    (if (= r (- m 1))
        (do ((m r (- m 1)))
            ((or (negative? m)
                 (vector-ref v m))
             (cgreg-liveregs-set! regs (+ m 1)))))))

(define (cgreg-release-except! regs vars)
  (do ((i (- (cgreg-liveregs regs) 1) (- i 1))
       (v (cgreg-contents regs)))
      ((negative? i))
      (let ((var (vector-ref v i)))
        (if (and var (not (memq var vars)))
            (cgreg-release! regs i)))))

(define (cgreg-clear! regs)
  (let ((m (cgreg-liveregs regs))
        (v1 (cgreg-contents regs))
        (v2 (cgreg-stale regs)))
    (do ((r 0 (+ r 1)))
        ((= r m)
         (cgreg-liveregs-set! regs 0))
        (vector-set! v1 r #f)
        (vector-set! v2 r #t))))

(define (cgreg-lookup regs var)
  (let ((m (cgreg-liveregs regs))
        (v (cgreg-contents regs)))
    (define (loop i)
      (cond ((< i 0)
             #f)
            ((eq? var (vector-ref v i))
             (list var 'register i '(object)))
            (else
             (loop (- i 1)))))
    (loop (- m 1))))

(define (cgreg-lookup-reg regs r)
  (let ((m (cgreg-liveregs regs))
        (v (cgreg-contents regs)))
    (if (<= m r)
        #f
        (vector-ref v r))))

(define (cgreg-join! regs1 regs2)
  (let ((m1 (cgreg-liveregs regs1))
        (m2 (cgreg-liveregs regs2))
        (v1 (cgreg-contents regs1))
        (v2 (cgreg-contents regs2))
        (stale1 (cgreg-stale regs1)))
    (do ((i (- (max m1 m2) 1) (- i 1)))
        ((< i 0)
         (cgreg-liveregs-set! regs1 (min m1 m2)))
        (let ((x1 (vector-ref v1 i))
              (x2 (vector-ref v2 i)))
          (cond ((eq? x1 x2)
                 #t)
                ((not x1)
                 (if x2
                     (vector-set! stale1 i #t)))
                (else
                 (vector-set! v1 i #f)
                 (vector-set! stale1 i #t)))))))

; New representation of
; Stack-frame environments.
; Represented as a three-element list.
;
; Its car is a list whose car is a list of slot entries, each
; of the form
;    (v n instruction stale)
; where
;    v is the name of a variable or temporary,
;    n is #f or a slot number,
;    instruction is a possibly phantom store or setstk instruction
;       that stores v into slot n, and
;    stale is a list of stale slot entries, each of the form
;          (#t . n)
;       or (#f . -1)
;       where slot n had been allocated, initialized, and released
;       before the store or setstk instruction was generated.
; Slot entries are updated by side effect.
;
; Its cadr is the list of currently stale slots.
;
; Its caddr is a list of variables that are free in the continuation,
; or #f if that information is unknown.
; This information allows a direct-style code generator to know when
; a slot becomes stale.
;
; Its cadddr is the size of the stack frame, which can be
; increased but not decreased.  The cdddr of the stack frame
; environment is shared with the save instruction that
; created the frame.  What a horrible crock!

; This stuff is private to the implementation of stack-frame
; environments.

(define cgframe:slots car)
(define cgframe:stale cadr)
(define cgframe:livevars caddr)
(define cgframe:slot.name car)
(define cgframe:slot.offset cadr)
(define cgframe:slot.instruction caddr)
(define cgframe:slot.stale cadddr)

(define cgframe:slots-set! set-car!)
(define (cgframe:stale-set! frame stale)
  (set-car! (cdr frame) stale))
(define (cgframe:livevars-set! frame vars)
  (set-car! (cddr frame) vars))

(define cgframe:slot.name-set! set-car!)

(define (cgframe:slot.offset-set! entry n)
  (let ((instruction (caddr entry)))
    (if (or (not (eq? #f (cadr entry)))
            (not (eq? $nop (car instruction))))
        (error "Compiler bug: cgframe" entry)
        (begin
         (set-car! (cdr entry) n)
         (set-car! instruction (cadr instruction))
         (set-cdr! instruction (cddr instruction))
         (if (eq? $setstk (car instruction))
             (set-car! (cdr instruction) n)
             (set-car! (cddr instruction) n))))))

; Reserves a slot offset that was unused where the instruction
; of the slot entry was generated, and returns that offset.

(define (cgframe:unused-slot frame entry)
  (let* ((stale (cgframe:slot.stale entry))
         (probe (assq #t stale)))
    (if probe
        (let ((n (cdr probe)))
          (if (zero? n)
              (cgframe-used! frame))
          (set-car! probe #f)
          n)
        (let* ((cell (cgframe-size-cell frame))
               (n (+ 1 (car cell))))
          (set-car! cell n)
          (if (zero? n)
              (cgframe:unused-slot frame entry)
              n)))))

; Public entry points.

; The runtime system requires slot 0 of a frame to contain
; a closure whose code pointer contains the return address
; of the frame.
; To prevent slot 0 from being used for some other purpose,
; we rely on a complex trick:  Slot 0 is initially stale.
; Gen-save! generates a store instruction for register 0,
; with slot 0 as the only stale slot for that instruction;
; then gen-save! clears the frame's set of stale slots, which
; prevents other store instructions from using slot 0.

(define (cgframe-initial)
  (list '()
        (list (cons #t 0))
        '#f
        -1))

(define cgframe-livevars cgframe:livevars)
(define cgframe-livevars-set! cgframe:livevars-set!)

(define (cgframe-size-cell frame)
  (cdddr frame))

(define (cgframe-size frame)
  (car (cgframe-size-cell frame)))

(define (cgframe-used! frame)
  (if (negative? (cgframe-size frame))
      (set-car! (cgframe-size-cell frame) 0)))

; Called only by gen-store!, gen-setstk!

(define (cgframe-bind! frame var instruction)
  (cgframe:slots-set! frame
                      (cons (list var #f instruction (cgframe:stale frame))
                            (cgframe:slots frame))))

; Called only by gen-load!, gen-stack!

(define (cgframe-touch! frame var)
  (let ((entry (assq var (cgframe:slots frame))))
    (if entry
        (let ((n (cgframe:slot.offset entry)))
          (if (eq? #f n)
              (let ((n (cgframe:unused-slot frame entry)))
                (cgframe:slot.offset-set! entry n))))
        (error "Compiler bug: cgframe-touch!" frame var))))

(define (cgframe-rename! frame alist)
  (for-each (lambda (entry)
              (let ((probe (assq (cgframe:slot.name entry) alist)))
                (if probe
                    (cgframe:slot.name-set! entry (cdr probe)))))
            (cgframe:slots frame)))

(define (cgframe-release! frame var)
  (let* ((slots (cgframe:slots frame))
         (entry (assq var slots)))
    (if entry
        (begin (cgframe:slots-set! frame (remq entry slots))
               (let ((n (cgframe:slot.offset entry)))
                 (if (and (not (eq? #f n))
                          (not (zero? n)))
                     (cgframe:stale-set!
                      frame
                      (cons (cons #t n)
                            (cgframe:stale frame)))))))))

(define (cgframe-release-except! frame vars)
  (let loop ((slots (reverse (cgframe:slots frame)))
             (newslots '())
             (stale (cgframe:stale frame)))
    (if (null? slots)
        (begin (cgframe:slots-set! frame newslots)
               (cgframe:stale-set! frame stale))
        (let ((slot (car slots)))
          (if (memq (cgframe:slot.name slot) vars)
              (loop (cdr slots)
                    (cons slot newslots)
                    stale)
              (let ((n (cgframe:slot.offset slot)))
                (cond ((eq? n #f)
                       (loop (cdr slots)
                             newslots
                             stale))
                      ((zero? n)
                       (loop (cdr slots)
                             (cons slot newslots)
                             stale))
                      (else
                       (loop (cdr slots)
                             newslots
                             (cons (cons #t n) stale))))))))))

(define (cgframe-lookup frame var)
  (let ((entry (assq var (cgframe:slots frame))))
    (if entry
        (let ((n (cgframe:slot.offset entry)))
          (if (eq? #f n)
              (cgframe-touch! frame var))
          (list var 'frame (cgframe:slot.offset entry) '(object)))
        #f)))

(define (cgframe-spilled? frame var)
  (let ((entry (assq var (cgframe:slots frame))))
    (if entry
        (let ((n (cgframe:slot.offset entry)))
          (not (eq? #f n)))
        #f)))

; For a conditional expression, the then and else parts must be
; evaluated using separate copies of the frame environment,
; and those copies must be resolved at the join point.  The
; nature of the resolution depends upon whether the conditional
; expression is in a tail position.
;
; Critical invariant:
; Any store instructions that are generated within either arm of the
; conditional involve variables and temporaries that are local to the
; conditional.
;
; If the conditional expression is in a tail position, then a slot
; that is stale after the test can be allocated independently by the
; two arms of the conditional.  If the conditional expression is in a
; non-tail position, then the slot can be allocated independently
; provided it is not a candidate destination for any previous emitted
; store instruction.

(define (cgframe-copy frame)
  (cons (car frame)
        (cons (cadr frame)
              (cons (caddr frame)
                    (cdddr frame)))))

(define (cgframe-update-stale! frame)
  (let* ((n (cgframe-size frame))
         (v (make-vector (+ 1 n) #t))
         (stale (cgframe:stale frame)))
    (for-each (lambda (x)
                (if (car x)
                    (let ((i (cdr x)))
                      (if (<= i n)
                          (vector-set! v i #f)))))
              stale)
    (for-each (lambda (slot)
                (let ((offset (cgframe:slot.offset slot)))
                  (if offset
                      (vector-set! v offset #f)
                      (for-each (lambda (stale)
                                  (if (car stale)
                                      (let ((i (cdr stale)))
                                        (if (< i n)
                                            (vector-set! v i #f)))))
                                (cgframe:slot.stale slot)))))
              (cgframe:slots frame))
    (do ((i n (- i 1))
         (stale (filter car stale)
                (if (vector-ref v i)
                    (cons (cons #t i) stale)
                    stale)))
        ((<= i 0)
         (cgframe:stale-set! frame stale)))))

(define (cgframe-join! frame1 frame2)
  (let* ((slots1 (cgframe:slots frame1))
         (slots2 (cgframe:slots frame2))
         (slots (intersection slots1 slots2))
         (deadslots (append (difference slots1 slots)
                            (difference slots2 slots)))
         (deadoffsets (make-set
                       (filter (lambda (x) (not (eq? x #f)))
                               (map cgframe:slot.offset deadslots))))
         (stale1 (cgframe:stale frame1))
         (stale2 (cgframe:stale frame2))
         (stale (intersection stale1 stale2))
         (stale (append (map (lambda (n) (cons #t n))
                             deadoffsets)
                        stale)))
    (cgframe:slots-set! frame1 slots)
    (cgframe:stale-set! frame1 stale)))

; Environments.
;
; Each identifier has one of the following kinds of entry.
;
;    (<name> register   <number>                (object))
;    (<name> frame      <slot>                  (object))
;    (<name> lexical    <rib>    <offset>       (object))
;    (<name> procedure  <rib>    <label>        (object))
;    (<name> integrable <arity>  <op>     <imm> (object))
;    (<name> global                             (object))
;
; Implementation.
;
; An environment is represented as a list of the form
;
;    ((<entry> ...)                          ; lexical rib
;     ...)
;
; where each <entry> has one of the forms
;
;    (<name> lexical <offset> (object))
;    (<name> procedure <rib> <label> (object))
;    (<name> integrable <arity> <op> <imm> (object))

(define entry.name car)
(define entry.kind cadr)
(define entry.rib caddr)
(define entry.offset cadddr)
(define entry.label cadddr)
(define entry.regnum caddr)
(define entry.slotnum caddr)
(define entry.arity caddr)
(define entry.op cadddr)
(define (entry.imm entry) (car (cddddr entry)))

(define (cgenv-initial integrable)
  (list (map (lambda (x)
               (list (car x)
                     'integrable
                     (cadr x)
                     (caddr x)
                     (cadddr x)
                     '(object)))
             integrable)))

(define (cgenv-lookup env id)
  (define (loop ribs m)
    (if (null? ribs)
        (cons id '(global (object)))
        (let ((x (assq id (car ribs))))
          (if x
              (case (cadr x)
                ((lexical)
                 (cons id
                       (cons (cadr x)
                             (cons m (cddr x)))))
                ((procedure)
                 (cons id
                       (cons (cadr x)
                             (cons m (cddr x)))))
                ((integrable)
                 (if (integrate-usual-procedures)
                     x
                     (loop '() m)))
                (else ???))
              (loop (cdr ribs) (+ m 1))))))
  (loop env 0))

(define (cgenv-extend env vars procs)
  (cons (do ((n 0 (+ n 1))
             (vars vars (cdr vars))
             (rib (map (lambda (id)
                         (list id 'procedure (make-label) '(object)))
                       procs)
                  (cons (list (car vars) 'lexical n '(object)) rib)))
            ((null? vars) rib))
        env))

(define (cgenv-bindprocs env procs)
  (cons (append (map (lambda (id)
                       (list id 'procedure (make-label) '(object)))
                     procs)
                (car env))
        (cdr env)))

(define (var-lookup var regs frame env)
  (or (cgreg-lookup regs var)
      (cgframe-lookup frame var)
      (cgenv-lookup env var)))

; Compositions.

(define compile
  (lambda (x)
    (pass4 (pass3 (pass2 (pass1 x))) $usual-integrable-procedures$)))

(define compile-block
  (lambda (x)
    (pass4 (pass3 (pass2 (pass1-block x))) $usual-integrable-procedures$)))

; For testing.

(define foo
  (lambda (x)
    (pretty-print (compile x))))

; Find the smallest number of registers such that
; adding more registers does not affect the code
; generated for x (from 4 to 32 registers).

(define (minregs x)
  (define (defregs R)
    (set! *nregs* R)
    (set! *lastreg* (- *nregs* 1))
    (set! *fullregs* (quotient *nregs* 2)))
  (defregs 32)
  (let ((code (assemble (compile x))))
    (define (binary-search m1 m2)
      (if (= (+ m1 1) m2)
          m2
          (let ((midpt (quotient (+ m1 m2) 2)))
            (defregs midpt)
            (if (equal? code (assemble (compile x)))
                (binary-search m1 midpt)
                (binary-search midpt m2)))))
    (defregs 4)
    (let ((newcode (assemble (compile x))))
      (if (equal? code newcode)
          4
          (binary-search 4 32)))))

; Minimums:
;  browse     10
;  triangle    5
;  traverse   10
;  destruct    6
;  puzzle      8,8,10,7
;  tak         6
;  fft        28   (changing the named lets to macros didn't matter)
; Copyright 1991 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 7 June 1999.
;
; Fourth pass of the Twobit compiler:
;   code generation for the MacScheme machine.
;
; This pass operates on input expressions described by the
; following grammar and the invariants that follow it.
;
; "X ..." means zero or more occurrences of X.
;
; L  -->  (lambda (I_1 ...)
;           (begin D ...)
;           (quote (R F G <decls> <doc>)
;           E)
;      |  (lambda (I_1 ... . I_rest)
;           (begin D ...)
;           (quote (R F G <decls> <doc>))
;           E)
; D  -->  (define I L)
; E  -->  (quote K)                        ; constants
;      |  (begin I)                        ; variable references
;      |  L                                ; lambda expressions
;      |  (E0 E1 ...)                      ; calls
;      |  (set! I E)                       ; assignments
;      |  (if E0 E1 E2)                    ; conditionals
;      |  (begin E0 E1 E2 ...)             ; sequential expressions
; I  -->  <identifier>
;
; R  -->  ((I <references> <assignments> <calls>) ...)
; F  -->  (I ...)
; G  -->  (I ...)
;
; Invariants that hold for the input
;   *  There are no assignments except to global variables.
;   *  If I is declared by an internal definition, then the right hand
;      side of the internal definition is a lambda expression and I
;      is referenced only in the procedure position of a call.
;   *  Every procedure defined by an internal definition takes a
;      fixed number of arguments.
;   *  Every call to a procedure defined by an internal definition
;      passes the correct number of arguments.
;   *  For each lambda expression, the associated F is a list of all
;      the identifiers that occur free in the body of that lambda
;      expression, and possibly a few extra identifiers that were
;      once free but have been removed by optimization.
;   *  For each lambda expression, the associated G is a subset of F
;      that contains every identifier that occurs free within some
;      inner lambda expression that escapes, and possibly a few that
;      don't.  (Assignment-elimination does not calculate G exactly.)
;   *  Variables named IGNORED are neither referenced nor assigned.
;   *  Any lambda expression that is declared to be in A-normal form
;      really is in A-normal form.
;
; 
; Stack frames are created by "save" instructions.
; A save instruction is generated
; 
;     *  at the beginning of each lambda body
;     *  at the beginning of the code for each arm of a conditional,
;        provided:
;          the conditional is in a tail position
;          the frames that were allocated by the save instructions
;            that dominate the arms of the conditional have not been
;            used (those save instructions will be eliminated during
;            assembly)
;
; The operand of a save instruction, and of its matching pop instructions,
; increases automatically as frame slots are allocated.
; 
; The code generated to return from a procedure is
; 
;         pop     n
;         return
; 
; The code generated for a tail call is
; 
;         pop     n
;         invoke  ...
;
; Invariant:  When the code generator reserves an argument register
; to hold a value, that value is named, and is stored into the current
; stack frame.  These store instructions are eliminated during assembly
; unless there is a matching load instruction.  If all of the instructions
; that store into a stack frame are eliminated, then the stack frame
; itself is eliminated.
; Exception:  An argument register may be used without naming or storing
; its value provided the register is not in use and no expressions are
; evaluated while it contains the unnamed and unstored value.


(define (pass4 exp integrable)
  (init-labels)
  (init-temps)
  (let ((output (make-assembly-stream))
        (frame (cgframe-initial))
        (regs (cgreg-initial))
        (t0 (newtemp)))
    (assembly-stream-info! output (make-hashtable equal-hash assoc))
    (cgreg-bind! regs 0 t0)
    (gen-save! output frame t0)
    (cg0 output
         exp
         'result
         regs
         frame
         (cgenv-initial integrable)
         #t)
    (pass4-code output)))

(define (pass4-code output)
  (hashtable-for-each (lambda (situation label)
                        (cg-trap output situation label))
                      (assembly-stream-info output))
  (assembly-stream-code output))

; Given:
;    an assembly stream into which instructions should be emitted
;    an expression
;    the target register
;      ('result, a register number, or '#f; tail position implies 'result)
;    a register environment [cgreg]
;    a stack-frame environment [cgframe]
;    a compile-time environment [cgenv]
;    a flag indicating whether the expression is in tail position
; Returns:
;    the target register ('result or a register number)
; Side effects:
;    may change the register and stack-frame environments
;    may increase the size of the stack frame, which changes previously
;       emitted instructions
;    writes instructions to the assembly stream

(define (cg0 output exp target regs frame env tail?)
  (case (car exp)
    ((quote)    (gen! output $const (constant.value exp))
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output frame regs 'result target)))
    ((lambda)   (cg-lambda output exp regs frame env)
                (if tail?
                    (begin (gen-pop! output frame)
                           (gen! output $return)
                           'result)
                    (cg-move output frame regs 'result target)))
    ((set!)     (cg0 output (assignment.rhs exp) 'result regs frame env #f)
                (cg-assignment-result output exp target regs frame env tail?))
    ((if)       (cg-if output exp target regs frame env tail?))
    ((begin)    (if (variable? exp)
                    (cg-variable output exp target regs frame env tail?)
                    (cg-sequential output exp target regs frame env tail?)))
    (else       (cg-call output exp target regs frame env tail?))))

; Lambda expressions that evaluate to closures.
; This is hard because the MacScheme machine's lambda instruction
; closes over the values that are in argument registers 0 through r
; (where r can be larger than *nregs*).
; The set of free variables is calculated and then sorted to minimize
; register shuffling.
;
; Returns: nothing.

(define (cg-lambda output exp regs frame env)
  (let* ((args (lambda.args exp))
         (vars (make-null-terminated args))
         (free (difference (lambda.F exp) vars))
         (free (cg-sort-vars free regs frame env))
         (newenv (cgenv-extend env (cons #t free) '()))
         (newoutput (make-assembly-stream)))
    (assembly-stream-info! newoutput (make-hashtable equal-hash assoc))
    (gen! newoutput $.proc)
    (if (list? args)
        (gen! newoutput $args= (length args))
        (gen! newoutput $args>= (- (length vars) 1)))
    (cg-known-lambda newoutput exp newenv)
    (cg-eval-vars output free regs frame env)
    ; FIXME
    '
    (if (not (ignore-space-leaks))
        ; FIXME: Is this the right constant?
        (begin (gen! output $const #f)
               (gen! output $setreg 0)))
    (gen! output
          $lambda
          (pass4-code newoutput)
          (length free)
          (lambda.doc exp))
    ; FIXME
    '
    (if (not (ignore-space-leaks))
        ; FIXME: This load forces a stack frame to be allocated.
        (gen-load! output frame 0 (cgreg-lookup-reg regs 0)))))

; Given a list of free variables, filters out the ones that
; need to be copied into a closure, and sorts them into an order
; that reduces register shuffling.  Returns a sorted version of
; the list in which the first element (element 0) should go
; into register 1, the second into register 2, and so on.

(define (cg-sort-vars free regs frame env)
  (let* ((free (filter (lambda (var)
                         (case (entry.kind
                                (var-lookup var regs frame env))
                           ((register frame)
                            #t)
                           ((lexical)
                            (not (ignore-space-leaks)))
                           (else #f)))
                       free))
         (n (length free))
         (m (min n (- *nregs* 1)))
         (vec (make-vector m #f)))
    (define (loop1 free free-notregister)
      (if (null? free)
          (loop2 0 free-notregister)
          (let* ((var (car free))
                 (entry (cgreg-lookup regs var)))
            (if entry
                (let ((r (entry.regnum entry)))
                  (if (<= r n)
                      (begin (vector-set! vec (- r 1) var)
                             (loop1 (cdr free)
                                    free-notregister))
                      (loop1 (cdr free)
                             (cons var free-notregister))))
                (loop1 (cdr free)
                       (cons var free-notregister))))))
    (define (loop2 i free)
      (cond ((null? free)
             (vector->list vec))
            ((= i m)
             (append (vector->list vec) free))
            ((vector-ref vec i)
             (loop2 (+ i 1) free))
            (else
             (vector-set! vec i (car free))
             (loop2 (+ i 1) (cdr free)))))
    (loop1 free '())))

; Fetches the given list of free variables into the corresponding
; registers in preparation for a $lambda or $lexes instruction.

(define (cg-eval-vars output free regs frame env)
  (let ((n (length free))
        (R-1 (- *nregs* 1)))
    (if (>= n R-1)
        (begin (gen! output $const '())
               (gen! output $setreg R-1)
               (cgreg-release! regs R-1)))
    (do ((r n (- r 1))
         (vars (reverse free) (cdr vars)))
        ((zero? r))
        (let* ((v (car vars))
               (entry (var-lookup v regs frame env)))
          (case (entry.kind entry)
            ((register)
             (let ((r1 (entry.regnum entry)))
               (if (not (eqv? r r1))
                   (if (< r R-1)
                       (begin (gen! output $movereg r1 r)
                              (cgreg-bind! regs r v))
                       (gen! output $reg r1 v)))))
            ((frame)
             (if (< r R-1)
                 (begin (gen-load! output frame r v)
                        (cgreg-bind! regs r v))
                 (gen-stack! output frame v)))
            ((lexical)
             (gen! output $lexical
                          (entry.rib entry)
                          (entry.offset entry)
                          v)
             (if (< r R-1)
                 (begin (gen! output $setreg r)
                        (cgreg-bind! regs r v)
                        (gen-store! output frame r v))))
            (else
             (error "Bug in cg-close-lambda")))
          (if (>= r R-1)
              (begin (gen! output $op2 $cons R-1)
                     (gen! output $setreg R-1)))))))

; Lambda expressions that appear on the rhs of a definition are
; compiled here.  They don't need an args= instruction at their head.
;
; Returns: nothing.

(define (cg-known-lambda output exp env)
  (let* ((vars (make-null-terminated (lambda.args exp)))
         (regs (cgreg-initial))
         (frame (cgframe-initial))
         (t0 (newtemp)))
    (if (member A-normal-form-declaration (lambda.decls exp))
        (cgframe-livevars-set! frame '()))
    (cgreg-bind! regs 0 t0)
    (gen-save! output frame t0)
    (do ((r 1 (+ r 1))
         (vars vars (cdr vars)))
        ((or (null? vars)
             (= r *lastreg*))
         (if (not (null? vars))
             (begin (gen! output $movereg *lastreg* 1)
                    (cgreg-release! regs 1)
                    (do ((vars vars (cdr vars)))
                        ((null? vars))
                        (gen! output $reg 1)
                        (gen! output $op1 $car:pair)
                        (gen-setstk! output frame (car vars))
                        (gen! output $reg 1)
                        (gen! output $op1 $cdr:pair)
                        (gen! output $setreg 1)))))
        (cgreg-bind! regs r (car vars))
        (gen-store! output frame r (car vars)))
    (cg-body output
             exp
             'result
             regs
             frame
             env
             #t)))

; Compiles a let or lambda body.
; The arguments of the lambda expression L are already in
; registers or the stack frame, as specified by regs and frame.
;
; The problem here is that the free variables of an internal
; definition must be in a heap-allocated environment, so any
; such variables in registers must be copied to the heap.
;
; Returns: destination register.

(define (cg-body output L target regs frame env tail?)
  (let* ((exp (lambda.body L))
         (defs (lambda.defs L))
         (free (apply-union
                      (map (lambda (def)
                             (let ((L (def.rhs def)))
                               (difference (lambda.F L)
                                           (lambda.args L))))
                           defs))))
    (cond ((or (null? defs) (constant? exp) (variable? exp))
           (cg0 output exp target regs frame env tail?))
          ((lambda? exp)
           (let* ((free (cg-sort-vars
                         (union free
                                (difference
                                 (lambda.F exp)
                                 (make-null-terminated (lambda.args exp))))
                         regs frame env))
                  (newenv1 (cgenv-extend env
                                         (cons #t free)
                                         (map def.lhs defs)))
                  (args (lambda.args exp))
                  (vars (make-null-terminated args))
                  (newoutput (make-assembly-stream)))
             (assembly-stream-info! newoutput (make-hashtable equal-hash assoc))
             (gen! newoutput $.proc)
             (if (list? args)
                 (gen! newoutput $args= (length args))
                 (gen! newoutput $args>= (- (length vars) 1)))
             (cg-known-lambda newoutput exp newenv1)
             (cg-defs newoutput defs newenv1)
             (cg-eval-vars output free regs frame env)
             (gen! output
                   $lambda
                   (pass4-code newoutput)
                   (length free)
                   (lambda.doc exp))
             (if tail?
                 (begin (gen-pop! output frame)
                        (gen! output $return)
                        'result)
                 (cg-move output frame regs 'result target))))
          ((every? (lambda (def)
                     (every? (lambda (v)
                               (case (entry.kind
                                      (var-lookup v regs frame env))
                                 ((register frame) #f)
                                 (else #t)))
                             (let ((Ldef (def.rhs def)))
                               (difference (lambda.F Ldef)
                                           (lambda.args Ldef)))))
                   defs)
           (let* ((newenv (cgenv-bindprocs env (map def.lhs defs)))
                  (L (make-label))
                  (r (cg0 output exp target regs frame newenv tail?)))
             (if (not tail?)
                 (gen! output $skip L (cgreg-live regs r)))
             (cg-defs output defs newenv)
             (if (not tail?)
                 (gen! output $.label L))
             r))
          (else
           (let ((free (cg-sort-vars free regs frame env)))
             (cg-eval-vars output free regs frame env)
             ; FIXME: Have to restore it too!
             '
             (if (not (ignore-space-leaks))
                 ; FIXME: Is this constant the right one?
                 (begin (gen! output $const #f)
                        (gen! output $setreg 0)))
             (let ((t0 (cgreg-lookup-reg regs 0))
                   (t1 (newtemp))
                   (newenv (cgenv-extend env
                                         (cons #t free)
                                         (map def.lhs defs)))
                   (L (make-label)))
               (gen! output $lexes (length free) free)
               (gen! output $setreg 0)
               (cgreg-bind! regs 0 t1)
               (if tail?
                   (begin (cgframe-release! frame t0)
                          (gen-store! output frame 0 t1)
                          (cg0 output exp 'result regs frame newenv #t)
                          (cg-defs output defs newenv)
                          'result)
                   (begin (gen-store! output frame 0 t1)
                          (cg0 output exp 'result regs frame newenv #f)
                          (gen! output $skip L (cgreg-tos regs))
                          (cg-defs output defs newenv)
                          (gen! output $.label L)
                          (gen-load! output frame 0 t0)
                          (cgreg-bind! regs 0 t0)
                          (cgframe-release! frame t1)
                          (cg-move output frame regs 'result target)))))))))

(define (cg-defs output defs env)
  (for-each (lambda (def)
              (gen! output $.align 4)
              (gen! output $.label
                           (entry.label
                            (cgenv-lookup env (def.lhs def))))
              (gen! output $.proc)
              (gen! output $.proc-doc (lambda.doc (def.rhs def)))
              (cg-known-lambda output
                               (def.rhs def)
                               env))
            defs))

; The right hand side has already been evaluated into the result register.

(define (cg-assignment-result output exp target regs frame env tail?)
  (gen! output $setglbl (assignment.lhs exp))
  (if tail?
      (begin (gen-pop! output frame)
             (gen! output $return)
             'result)
      (cg-move output frame regs 'result target)))

(define (cg-if output exp target regs frame env tail?)
  ; The test can be a constant, because it is awkward
  ; to remove constant tests from an A-normal form.
  (if (constant? (if.test exp))
      (cg0 output
           (if (constant.value (if.test exp))
               (if.then exp)
               (if.else exp))
           target regs frame env tail?)
      (begin
       (cg0 output (if.test exp) 'result regs frame env #f)
       (cg-if-result output exp target regs frame env tail?))))

; The test expression has already been evaluated into the result register.

(define (cg-if-result output exp target regs frame env tail?)
  (let ((L1 (make-label))
        (L2 (make-label)))
    (gen! output $branchf L1 (cgreg-tos regs))
    (let* ((regs2 (cgreg-copy regs))
           (frame1 (if (and tail?
                            (negative? (cgframe-size frame)))
                       (cgframe-initial)
                       frame))
           (frame2 (if (eq? frame frame1)
                       (cgframe-copy frame1)
                       (cgframe-initial)))
           (t0 (cgreg-lookup-reg regs 0)))
      (if (not (eq? frame frame1))
          (let ((live (cgframe-livevars frame)))
            (cgframe-livevars-set! frame1 live)
            (cgframe-livevars-set! frame2 live)
            (gen-save! output frame1 t0)
            (cg-saveregs output regs frame1)))
      (let ((r (cg0 output (if.then exp) target regs frame1 env tail?)))
        (if (not tail?)
            (gen! output $skip L2 (cgreg-live regs r)))
        (gen! output $.label L1)
        (if (not (eq? frame frame1))
            (begin (gen-save! output frame2 t0)
                   (cg-saveregs output regs2 frame2))
            (cgframe-update-stale! frame2))
        (cg0 output (if.else exp) r regs2 frame2 env tail?)
        (if (not tail?)
            (begin (gen! output $.label L2)
                   (cgreg-join! regs regs2)
                   (cgframe-join! frame1 frame2)))
        (if (and (not target)
                 (not (eq? r 'result))
                 (not (cgreg-lookup-reg regs r)))
            (cg-move output frame regs r 'result)
            r)))))

(define (cg-variable output exp target regs frame env tail?)
  (define (return id)
    (if tail?
        (begin (gen-pop! output frame)
               (gen! output $return)
               'result)
        (if (and target
                 (not (eq? 'result target)))
            (begin (gen! output $setreg target)
                   (cgreg-bind! regs target id)
                   (gen-store! output frame target id)
                   target)
            'result)))
  ; Same as return, but doesn't emit a store instruction.
  (define (return-nostore id)
    (if tail?
        (begin (gen-pop! output frame)
               (gen! output $return)
               'result)
        (if (and target
                 (not (eq? 'result target)))
            (begin (gen! output $setreg target)
                   (cgreg-bind! regs target id)
                   target)
            'result)))
  (let* ((id (variable.name exp))
         (entry (var-lookup id regs frame env)))
    (case (entry.kind entry)
      ((global integrable)
       (gen! output $global id)
       (return (newtemp)))
      ((lexical)
       (let ((m (entry.rib entry))
             (n (entry.offset entry)))
         (gen! output $lexical m n id)
         (if (or (zero? m)
                 (negative? (cgframe-size frame)))
             (return-nostore id)
             (return id))))
      ((procedure) (error "Bug in cg-variable" exp))
      ((register)
       (let ((r (entry.regnum entry)))
         (if (or tail?
                 (and target (not (eqv? target r))))
             (begin (gen! output $reg (entry.regnum entry) id)
                    (return-nostore id))
             r)))
      ((frame)
       (cond ((eq? target 'result)
              (gen-stack! output frame id)
              (return id))
             (target
              ; Must be non-tail.
              (gen-load! output frame target id)
              (cgreg-bind! regs target id)
              target)
             (else
              ; Must be non-tail.
              (let ((r (choose-register regs frame)))
                (gen-load! output frame r id)
                (cgreg-bind! regs r id)
                r))))
      (else (error "Bug in cg-variable" exp)))))

(define (cg-sequential output exp target regs frame env tail?)
  (cg-sequential-loop output (begin.exprs exp) target regs frame env tail?))

(define (cg-sequential-loop output exprs target regs frame env tail?)
  (cond ((null? exprs)
         (gen! output $const unspecified)
         (if tail?
             (begin (gen-pop! output frame)
                    (gen! output $return)
                    'result)
             (cg-move output frame regs 'result target)))
        ((null? (cdr exprs))
         (cg0 output (car exprs) target regs frame env tail?))
        (else (cg0 output (car exprs) #f regs frame env #f)
              (cg-sequential-loop output
                                  (cdr exprs)
                                  target regs frame env tail?))))

(define (cg-saveregs output regs frame)
  (do ((i 1 (+ i 1))
       (vars (cdr (cgreg-vars regs)) (cdr vars)))
      ((null? vars))
      (let ((t (car vars)))
        (if t
            (gen-store! output frame i t)))))

(define (cg-move output frame regs src dst)
  (define (bind dst)
    (let ((temp (newtemp)))
      (cgreg-bind! regs dst temp)
      (gen-store! output frame dst temp)
      dst))
  (cond ((not dst)
         src)
        ((eqv? src dst)
         dst)
        ((eq? dst 'result)
         (gen! output $reg src)
         dst)
        ((eq? src 'result)
         (gen! output $setreg dst)
         (bind dst))
        ((and (not (zero? src))
              (not (zero? dst)))
         (gen! output $movereg src dst)
         (bind dst))
        (else
         (gen! output $reg src)
         (gen! output $setreg dst)
         (bind dst))))

; On-the-fly register allocator.
; Tries to allocate:
;    a hardware register that isn't being used
;    a hardware register whose contents have already been spilled
;    a software register that isn't being used, unless a stack
;       frame has already been created, in which case it is better to use
;    a hardware register that is in use and hasn't yet been spilled
;
; All else equal, it is better to allocate a higher-numbered register
; because the lower-numbered registers are targets when arguments
; are being evaluated.
;
; Invariant:  Every register that is returned by this allocator
; is either not in use or has been spilled.

(define (choose-register regs frame)
  (car (choose-registers regs frame 1)))

(define (choose-registers regs frame n)
  
  ; Find unused hardware registers.
  (define (loop1 i n good)
    (cond ((zero? n)
           good)
          ((zero? i)
           (if (negative? (cgframe-size frame))
               (hardcase)
               (loop2 (- *nhwregs* 1) n good)))
          (else
           (if (cgreg-lookup-reg regs i)
               (loop1 (- i 1) n good)
               (loop1 (- i 1)
                      (- n 1)
                      (cons i good))))))
  
  ; Find already spilled hardware registers.
  (define (loop2 i n good)
    (cond ((zero? n)
           good)
          ((zero? i)
           (hardcase))
          (else
           (let ((t (cgreg-lookup-reg regs i)))
             (if (and t (cgframe-spilled? frame t))
                 (loop2 (- i 1)
                        (- n 1)
                        (cons i good))
                 (loop2 (- i 1) n good))))))
  
  ; This is ridiculous.
  ; Fortunately the correctness of the compiler is independent
  ; of the predicate used for this sort.
  
  (define (hardcase)
    (let* ((frame-exists? (not (negative? (cgframe-size frame))))
           (stufftosort
            (map (lambda (r)
                   (let* ((t (cgreg-lookup-reg regs r))
                          (spilled?
                           (and t
                                (cgframe-spilled? frame t))))
                     (list r t spilled?)))
                 (cdr (iota *nregs*))))
           (registers
            (twobit-sort
             (lambda (x1 x2)
               (let ((r1 (car x1))
                     (r2 (car x2))
                     (t1 (cadr x1))
                     (t2 (cadr x2)))
                 (cond ((< r1 *nhwregs*)
                        (cond ((not t1)                     #t)
                              ((< r2 *nhwregs*)
                               (cond ((not t2)              #f)
                                     ((caddr x1)            #t)
                                     ((caddr x2)            #f)
                                     (else                  #t)))
                              (frame-exists?                #t)
                              (t2                           #t)
                              (else                         #f)))
                       ((< r2 *nhwregs*)
                        (cond (frame-exists?                #f)
                              (t1                           #f)
                              (t2                           #t)
                              (else                         #f)))
                       (t1
                        (if (and (caddr x1)
                                 t2
                                 (not (caddr x2)))
                            #t
                            #f))
                       (else #t))))
             stufftosort)))
      ; FIXME: What was this for?
      '
      (for-each (lambda (register)
                  (let ((t (cadr register))
                        (spilled? (caddr register)))
                    (if (and t (not spilled?))
                        (cgframe-touch! frame t))))
                registers)
      (do ((sorted (map car registers) (cdr sorted))
           (rs '() (cons (car sorted) rs))
           (n n (- n 1)))
          ((zero? n)
           (reverse rs)))))
  
  (if (< n *nregs*)
      (loop1 (- *nhwregs* 1) n '())
      (error (string-append "Compiler bug: can't allocate "
                            (number->string n)
                            " registers on this target."))))
; Copyright 1991 William Clinger
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 21 May 1999.

; Procedure calls.

(define (cg-call output exp target regs frame env tail?)
  (let ((proc (call.proc exp)))
    (cond ((and (lambda? proc)
                (list? (lambda.args proc)))
           (cg-let output exp target regs frame env tail?))
          ((not (variable? proc))
           (cg-unknown-call output exp target regs frame env tail?))
          (else (let ((entry
                       (var-lookup (variable.name proc) regs frame env)))
                  (case (entry.kind entry)
                    ((global lexical frame register)
                     (cg-unknown-call output
                                      exp
                                      target regs frame env tail?))
                    ((integrable)
                     (cg-integrable-call output
                                         exp
                                         target regs frame env tail?))
                    ((procedure)
                     (cg-known-call output
                                    exp
                                    target regs frame env tail?))
                    (else (error "Bug in cg-call" exp))))))))

(define (cg-unknown-call output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (L (make-label)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call output exp target regs frame env tail?))
          (else
           (let ((r0 (cgreg-lookup-reg regs 0)))
             (if (variable? proc)
                 (let ((entry (cgreg-lookup regs (variable.name proc))))
                   (if (and entry
                            (<= (entry.regnum entry) n))
                       (begin (cg-arguments output
                                            (iota1 (+ n 1))
                                            (append args (list proc))
                                            regs frame env)
                              (gen! output $reg (+ n 1)))
                       (begin (cg-arguments output
                                            (iota1 n)
                                            args
                                            regs frame env)
                              (cg0 output proc 'result regs frame env #f)))
                   (if tail?
                       (gen-pop! output frame)
                       (begin (cgframe-used! frame)
                              (gen! output $setrtn L)))
                   (gen! output $invoke n))
                 (begin (cg-arguments output
                                      (iota1 (+ n 1))
                                      (append args (list proc))
                                      regs frame env)
                        (gen! output $reg (+ n 1))
                        (if tail?
                            (gen-pop! output frame)
                            (begin (cgframe-used! frame)
                                   (gen! output $setrtn L)))
                        (gen! output $invoke n)))
             (if tail?
                 'result
                 (begin (gen! output $.align 4)
                        (gen! output $.label L)
                        (gen! output $.cont)
                        (cgreg-clear! regs)
                        (cgreg-bind! regs 0 r0)
                        (gen-load! output frame 0 r0)
                        (cg-move output frame regs 'result target))))))))

(define (cg-known-call output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (n (length args))
         (L (make-label)))
    (cond ((>= (+ n 1) *lastreg*)
           (cg-big-call output exp target regs frame env tail?))
          (else
           (let ((r0 (cgreg-lookup-reg regs 0)))
             (cg-arguments output (iota1 n) args regs frame env)
             (if tail?
                 (gen-pop! output frame)
                 (begin (cgframe-used! frame)
                        (gen! output $setrtn L)))
             (let* ((entry (cgenv-lookup env (variable.name (call.proc exp))))
                    (label (entry.label entry))
                    (m (entry.rib entry)))
               (if (zero? m)
                   (gen! output $branch label n)
                   (gen! output $jump m label n)))
             (if tail?
                 'result
                 (begin (gen! output $.align 4)
                        (gen! output $.label L)
                        (gen! output $.cont)
                        (cgreg-clear! regs)
                        (cgreg-bind! regs 0 r0)
                        (gen-load! output frame 0 r0)
                        (cg-move output frame regs 'result target))))))))

; Any call can be compiled as follows, even if there are no free registers.
;
; Let T0, T1, ..., Tn be newly allocated stack temporaries.
;
;     <arg0>
;     setstk  T0
;     <arg1>             -|
;     setstk  T1          |
;     ...                 |- evaluate args into stack frame
;     <argn>              |
;     setstk  Tn         -|
;     const   ()
;     setreg  R-1
;     stack   Tn         -|
;     op2     cons,R-1    |
;     setreg  R-1         |
;     ...                 |- cons up overflow args
;     stack   T_{R-1}     |
;     op2     cons,R-1    |
;     setreg  R-1        -|
;     stack   T_{R-2}      -|
;     setreg  R-2           |
;     ...                   |- pop remaining args into registers
;     stack   T1            |
;     setreg  1            -|
;     stack   T0
;     invoke  n

(define (cg-big-call output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (args (call.args exp))
         (n (length args))
         (argslots (newtemps n))
         (procslot (newtemp))
         (r0 (cgreg-lookup-reg regs 0))
         (R-1 (- *nregs* 1))
         (entry (if (variable? proc)
                    (let ((entry
                           (var-lookup (variable.name proc)
                                       regs frame env)))
                      (if (eq? (entry.kind entry) 'procedure)
                          entry
                          #f))
                    #f))
         (L (make-label)))
    (if (not entry)
        (begin
         (cg0 output proc 'result regs frame env #f)
         (gen-setstk! output frame procslot)))
    (for-each (lambda (arg argslot)
                (cg0 output arg 'result regs frame env #f)
                (gen-setstk! output frame argslot))
              args
              argslots)
    (cgreg-clear! regs)
    (gen! output $const '())
    (gen! output $setreg R-1)
    (do ((i n (- i 1))
         (slots (reverse argslots) (cdr slots)))
        ((zero? i))
        (if (< i R-1)
            (gen-load! output frame i (car slots))
            (begin (gen-stack! output frame (car slots))
                   (gen! output $op2 $cons R-1)
                   (gen! output $setreg R-1))))
    (if (not entry)
        (gen-stack! output frame procslot))
    (if tail?
        (gen-pop! output frame)
        (begin (cgframe-used! frame)
               (gen! output $setrtn L)))
    (if entry
        (let ((label (entry.label entry))
              (m (entry.rib entry)))
          (if (zero? m)
              (gen! output $branch label n)
              (gen! output $jump m label n)))
        (gen! output $invoke n))
    (if tail?
        'result
        (begin (gen! output $.align 4)
               (gen! output $.label L)
               (gen! output $.cont)
               (cgreg-clear! regs) ; redundant, see above
               (cgreg-bind! regs 0 r0)
               (gen-load! output frame 0 r0)
               (cg-move output frame regs 'result target)))))

(define (cg-integrable-call output exp target regs frame env tail?)
  (let ((args (call.args exp))
        (entry (var-lookup (variable.name (call.proc exp)) regs frame env)))
    (if (= (entry.arity entry) (length args))
        (begin (case (entry.arity entry)
                 ((0) (gen! output $op1 (entry.op entry)))
                 ((1) (cg0 output (car args) 'result regs frame env #f)
                      (gen! output $op1 (entry.op entry)))
                 ((2) (cg-integrable-call2 output
                                           entry
                                           args
                                           regs frame env))
                 ((3) (cg-integrable-call3 output
                                           entry
                                           args
                                           regs frame env))
                 (else (error "Bug detected by cg-integrable-call"
                              (make-readable exp))))
               (if tail?
                   (begin (gen-pop! output frame)
                          (gen! output $return)
                          'result)
                   (cg-move output frame regs 'result target)))
        (if (negative? (entry.arity entry))
            (cg-special output exp target regs frame env tail?)
            (error "Wrong number of arguments to integrable procedure"
                   (make-readable exp))))))

(define (cg-integrable-call2 output entry args regs frame env)
  (let ((op (entry.op entry)))
    (if (and (entry.imm entry)
             (constant? (cadr args))
             ((entry.imm entry) (constant.value (cadr args))))
        (begin (cg0 output (car args) 'result regs frame env #f)
               (gen! output $op2imm
                            op
                            (constant.value (cadr args))))
        (let* ((reg2 (cg0 output (cadr args) #f regs frame env #f))
               (r2 (choose-register regs frame))
               (t2 (if (eq? reg2 'result)
                       (let ((t2 (newtemp)))
                         (gen! output $setreg r2)
                         (cgreg-bind! regs r2 t2)
                         (gen-store! output frame r2 t2)
                         t2)
                       (cgreg-lookup-reg regs reg2))))
          (cg0 output (car args) 'result regs frame env #f)
          (let* ((r2 (or (let ((entry (cgreg-lookup regs t2)))
                           (if entry
                               (entry.regnum entry)
                               #f))
                         (let ((r2 (choose-register regs frame)))
                           (cgreg-bind! regs r2 t2)
                           (gen-load! output frame r2 t2)
                           r2))))
            (gen! output $op2 (entry.op entry) r2)
            (if (eq? reg2 'result)
                (begin (cgreg-release! regs r2)
                       (cgframe-release! frame t2)))))))
  'result)

(define (cg-integrable-call3 output entry args regs frame env)
  (let* ((reg2 (cg0 output (cadr args) #f regs frame env #f))
         (r2 (choose-register regs frame))
         (t2 (if (eq? reg2 'result)
                 (let ((t2 (newtemp)))
                   (gen! output $setreg r2)
                   (cgreg-bind! regs r2 t2)
                   (gen-store! output frame r2 t2)
                   t2)
                 (cgreg-lookup-reg regs reg2)))
         (reg3 (cg0 output (caddr args) #f regs frame env #f))
         (spillregs (choose-registers regs frame 2))
         (t3 (if (eq? reg3 'result)
                 (let ((t3 (newtemp))
                       (r3 (if (eq? t2 (cgreg-lookup-reg
                                        regs (car spillregs)))
                               (cadr spillregs)
                               (car spillregs))))
                   (gen! output $setreg r3)
                   (cgreg-bind! regs r3 t3)
                   (gen-store! output frame r3 t3)
                   t3)
                 (cgreg-lookup-reg regs reg3))))
    (cg0 output (car args) 'result regs frame env #f)
    (let* ((spillregs (choose-registers regs frame 2))
           (r2 (or (let ((entry (cgreg-lookup regs t2)))
                           (if entry
                               (entry.regnum entry)
                               #f))
                   (let ((r2 (car spillregs)))
                     (cgreg-bind! regs r2 t2)
                     (gen-load! output frame r2 t2)
                     r2)))
           (r3 (or (let ((entry (cgreg-lookup regs t3)))
                           (if entry
                               (entry.regnum entry)
                               #f))
                   (let ((r3 (if (eq? r2 (car spillregs))
                                 (cadr spillregs)
                                 (car spillregs))))
                     (cgreg-bind! regs r3 t3)
                     (gen-load! output frame r3 t3)
                     r3))))
      (gen! output $op3 (entry.op entry) r2 r3)
      (if (eq? reg2 'result)
          (begin (cgreg-release! regs r2)
                 (cgframe-release! frame t2)))
      (if (eq? reg3 'result)
          (begin (cgreg-release! regs r3)
                 (cgframe-release! frame t3)))))
  'result)

; Given a short list of expressions that can be evaluated in any order,
; evaluates the first into the result register and the others into any
; register, and returns an ordered list of the registers that contain
; the arguments that follow the first.
; The number of expressions must be less than the number of argument
; registers.

(define (cg-primop-args output args regs frame env)
  
  ; Given a list of expressions to evaluate, a list of variables
  ; and temporary names for arguments that have already been
  ; evaluated, in reverse order, and a mask of booleans that
  ; indicate which temporaries should be released before returning,
  ; returns the correct result.
  
  (define (eval-loop args temps mask)
    (if (null? args)
        (eval-first-into-result temps mask)
        (let ((reg (cg0 output (car args) #f regs frame env #f)))
          (if (eq? reg 'result)
              (let* ((r (choose-register regs frame))
                     (t (newtemp)))
                (gen! output $setreg r)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t)
                (eval-loop (cdr args)
                           (cons t temps)
                           (cons #t mask)))
              (eval-loop (cdr args)
                         (cons (cgreg-lookup-reg regs reg) temps)
                         (cons #f mask))))))
  
  (define (eval-first-into-result temps mask)
    (cg0 output (car args) 'result regs frame env #f)
    (finish-loop (choose-registers regs frame (length temps))
                 temps
                 mask
                 '()))
  
  ; Given a sufficient number of disjoint registers, a list of
  ; variable and temporary names that may need to be loaded into
  ; registers, a mask of booleans that indicates which temporaries
  ; should be released, and a list of registers in forward order,
  ; returns the correct result.
  
  (define (finish-loop disjoint temps mask registers)
    (if (null? temps)
        registers
        (let* ((t (car temps))
               (entry (cgreg-lookup regs t)))
          (if entry
              (let ((r (entry.regnum entry)))
                (if (car mask)
                    (begin (cgreg-release! regs r)
                           (cgframe-release! frame t)))
                (finish-loop disjoint
                             (cdr temps)
                             (cdr mask)
                             (cons r registers)))
              (let ((r (car disjoint)))
                (if (memv r registers)
                    (finish-loop (cdr disjoint) temps mask registers)
                    (begin (gen-load! output frame r t)
                           (cgreg-bind! regs r t)
                           (if (car mask)
                               (begin (cgreg-release! regs r)
                                      (cgframe-release! frame t)))
                           (finish-loop disjoint
                                        (cdr temps)
                                        (cdr mask)
                                        (cons r registers)))))))))
  
  (if (< (length args) *nregs*)
      (eval-loop (cdr args) '() '())
      (error "Bug detected by cg-primop-args" args)))


; Parallel assignment.

; Given a list of target registers, a list of expressions, and a
; compile-time environment, generates code to evaluate the expressions
; into the registers.
;
; Argument evaluation proceeds as follows:
;
; 1.  Evaluate all but one of the complicated arguments.
; 2.  Evaluate remaining arguments.
; 3.  Load spilled arguments from stack.

(define (cg-arguments output targets args regs frame env)
  
  ; Sorts the args and their targets into complicated and
  ; uncomplicated args and targets.
  ; Then it calls evalargs.
  
  (define (sortargs targets args targets1 args1 targets2 args2)
    (if (null? args)
        (evalargs targets1 args1 targets2 args2)
        (let ((target (car targets))
              (arg (car args))
              (targets (cdr targets))
              (args (cdr args)))
          (if (complicated? arg env)
              (sortargs targets
                        args
                        (cons target targets1)
                        (cons arg args1)
                        targets2
                        args2)
              (sortargs targets
                        args
                        targets1
                        args1
                        (cons target targets2)
                        (cons arg args2))))))
  
  ; Given the complicated args1 and their targets1,
  ; and the uncomplicated args2 and their targets2,
  ; evaluates all the arguments into their target registers.
  
  (define (evalargs targets1 args1 targets2 args2)
    (let* ((temps1 (newtemps (length targets1)))
           (temps2 (newtemps (length targets2))))
      (if (not (null? args1))
          (for-each (lambda (arg temp)
                      (cg0 output arg 'result regs frame env #f)
                      (gen-setstk! output frame temp))
                    (cdr args1)
                    (cdr temps1)))
      (if (not (null? args1))
          (evalargs0 (cons (car targets1) targets2)
                     (cons (car args1) args2)
                     (cons (car temps1) temps2))
          (evalargs0 targets2 args2 temps2))
      (for-each (lambda (r t)
                  (let ((temp (cgreg-lookup-reg regs r)))
                    (if (not (eq? temp t))
                        (let ((entry (var-lookup t regs frame env)))
                          (case (entry.kind entry)
                            ((register)
                             (gen! output $movereg (entry.regnum entry) r))
                            ((frame)
                             (gen-load! output frame r t)))
                          (cgreg-bind! regs r t)))
                    (cgframe-release! frame t)))
                (append targets1 targets2)
                (append temps1 temps2))))
  
  (define (evalargs0 targets args temps)
    (if (not (null? targets))
        (let ((para (let* ((regvars (map (lambda (reg)
                                           (cgreg-lookup-reg regs reg))
                                         targets)))
                      (parallel-assignment targets
                                           (map cons regvars targets)
                                           args))))
          (if para
              (let ((targets para)
                    (args (cg-permute args targets para))
                    (temps (cg-permute temps targets para)))
                (for-each (lambda (arg r t)
                            (cg0 output arg r regs frame env #f)
                            (cgreg-bind! regs r t)
                            (gen-store! output frame r t))
                          args
                          para
                          temps))
              (let ((r (choose-register regs frame))
                    (t (car temps)))
                (cg0 output (car args) r regs frame env #f)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t)
                (evalargs0 (cdr targets)
                           (cdr args)
                           (cdr temps)))))))
  
  (if (parallel-assignment-optimization)
      (sortargs (reverse targets) (reverse args) '() '() '() '())
      (cg-evalargs output targets args regs frame env)))

; Left-to-right evaluation of arguments directly into targets.

(define (cg-evalargs output targets args regs frame env)
  (let ((temps (newtemps (length targets))))
    (for-each (lambda (arg r t)
                (cg0 output arg r regs frame env #f)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t))
              args
              targets
              temps)
    (for-each (lambda (r t)
                (let ((temp (cgreg-lookup-reg regs r)))
                  (if (not (eq? temp t))
                      (begin (gen-load! output frame r t)
                             (cgreg-bind! regs r t)))
                  (cgframe-release! frame t)))
              targets
              temps)))

; For heuristic use only.
; An expression is complicated unless it can probably be evaluated
; without saving and restoring any registers, even if it occurs in
; a non-tail position.

(define (complicated? exp env)
  (case (car exp)
    ((quote)    #f)
    ((lambda)   #t)
    ((set!)     (complicated? (assignment.rhs exp) env))
    ((if)       (or (complicated? (if.test exp) env)
                    (complicated? (if.then exp) env)
                    (complicated? (if.else exp) env)))
    ((begin)    (if (variable? exp)
                    #f
                    (some? (lambda (exp)
                             (complicated? exp env))
                           (begin.exprs exp))))
    (else       (let ((proc (call.proc exp)))
                  (if (and (variable? proc)
                           (let ((entry
                                  (cgenv-lookup env (variable.name proc))))
                             (eq? (entry.kind entry) 'integrable)))
                      (some? (lambda (exp)
                               (complicated? exp env))
                             (call.args exp))
                      #t)))))

; Returns a permutation of the src list, permuted the same way the
; key list was permuted to obtain newkey.

(define (cg-permute src key newkey)
  (let ((alist (map cons key (iota (length key)))))
    (do ((newkey newkey (cdr newkey))
         (dest '()
               (cons (list-ref src (cdr (assq (car newkey) alist)))
                     dest)))
        ((null? newkey) (reverse dest)))))

; Given a list of register numbers,
; an association list with entries of the form (name . regnum) giving
; the variable names by which those registers are known in code,
; and a list of expressions giving new values for those registers,
; returns an ordering of the register assignments that implements a
; parallel assignment if one can be found, otherwise returns #f.

(define parallel-assignment
 (lambda (regnums alist exps)
   (if (null? regnums)
       #t
       (let ((x (toposort (dependency-graph regnums alist exps))))
         (if x (reverse x) #f)))))

(define dependency-graph
 (lambda (regnums alist exps)
   (let ((names (map car alist)))
     (do ((regnums regnums (cdr regnums))
          (exps exps (cdr exps))
          (l '() (cons (cons (car regnums)
                             (map (lambda (var) (cdr (assq var alist)))
                                  (intersection (freevariables (car exps))
                                                names)))
                       l)))
         ((null? regnums) l)))))

; Given a nonempty graph represented as a list of the form
;     ((node1 . <list of nodes that node1 is less than or equal to>)
;      (node2 . <list of nodes that node2 is less than or equal to>)
;      ...)
; returns a topological sort of the nodes if one can be found,
; otherwise returns #f.

(define toposort
 (lambda (graph)
   (cond ((null? (cdr graph)) (list (caar graph)))
         (else (toposort2 graph '())))))

(define toposort2
 (lambda (totry tried)
   (cond ((null? totry) #f)
         ((or (null? (cdr (car totry)))
              (and (null? (cddr (car totry)))
                   (eq? (cadr (car totry))
                        (car (car totry)))))
          (if (and (null? (cdr totry)) (null? tried))
              (list (caar totry))
              (let* ((node (caar totry))
                     (x (toposort2 (map (lambda (y)
                                          (cons (car y) (remove node (cdr y))))
                                        (append (cdr totry) tried))
                                   '())))
                (if x
                    (cons node x)
                    #f))))
         (else (toposort2 (cdr totry) (cons (car totry) tried))))))

(define iota (lambda (n) (iota2 n '())))

(define iota1 (lambda (n) (cdr (iota2 (+ n 1) '()))))

(define iota2
 (lambda (n l)
   (if (zero? n)
       l
       (let ((n (- n 1)))
         (iota2 n (cons n l))))))

(define (freevariables exp)
  (freevars2 exp '()))

(define (freevars2 exp env)
  (cond ((symbol? exp)
         (if (memq exp env) '() (list exp)))
        ((not (pair? exp)) '())
        (else (let ((keyword (car exp)))
                (cond ((eq? keyword 'quote) '())
                      ((eq? keyword 'lambda)
                       (let ((env (append (make-null-terminated (cadr exp))
                                          env)))
                         (apply-union
                          (map (lambda (x) (freevars2 x env))
                               (cddr exp)))))
                      ((memq keyword '(if set! begin))
                       (apply-union
                        (map (lambda (x) (freevars2 x env))
                             (cdr exp))))
                      (else (apply-union
                             (map (lambda (x) (freevars2 x env))
                                  exp))))))))
; Copyright 1991 William Clinger (cg-let and cg-let-body)
; Copyright 1999 William Clinger (everything else)
;
; 10 June 1999.

; Generates code for a let expression.

(define (cg-let output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (vars (lambda.args proc))
         (n (length vars))
         (free (lambda.F proc))
         (live (cgframe-livevars frame)))
    (if (and (null? (lambda.defs proc))
             (= n 1))
        (cg-let1 output exp target regs frame env tail?)
        (let* ((args (call.args exp))
               (temps (newtemps n))
               (alist (map cons temps vars)))
          (for-each (lambda (arg t)
                      (let ((r (choose-register regs frame)))
                        (cg0 output arg r regs frame env #f)
                        (cgreg-bind! regs r t)
                        (gen-store! output frame r t)))
                    args
                    temps)
          (cgreg-rename! regs alist)
          (cgframe-rename! frame alist)
          (cg-let-release! free live regs frame tail?)
          (cg-let-body output proc target regs frame env tail?)))))

; Given the free variables of a let body, and the variables that are
; live after the let expression, and the usual regs, frame, and tail?
; arguments, releases any registers and frame slots that don't need
; to be preserved across the body of the let.

(define (cg-let-release! free live regs frame tail?)
  ; The tail case is easy because there are no live temporaries,
  ; and there are no free variables in the context.
  ; The non-tail case assumes A-normal form.
  (cond (tail?
         (let ((keepers (cons (cgreg-lookup-reg regs 0) free)))
           (cgreg-release-except! regs keepers)
           (cgframe-release-except! frame keepers)))
        (live
         (let ((keepers (cons (cgreg-lookup-reg regs 0)
                              (union live free))))
           (cgreg-release-except! regs keepers)
           (cgframe-release-except! frame keepers)))))

; Generates code for the body of a let.

(define (cg-let-body output L target regs frame env tail?)
  (let ((vars (lambda.args L))
        (free (lambda.F L))
        (live (cgframe-livevars frame)))
    (let ((r (cg-body output L target regs frame env tail?)))
      (for-each (lambda (v)
                  (let ((entry (cgreg-lookup regs v)))
                    (if entry
                        (cgreg-release! regs (entry.regnum entry)))
                    (cgframe-release! frame v)))
                vars)
      (if (and (not target)
               (not (eq? r 'result))
               (not (cgreg-lookup-reg regs r)))
          (cg-move output frame regs r 'result)
          r))))

; Generates code for a let expression that binds exactly one variable
; and has no internal definitions.  These let expressions are very
; common in A-normal form, and there are many special cases with
; respect to register allocation and order of evaluation.

(define (cg-let1 output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (v (car (lambda.args proc)))
         (arg (car (call.args exp)))
         (free (lambda.F proc))
         (live (cgframe-livevars frame))
         (body (lambda.body proc)))
    
    (define (evaluate-into-register r)
      (cg0 output arg r regs frame env #f)
      (cgreg-bind! regs r v)
      (gen-store! output frame r v)
      r)
    
    (define (release-registers!)
      (cgframe-livevars-set! frame live)
      (cg-let-release! free live regs frame tail?))
    
    (define (finish)
      (release-registers!)
      (cg-let-body output proc target regs frame env tail?))
    
    (if live
        (cgframe-livevars-set! frame (union live free)))
    
    (cond ((assq v *regnames*)
           (evaluate-into-register (cdr (assq v *regnames*)))
           (finish))
          ((not (memq v free))
           (cg0 output arg #f regs frame env #f)
           (finish))
          (live
           (cg0 output arg 'result regs frame env #f)
           (release-registers!)
           (cg-let1-result output exp target regs frame env tail?))
          (else
           (evaluate-into-register (choose-register regs frame))
           (finish)))))

; Given a let expression that binds one variable whose value has already
; been evaluated into the result register, generates code for the rest
; of the let expression.
; The main difficulty is an unfortunate interaction between A-normal
; form and the MacScheme machine architecture:  We don't want to move
; a value from the result register into a general register if it has
; only one use and can remain in the result register until that use.

(define (cg-let1-result output exp target regs frame env tail?)
  (let* ((proc (call.proc exp))
         (v (car (lambda.args proc)))
         (free (lambda.F proc))
         (live (cgframe-livevars frame))
         (body (lambda.body proc))
         (pattern (cg-let-used-once v body)))
    
    (define (move-to-register r)
      (gen! output $setreg r)
      (cgreg-bind! regs r v)
      (gen-store! output frame r v)
      r)
    
    (define (release-registers!)
      (cgframe-livevars-set! frame live)
      (cg-let-release! free live regs frame tail?))
    
    ; FIXME: The live variables must be correct in the frame.
    
    (case pattern
      ((if)
       (cg-if-result output body target regs frame env tail?))
      ((let-if)
       (if live
           (cgframe-livevars-set! frame (union live free)))
       (cg-if-result output
                     (car (call.args body))
                     'result regs frame env #f)
       (release-registers!)
       (cg-let1-result output body target regs frame env tail?))
      ((set!)
       (cg-assignment-result output
                             body target regs frame env tail?))
      ((let-set!)
       (cg-assignment-result output
                             (car (call.args body))
                             'result regs frame env #f)
       (cg-let1-result output body target regs frame env tail?))
      ((primop)
       (cg-primop-result output body target regs frame env tail?))
      ((let-primop)
       (cg-primop-result output
                         (car (call.args body))
                         'result regs frame env #f)
       (cg-let1-result output body target regs frame env tail?))
      ; FIXME
      ((_called)
       (cg-call-result output body target regs frame env tail?))
      ; FIXME
      ((_let-called)
       (cg-call-result output
                       (car (call.args body))
                       'result regs frame env #f)
       (cg-let1-result output body target regs frame env tail?))
      (else
       ; FIXME:  The first case was handled by cg-let1.
       (cond ((assq v *regnames*)
              (move-to-register (cdr (assq v *regnames*))))
             ((memq v free)
              (move-to-register (choose-register regs frame))))
       (cg-let-body output proc target regs frame env tail?)))))

; Given a call to a primop whose first argument has already been
; evaluated into the result register and whose remaining arguments
; consist of constants and variable references, generates code for
; the call.

(define (cg-primop-result output exp target regs frame env tail?)
  (let ((args (call.args exp))
        (entry (var-lookup (variable.name (call.proc exp)) regs frame env)))
    (if (= (entry.arity entry) (length args))
        (begin (case (entry.arity entry)
                 ((0) (gen! output $op1 (entry.op entry)))
                 ((1) (gen! output $op1 (entry.op entry)))
                 ((2) (cg-primop2-result! output entry args regs frame env))
                 ((3) (let ((rs (cg-result-args output args regs frame env)))
                        (gen! output
                              $op3 (entry.op entry) (car rs) (cadr rs))))
                 (else (error "Bug detected by cg-primop-result"
                              (make-readable exp))))
               (if tail?
                   (begin (gen-pop! output frame)
                          (gen! output $return)
                          'result)
                   (cg-move output frame regs 'result target)))
        (if (negative? (entry.arity entry))
            (cg-special-result output exp target regs frame env tail?)
            (error "Wrong number of arguments to integrable procedure"
                   (make-readable exp))))))

(define (cg-primop2-result! output entry args regs frame env)
  (let ((op (entry.op entry))
        (arg2 (cadr args)))
    (if (and (constant? arg2)
             (entry.imm entry)
             ((entry.imm entry) (constant.value arg2)))
        (gen! output $op2imm op (constant.value arg2))
        (let ((rs (cg-result-args output args regs frame env)))
          (gen! output $op2 op (car rs))))))

; Given a short list of constants and variable references to be evaluated
; into arbitrary general registers, evaluates them into registers without
; disturbing the result register and returns a list of the registers into
; which they are evaluated.  Before returning, any registers that were
; allocated by this routine are released.

(define (cg-result-args output args regs frame env)
  
  ; Given a list of unevaluated arguments,
  ; a longer list of disjoint general registers,
  ; the register that holds the first evaluated argument,
  ; a list of registers in reverse order that hold other arguments,
  ; and a list of registers to be released afterwards,
  ; generates code to evaluate the arguments,
  ; deallocates any registers that were evaluated to hold the arguments,
  ; and returns the list of registers that contain the arguments.
  
  (define (loop args registers rr rs temps)
    (if (null? args)
        (begin (if (not (eq? rr 'result))
                   (gen! output $reg rr))
               (for-each (lambda (r) (cgreg-release! regs r))
                         temps)
               (reverse rs))
        (let ((arg (car args)))
          (cond ((constant? arg)
                 (let ((r (car registers)))
                   (gen! output $const/setreg (constant.value arg) r)
                   (cgreg-bind! regs r #t)
                   (loop (cdr args)
                         (cdr registers)
                         rr
                         (cons r rs)
                         (cons r temps))))
                ((variable? arg)
                 (let* ((id (variable.name arg))
                        (entry (var-lookup id regs frame env)))
                   (case (entry.kind entry)
                     ((global integrable)
                      (if (eq? rr 'result)
                          (save-result! args registers rr rs temps)
                          (let ((r (car registers)))
                            (gen! output $global id)
                            (gen! output $setreg r)
                            (cgreg-bind! regs r id)
                            (loop (cdr args)
                                  (cdr registers)
                                  rr
                                  (cons r rs)
                                  (cons r temps)))))
                     ((lexical)
                      (if (eq? rr 'result)
                          (save-result! args registers rr rs temps)
                          (let ((m (entry.rib entry))
                                (n (entry.offset entry))
                                (r (car registers)))
                            (gen! output $lexical m n id)
                            (gen! output $setreg r)
                            (cgreg-bind! regs r id)
                            (loop (cdr args)
                                  (cdr registers)
                                  rr
                                  (cons r rs)
                                  (cons r temps)))))
                     ((procedure) (error "Bug in cg-variable" arg))
                     ((register)
                      (let ((r (entry.regnum entry)))
                        (loop (cdr args)
                              registers
                              rr
                              (cons r rs)
                              temps)))
                     ((frame)
                      (let ((r (car registers)))
                        (gen-load! output frame r id)
                        (cgreg-bind! regs r id)
                        (loop (cdr args)
                              (cdr registers)
                              rr
                              (cons r rs)
                              (cons r temps))))
                     (else (error "Bug in cg-result-args" arg)))))
                (else
                 (error "Bug in cg-result-args"))))))
  
  (define (save-result! args registers rr rs temps)
    (let ((r (car registers)))
      (gen! output $setreg r)
      (loop args
            (cdr registers)
            r
            rs
            temps)))
  
  (loop (cdr args)
        (choose-registers regs frame (length args))
        'result '() '()))

; Given a local variable T1 and an expression in A-normal form,
; cg-let-used-once returns a symbol if the local variable is used
; exactly once in the expression and the expression matches one of
; the patterns below.  Otherwise returns #f.  The symbol that is
; returned is the name of the pattern that is matched.
;
;     pattern                         symbol returned
; 
;     (if T1 ... ...)                 if
; 
;     (<primop> T1 ...)               primop
; 
;     (T1 ...)                        called
; 
;     (set! ... T1)                   set!
; 
;     (let ((T2 (if T1 ... ...)))     let-if
;       E3)
; 
;     (let ((T2 (<primop> T1 ...)))   let-primop
;       E3)
; 
;     (let ((T2 (T1 ...)))            let-called
;       E3)
; 
;     (let ((T2 (set! ... T1)))       let-set!
;       E3)
;
; This implementation sometimes returns #f incorrectly, but it always
; returns an answer in constant time (assuming A-normal form).

(define (cg-let-used-once T1 exp)
  (define budget 20)
  (define (cg-let-used-once T1 exp)
    (define (used? T1 exp)
      (set! budget (- budget 1))
      (cond ((negative? budget) #t)
            ((constant? exp) #f)
            ((variable? exp)
             (eq? T1 (variable.name exp)))
            ((lambda? exp)
             (memq T1 (lambda.F exp)))
            ((assignment? exp)
             (used? T1 (assignment.rhs exp)))
            ((call? exp)
             (or (used? T1 (call.proc exp))
                 (used-in-args? T1 (call.args exp))))
            ((conditional? exp)
             (or (used? T1 (if.test exp))
                 (used? T1 (if.then exp))
                 (used? T1 (if.else exp))))
            (else #t)))
    (define (used-in-args? T1 args)
      (if (null? args)
          #f
          (or (used? T1 (car args))
              (used-in-args? T1 (cdr args)))))
    (set! budget (- budget 1))
    (cond ((negative? budget) #f)
          ((call? exp)
           (let ((proc (call.proc exp))
                 (args (call.args exp)))
             (cond ((variable? proc)
                    (let ((f (variable.name proc)))
                      (cond ((eq? f T1)
                             (and (not (used-in-args? T1 args))
                                  'called))
                            ((and (integrable? f)
                                  (not (null? args))
                                  (variable? (car args))
                                  (eq? T1 (variable.name (car args))))
                             (and (not (used-in-args? T1 (cdr args)))
                                  'primop))
                            (else #f))))
                   ((lambda? proc)
                    (and (not (memq T1 (lambda.F proc)))
                         (not (null? args))
                         (null? (cdr args))
                         (case (cg-let-used-once T1 (car args))
                           ((if)       'let-if)
                           ((primop)   'let-primop)
                           ((called)   'let-called)
                           ((set!)     'let-set!)
                           (else       #f))))
                   (else #f))))
          ((conditional? exp)
           (let ((E0 (if.test exp)))
             (and (variable? E0)
                  (eq? T1 (variable.name E0))
                  (not (used? T1 (if.then exp)))
                  (not (used? T1 (if.else exp)))
                  'if)))
          ((assignment? exp)
           (let ((rhs (assignment.rhs exp)))
             (and (variable? rhs)
                  (eq? T1 (variable.name rhs))
                  'set!)))
          (else #f)))
  (cg-let-used-once T1 exp))

; Given the name of a let-body pattern, an expression that matches that
; pattern, and an expression to be substituted for the let variable,
; returns the transformed expression.

; FIXME: No longer used.

(define (cg-let-transform pattern exp E1)
  (case pattern
    ((if)
     (make-conditional E1 (if.then exp) (if.else exp)))
    ((primop)
     (make-call (call.proc exp)
                (cons E1 (cdr (call.args exp)))))
    ((called)
     (make-call E1 (call.args exp)))
    ((set!)
     (make-assignment (assignment.lhs exp) E1))
    ((let-if let-primop let-called let-set!)
     (make-call (call.proc exp)
                (list (cg-let-transform (case pattern
                                          ((let-if)     'if)
                                          ((let-primop) 'primop)
                                          ((let-called) 'called)
                                          ((let-set!)   'set!))
                                        (car (call.args exp))
                                        E1))))
    (else
     (error "Unrecognized pattern in cg-let-transform" pattern)))); Copyright 1999 William Clinger
;
; Code for special primitives, used to generate runtime safety checks,
; efficient code for call-with-values, and other weird things.
;
; 4 June 1999.

(define (cg-special output exp target regs frame env tail?)
  (let ((name (variable.name (call.proc exp))))
    (cond ((eq? name name:CHECK!)
           (if (runtime-safety-checking)
               (cg-check output exp target regs frame env tail?)))
          (else
           (error "Compiler bug: cg-special" (make-readable exp))))))

(define (cg-special-result output exp target regs frame env tail?)
  (let ((name (variable.name (call.proc exp))))
    (cond ((eq? name name:CHECK!)
           (if (runtime-safety-checking)
               (cg-check-result output exp target regs frame env tail?)))
          (else
           (error "Compiler bug: cg-special" (make-readable exp))))))

(define (cg-check output exp target regs frame env tail?)
  (cg0 output (car (call.args exp)) 'result regs frame env #f)
  (cg-check-result output exp target regs frame env tail?))

(define (cg-check-result output exp target regs frame env tail?)
  (let* ((args (call.args exp))
         (nargs (length args))
         (valexps (cddr args)))
    (if (and (<= 2 nargs 5)
             (constant? (cadr args))
             (every? (lambda (exp)
                       (or (constant? exp)
                           (variable? exp)))
                     valexps))
        (let* ((exn (constant.value (cadr args)))
               (vars (filter variable? valexps))
               (rs (cg-result-args output
                                   (cons (car args) vars)
                                   regs frame env)))
          
          ; Construct the trap situation:
          ; the exception number followed by an ordered list of
          ; register numbers and constant expressions.
          
          (let loop ((registers rs)
                     (exps valexps)
                     (operands '()))
            (cond ((null? exps)
                   (let* ((situation (cons exn (reverse operands)))
                          (ht (assembly-stream-info output))
                          (L1 (or (hashtable-get ht situation)
                                  (let ((L1 (make-label)))
                                    (hashtable-put! ht situation L1)
                                    L1))))
                     (define (translate r)
                       (if (number? r) r 0))
                     (case (length operands)
                       ((0) (gen! output $check 0 0 0 L1))
                       ((1) (gen! output $check
                                         (translate (car operands))
                                         0 0 L1))
                       ((2) (gen! output $check
                                         (translate (car operands))
                                         (translate (cadr operands))
                                         0 L1))
                       ((3) (gen! output $check
                                         (translate (car operands))
                                         (translate (cadr operands))
                                         (translate (caddr operands))
                                         L1)))))
                  ((constant? (car exps))
                   (loop registers
                         (cdr exps)
                         (cons (car exps) operands)))
                  (else
                   (loop (cdr registers)
                         (cdr exps)
                         (cons (car registers) operands))))))
        (error "Compiler bug: runtime check" (make-readable exp)))))

; Given an assembly stream and the description of a trap as recorded
; by cg-check above, generates a non-continuable trap at that label for
; that trap, passing the operands to the exception handler.

(define (cg-trap output situation L1)
  (let* ((exn (car situation))
         (operands (cdr situation)))
    (gen! output $.label L1)
    (let ((liveregs (filter number? operands)))
      (define (loop operands registers r)
        (cond ((null? operands)
               (case (length registers)
                 ((0) (gen! output $trap 0 0 0 exn))
                 ((1) (gen! output $trap (car registers) 0 0 exn))
                 ((2) (gen! output $trap
                                   (car registers)
                                   (cadr registers)
                                   0
                                   exn))
                 ((3) (gen! output $trap
                                   (car registers)
                                   (cadr registers)
                                   (caddr registers)
                                   exn))
                 (else "Compiler bug: trap")))
              ((number? (car operands))
               (loop (cdr operands)
                     (cons (car operands) registers)
                     r))
              ((memv r liveregs)
               (loop operands registers (+ r 1)))
              (else
               (gen! output $const (constant.value (car operands)))
               (gen! output $setreg r)
               (loop (cdr operands)
                     (cons r registers)
                     (+ r 1)))))
      (loop (reverse operands) '() 1))))

; Given a short list of expressions that can be evaluated in any order,
; evaluates the first into the result register and the others into any
; register, and returns an ordered list of the registers that contain
; the arguments that follow the first.
; The number of expressions must be less than the number of argument
; registers.

; FIXME: No longer used.

(define (cg-check-args output args regs frame env)
  
  ; Given a list of expressions to evaluate, a list of variables
  ; and temporary names for arguments that have already been
  ; evaluated, in reverse order, and a mask of booleans that
  ; indicate which temporaries should be released before returning,
  ; returns the correct result.
  
  (define (eval-loop args temps mask)
    (if (null? args)
        (eval-first-into-result temps mask)
        (let ((reg (cg0 output (car args) #f regs frame env #f)))
          (if (eq? reg 'result)
              (let* ((r (choose-register regs frame))
                     (t (newtemp)))
                (gen! output $setreg r)
                (cgreg-bind! regs r t)
                (gen-store! output frame r t)
                (eval-loop (cdr args)
                           (cons t temps)
                           (cons #t mask)))
              (eval-loop (cdr args)
                         (cons (cgreg-lookup-reg regs reg) temps)
                         (cons #f mask))))))
  
  (define (eval-first-into-result temps mask)
    (cg0 output (car args) 'result regs frame env #f)
    (finish-loop (choose-registers regs frame (length temps))
                 temps
                 mask
                 '()))
  
  ; Given a sufficient number of disjoint registers, a list of
  ; variable and temporary names that may need to be loaded into
  ; registers, a mask of booleans that indicates which temporaries
  ; should be released, and a list of registers in forward order,
  ; returns the correct result.
  
  (define (finish-loop disjoint temps mask registers)
    (if (null? temps)
        registers
        (let* ((t (car temps))
               (entry (cgreg-lookup regs t)))
          (if entry
              (let ((r (entry.regnum entry)))
                (if (car mask)
                    (begin (cgreg-release! regs r)
                           (cgframe-release! frame t)))
                (finish-loop disjoint
                             (cdr temps)
                             (cdr mask)
                             (cons r registers)))
              (let ((r (car disjoint)))
                (if (memv r registers)
                    (finish-loop (cdr disjoint) temps mask registers)
                    (begin (gen-load! output frame r t)
                           (cgreg-bind! regs r t)
                           (if (car mask)
                               (begin (cgreg-release! regs r)
                                      (cgframe-release! frame t)))
                           (finish-loop disjoint
                                        (cdr temps)
                                        (cdr mask)
                                        (cons r registers)))))))))
  
  (if (< (length args) *nregs*)
      (eval-loop (cdr args) '() '())
      (error "Bug detected by cg-primop-args" args)))
; Copyright 1998 William Clinger.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 5 June 1999.
;
; Local optimizations for MacScheme machine assembly code.
;
; Branch tensioning.
; Suppress nop instructions.
; Suppress save, restore, and pop instructions whose operand is -1.
; Suppress redundant stores.
; Suppress definitions (primarily loads) of dead registers.
;
; Note:  Twobit never generates a locally redundant load or store,
; so this code must be tested with a different code generator.
;
; To perform these optimizations, the basic block must be traversed
; both forwards and backwards.
; The forward traversal keeps track of registers that were defined
; by a load.
; The backward traversal keeps track of live registers.

(define filter-basic-blocks
  
  (let* ((suppression-message
          "Local optimization detected a useless instruction.")
         
         ; Each instruction is mapping to an encoding of the actions
         ; to be performed when it is encountered during the forward
         ; or backward traversal.
         
         (forward:normal                   0)
         (forward:nop                      1)
         (forward:ends-block               2)
         (forward:interesting              3)
         (forward:kills-all-registers      4)
         (forward:nop-if-arg1-is-negative  5)
         
         (backward:normal                  0)
         (backward:ends-block              1)
         (backward:begins-block            2)
         (backward:uses-arg1               4)
         (backward:uses-arg2               8)
         (backward:uses-arg3              16)
         (backward:kills-arg1             32)
         (backward:kills-arg2             64)
         (backward:uses-many             128)
         
         ; largest mnemonic + 1
         
         (dispatch-table-size *number-of-mnemonics*)
         
         ; Dispatch table for the forwards traversal.
         
         (forward-table (make-bytevector dispatch-table-size))
         
         ; Dispatch table for the backwards traversal.
         
         (backward-table (make-bytevector dispatch-table-size)))
    
    (do ((i 0 (+ i 1)))
        ((= i dispatch-table-size))
        (bytevector-set! forward-table i forward:normal)
        (bytevector-set! backward-table i backward:normal))
    
    (bytevector-set! forward-table $nop     forward:nop)
    
    (bytevector-set! forward-table $invoke  forward:ends-block)
    (bytevector-set! forward-table $return  forward:ends-block)
    (bytevector-set! forward-table $skip    forward:ends-block)
    (bytevector-set! forward-table $branch  forward:ends-block)
    (bytevector-set! forward-table $branchf forward:ends-block)
    (bytevector-set! forward-table $jump    forward:ends-block)
    (bytevector-set! forward-table $.align  forward:ends-block)
    (bytevector-set! forward-table $.proc   forward:ends-block)
    (bytevector-set! forward-table $.cont   forward:ends-block)
    (bytevector-set! forward-table $.label  forward:ends-block)
    
    (bytevector-set! forward-table $store   forward:interesting)
    (bytevector-set! forward-table $load    forward:interesting)
    (bytevector-set! forward-table $setstk  forward:interesting)
    (bytevector-set! forward-table $setreg  forward:interesting)
    (bytevector-set! forward-table $movereg forward:interesting)
    (bytevector-set! forward-table $const/setreg
                                            forward:interesting)
    
    (bytevector-set! forward-table $args>=  forward:kills-all-registers)
    (bytevector-set! forward-table $popstk  forward:kills-all-registers)
    
    ; These instructions also kill all registers.
    
    (bytevector-set! forward-table $save    forward:nop-if-arg1-is-negative)
    (bytevector-set! forward-table $restore forward:nop-if-arg1-is-negative)
    (bytevector-set! forward-table $pop     forward:nop-if-arg1-is-negative)
  
    (bytevector-set! backward-table $invoke  backward:ends-block)
    (bytevector-set! backward-table $return  backward:ends-block)
    (bytevector-set! backward-table $skip    backward:ends-block)
    (bytevector-set! backward-table $branch  backward:ends-block)
    (bytevector-set! backward-table $branchf backward:ends-block)
    
    (bytevector-set! backward-table $jump    backward:begins-block) ; [sic]
    (bytevector-set! backward-table $.align  backward:begins-block)
    (bytevector-set! backward-table $.proc   backward:begins-block)
    (bytevector-set! backward-table $.cont   backward:begins-block)
    (bytevector-set! backward-table $.label  backward:begins-block)
    
    (bytevector-set! backward-table $op2     backward:uses-arg2)
    (bytevector-set! backward-table $op3     (logior backward:uses-arg2
                                                     backward:uses-arg3))
    (bytevector-set! backward-table $check   (logior
                                              backward:uses-arg1
                                              (logior backward:uses-arg2
                                                      backward:uses-arg3)))
    (bytevector-set! backward-table $trap    (logior
                                              backward:uses-arg1
                                              (logior backward:uses-arg2
                                                      backward:uses-arg3)))
    (bytevector-set! backward-table $store   backward:uses-arg1)
    (bytevector-set! backward-table $reg     backward:uses-arg1)
    (bytevector-set! backward-table $load    backward:kills-arg1)
    (bytevector-set! backward-table $setreg  backward:kills-arg1)
    (bytevector-set! backward-table $movereg (logior backward:uses-arg1
                                                     backward:kills-arg2))
    (bytevector-set! backward-table $const/setreg
                                             backward:kills-arg2)
    (bytevector-set! backward-table $lambda  backward:uses-many)
    (bytevector-set! backward-table $lexes   backward:uses-many)
    (bytevector-set! backward-table $args>=  backward:uses-many)
    
    (lambda (instructions)
      
      (let* ((*nregs* *nregs*) ; locals might be faster than globals
             
             ; During the forwards traversal:
             ;    (vector-ref registers i) = #f
             ;        means the content of register i is unknown
             ;    (vector-ref registers i) = j
             ;        means register was defined by load i,j
             ;
             ; During the backwards traversal:
             ;    (vector-ref registers i) = #f means register i is dead
             ;    (vector-ref registers i) = #t means register i is live
             
             (registers (make-vector *nregs* #f))
             
             ; During the forwards traversal, the label of a block that
             ; falls through into another block or consists of a skip
             ; to another block is mapped to another label.
             ; This mapping is implemented by a hash table.
             ; Before the backwards traversal, the transitive closure
             ; is computed.  The graph has no cycles, and the maximum
             ; out-degree is 1, so this is easy.
             
             (label-table (make-hashtable (lambda (n) n) assv)))
        
        (define (compute-transitive-closure!)
          (define (lookup x)
            (let ((y (hashtable-get label-table x)))
              (if y
                  (lookup y)
                  x)))
          (hashtable-for-each (lambda (x y)
                                (hashtable-put! label-table x (lookup y)))
                              label-table))
        
        ; Don't use this procedure until the preceding procedure
        ; has been called.
        
        (define (lookup-label x)
          (hashtable-fetch label-table x x))
        
        (define (vector-fill! v x)
          (subvector-fill! v 0 (vector-length v) x))
        
        (define (subvector-fill! v i j x)
          (if (< i j)
              (begin (vector-set! v i x)
                     (subvector-fill! v (+ i 1) j x))))
        
        (define (kill-stack! j)
          (do ((i 0 (+ i 1)))
              ((= i *nregs*))
              (let ((x (vector-ref registers i)))
                (if (and x (= x j))
                    (vector-set! registers i #f)))))
        
        ; Dispatch procedure for the forwards traversal.
        
        (define (forwards instructions filtered)
          (if (null? instructions)
              (begin (vector-fill! registers #f)
                     (vector-set! registers 0 #t)
                     (compute-transitive-closure!)
                     (backwards0 filtered '()))
              (let* ((instruction (car instructions))
                     (instructions (cdr instructions))
                     (op (instruction.op instruction))
                     (flags (bytevector-ref forward-table op)))
                (cond ((eqv? flags forward:normal)
                       (forwards instructions (cons instruction filtered)))
                      ((eqv? flags forward:nop)
                       (forwards instructions filtered))
                      ((eqv? flags forward:nop-if-arg1-is-negative)
                       (if (negative? (instruction.arg1 instruction))
                           (forwards instructions filtered)
                           (begin (vector-fill! registers #f)
                                  (forwards instructions
                                            (cons instruction filtered)))))
                      ((eqv? flags forward:kills-all-registers)
                       (vector-fill! registers #f)
                       (forwards instructions
                                 (cons instruction filtered)))
                      ((eqv? flags forward:ends-block)
                       (vector-fill! registers #f)
                       (if (eqv? op $.label)
                           (forwards-label instruction
                                           instructions
                                           filtered)
                           (forwards instructions
                                     (cons instruction filtered))))
                      ((eqv? flags forward:interesting)
                       (cond ((eqv? op $setreg)
                              (vector-set! registers
                                           (instruction.arg1 instruction)
                                           #f)
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $const/setreg)
                              (vector-set! registers
                                           (instruction.arg2 instruction)
                                           #f)
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $movereg)
                              (vector-set! registers
                                           (instruction.arg2 instruction)
                                           #f)
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $setstk)
                              (kill-stack! (instruction.arg1 instruction))
                              (forwards instructions
                                        (cons instruction filtered)))
                             ((eqv? op $load)
                              (let ((i (instruction.arg1 instruction))
                                    (j (instruction.arg2 instruction)))
                                (if (eqv? (vector-ref registers i) j)
                                    ; Suppress redundant load.
                                    ; Should never happen with Twobit.
                                    (suppress-forwards instruction
                                                       instructions
                                                       filtered)
                                    (begin (vector-set! registers i j)
                                           (forwards instructions
                                                     (cons instruction
                                                           filtered))))))
                             ((eqv? op $store)
                              (let ((i (instruction.arg1 instruction))
                                    (j (instruction.arg2 instruction)))
                                (if (eqv? (vector-ref registers i) j)
                                    ; Suppress redundant store.
                                    ; Should never happen with Twobit.
                                    (suppress-forwards instruction
                                                       instructions
                                                       filtered)
                                    (begin (kill-stack! j)
                                           (forwards instructions
                                                     (cons instruction
                                                           filtered))))))
                             (else
                              (local-optimization-error op))))
                      (else
                       (local-optimization-error op))))))
        
        ; Enters labels into a table for branch tensioning.
        
        (define (forwards-label instruction1 instructions filtered)
          (let ((label1 (instruction.arg1 instruction1)))
            (if (null? instructions)
                ; This is ok provided the label is unreachable.
                (forwards instructions (cdr filtered))
                (let loop ((instructions instructions)
                           (filtered (cons instruction1 filtered)))
                  (let* ((instruction (car instructions))
                         (op (instruction.op instruction))
                         (flags (bytevector-ref forward-table op)))
                    (cond ((eqv? flags forward:nop)
                           (loop (cdr instructions) filtered))
                          ((and (eqv? flags forward:nop-if-arg1-is-negative)
                                (negative? (instruction.arg1 instruction)))
                           (loop (cdr instructions) filtered))
                          ((eqv? op $.label)
                           (let ((label2 (instruction.arg1 instruction)))
                             (hashtable-put! label-table label1 label2)
                             (forwards-label instruction
                                             (cdr instructions)
                                             (cdr filtered))))
                          ((eqv? op $skip)
                           (let ((label2 (instruction.arg1 instruction)))
                             (hashtable-put! label-table label1 label2)
                             ; We can't get rid of the skip instruction
                             ; because control might fall into this block,
                             ; but we can get rid of the label.
                             (forwards instructions (cdr filtered))))
                          (else
                           (forwards instructions filtered))))))))
        
        ; Dispatch procedure for the backwards traversal.
        
        (define (backwards instructions filtered)
          (if (null? instructions)
              filtered
              (let* ((instruction (car instructions))
                     (instructions (cdr instructions))
                     (op (instruction.op instruction))
                     (flags (bytevector-ref backward-table op)))
                (cond ((eqv? flags backward:normal)
                       (backwards instructions (cons instruction filtered)))
                      ((eqv? flags backward:ends-block)
                       (backwards0 (cons instruction instructions)
                                   filtered))
                      ((eqv? flags backward:begins-block)
                       (backwards0 instructions
                                   (cons instruction filtered)))
                      ((eqv? flags backward:uses-many)
                       (cond ((or (eqv? op $lambda)
                                  (eqv? op $lexes))
                              (let ((live
                                     (if (eqv? op $lexes)
                                         (instruction.arg1 instruction)
                                         (instruction.arg2 instruction))))
                                (subvector-fill! registers
                                                 0
                                                 (min *nregs* (+ 1 live))
                                                 #t)
                                (backwards instructions
                                           (cons instruction filtered))))
                             ((eqv? op $args>=)
                              (vector-fill! registers #t)
                              (backwards instructions
                                         (cons instruction filtered)))
                             (else
                              (local-optimization-error op))))
                      ((and (eqv? (logand flags backward:kills-arg1)
                                  backward:kills-arg1)
                            (not (vector-ref registers
                                             (instruction.arg1 instruction))))
                       ; Suppress initialization of dead register.
                       (suppress-backwards instruction
                                           instructions
                                           filtered))
                      ((and (eqv? (logand flags backward:kills-arg2)
                                  backward:kills-arg2)
                            (not (vector-ref registers
                                             (instruction.arg2 instruction))))
                       ; Suppress initialization of dead register.
                       (suppress-backwards instruction
                                           instructions
                                           filtered))
                      ((and (eqv? op $movereg)
                            (= (instruction.arg1 instruction)
                               (instruction.arg2 instruction)))
                       (backwards instructions filtered))
                      (else
                       (let ((filtered (cons instruction filtered)))
                         (if (eqv? (logand flags backward:kills-arg1)
                                   backward:kills-arg1)
                             (vector-set! registers
                                          (instruction.arg1 instruction)
                                          #f))
                         (if (eqv? (logand flags backward:kills-arg2)
                                   backward:kills-arg2)
                             (vector-set! registers
                                          (instruction.arg2 instruction)
                                          #f))
                         (if (eqv? (logand flags backward:uses-arg1)
                                   backward:uses-arg1)
                             (vector-set! registers
                                          (instruction.arg1 instruction)
                                          #t))
                         (if (eqv? (logand flags backward:uses-arg2)
                                   backward:uses-arg2)
                             (vector-set! registers
                                          (instruction.arg2 instruction)
                                          #t))
                         (if (eqv? (logand flags backward:uses-arg3)
                                   backward:uses-arg3)
                             (vector-set! registers
                                          (instruction.arg3 instruction)
                                          #t))
                         (backwards instructions filtered)))))))
        
        ; Given a list of instructions in reverse order, whose first
        ; element is the last instruction of a basic block,
        ; and a filtered list of instructions in forward order,
        ; returns a filtered list of instructions in the correct order.
        
        (define (backwards0 instructions filtered)
          (if (null? instructions)
              filtered
              (let* ((instruction (car instructions))
                     (mnemonic (instruction.op instruction)))
                (cond ((or (eqv? mnemonic $.label)
                           (eqv? mnemonic $.proc)
                           (eqv? mnemonic $.cont)
                           (eqv? mnemonic $.align))
                       (backwards0 (cdr instructions)
                                   (cons instruction filtered)))
                      ; all registers are dead at a $return
                      ((eqv? mnemonic $return)
                       (vector-fill! registers #f)
                       (vector-set! registers 0 #t)
                       (backwards (cdr instructions)
                                  (cons instruction filtered)))
                      ; all but the argument registers are dead at an $invoke
                      ((eqv? mnemonic $invoke)
                       (let ((n+1 (min *nregs*
                                       (+ (instruction.arg1 instruction) 1))))
                         (subvector-fill! registers 0 n+1 #t)
                         (subvector-fill! registers n+1 *nregs* #f)
                         (backwards (cdr instructions)
                                    (cons instruction filtered))))
                      ; the compiler says which registers are live at the
                      ; target of $skip, $branch, $branchf, or $jump
                      ((or (eqv? mnemonic $skip)
                           (eqv? mnemonic $branch))
                       (let* ((live (instruction.arg2 instruction))
                              (n+1 (min *nregs* (+ live 1))))
                         (subvector-fill! registers 0 n+1 #t)
                         (subvector-fill! registers n+1 *nregs* #f)
                         (let ((instruction
                                ; FIXME
                                (list mnemonic
                                      (lookup-label
                                       (instruction.arg1 instruction))
                                      live)))
                           (backwards (cdr instructions)
                                      (cons instruction filtered)))))
                      ((eqv? mnemonic $jump)
                       (let ((n+1 (min *nregs*
                                       (+ (instruction.arg3 instruction) 1))))
                         (subvector-fill! registers 0 n+1 #t)
                         (subvector-fill! registers n+1 *nregs* #f)
                         (backwards (cdr instructions)
                                    (cons instruction filtered))))
                      ; the live registers at the target of a $branchf must be
                      ; combined with the live registers at the $branchf
                      ((eqv? mnemonic $branchf)
                       (let* ((live (instruction.arg2 instruction))
                              (n+1 (min *nregs* (+ live 1))))
                         (subvector-fill! registers 0 n+1 #t)
                         (let ((instruction
                                ; FIXME
                                (list mnemonic
                                      (lookup-label
                                       (instruction.arg1 instruction))
                                      live)))
                           (backwards (cdr instructions)
                                      (cons instruction filtered)))))
                      (else (backwards instructions filtered))))))
        
        (define (suppress-forwards instruction instructions filtered)
          (if (issue-warnings)
              '(begin (display suppression-message)
                      (newline)))
          (forwards instructions filtered))
        
        (define (suppress-backwards instruction instructions filtered)
          (if (issue-warnings)
              '(begin (display suppression-message)
                      (newline)))
          (backwards instructions filtered))
        
        (define (local-optimization-error op)
          (error "Compiler bug: local optimization" op))
        
        (vector-fill! registers #f)
        (forwards instructions '())))))
; Copyright 1998 Lars T Hansen.
; 
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 28 April 1999
;
; compile313 -- compilation parameters and driver procedures.


; File types -- these may differ between operating systems.

(define *scheme-file-types* '(".sch" ".scm"))
(define *lap-file-type*     ".lap")
(define *mal-file-type*     ".mal")
(define *lop-file-type*     ".lop")
(define *fasl-file-type*    ".fasl")

; Compile and assemble a scheme source file and produce a fastload file.

(define (compile-file infilename . rest)

  (define (doit)
    (let ((outfilename
           (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename
                                  *scheme-file-types*
                                  *fasl-file-type*)))
          (user
           (assembly-user-data)))
      (if (and (not (integrate-usual-procedures))
               (issue-warnings))
          (begin 
            (display "WARNING from compiler: ")
            (display "integrate-usual-procedures is turned off")
            (newline)
            (display "Performance is likely to be poor.")
            (newline)))
      (if (benchmark-block-mode)
          (process-file-block infilename
                              (list outfilename 'binary)
                              dump-fasl-segment-to-port
                              (lambda (forms)
                                (assemble (compile-block forms) user)))
          (process-file infilename
                        (list outfilename 'binary)
                        dump-fasl-segment-to-port
                        (lambda (expr)
                          (assemble (compile expr) user))))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Compile-file not supported on this target architecture.")
      (doit)))


; Assemble a MAL or LOP file and produce a FASL file.

(define (assemble-file infilename . rest)
  (define (doit)
    (let ((outfilename
           (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename 
                                  (list *lap-file-type* *mal-file-type*)
                                  *fasl-file-type*)))
          (malfile?
           (file-type=? infilename *mal-file-type*))
          (user
           (assembly-user-data)))
      (process-file infilename
                    (list outfilename 'binary)
                    dump-fasl-segment-to-port
                    (lambda (x) (assemble (if malfile? (eval x) x) user)))
      (unspecified)))
  
  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Assemble-file not supported on this target architecture.")
      (doit)))


; Compile and assemble a single expression; return the LOP segment.

(define compile-expression
  (let ()
    
    (define (compile-expression expr env)
      (let ((syntax-env
             (case (environment-tag env)
               ((0 1) (make-standard-syntactic-environment))
               ((2)   global-syntactic-environment)
               (else  
                (error "Invalid environment for compile-expression: " env)
                #t))))
        (let ((current-env global-syntactic-environment))
          (dynamic-wind
           (lambda ()
             (set! global-syntactic-environment syntax-env))
           (lambda ()
             (assemble (compile expr)))
           (lambda ()
             (set! global-syntactic-environment current-env))))))
    
    compile-expression))


(define macro-expand-expression
  (let ()
    
    (define (macro-expand-expression expr env)
      (let ((syntax-env
             (case (environment-tag env)
               ((0 1) (make-standard-syntactic-environment))
               ((2)   global-syntactic-environment)
               (else  
                (error "Invalid environment for compile-expression: " env)
                #t))))
        (let ((current-env global-syntactic-environment))
          (dynamic-wind
           (lambda ()
             (set! global-syntactic-environment syntax-env))
           (lambda ()
             (make-readable
              (macro-expand expr)))
           (lambda ()
             (set! global-syntactic-environment current-env))))))
    
    macro-expand-expression))


; Compile a scheme source file to a LAP file.

(define (compile313 infilename . rest)
  (let ((outfilename
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type infilename
                                *scheme-file-types* 
                                *lap-file-type*)))
        (write-lap
         (lambda (item port)
           (write item port)
           (newline port)
           (newline port))))
    (if (benchmark-block-mode)
        (process-file-block infilename outfilename write-lap compile-block)
        (process-file infilename outfilename write-lap compile))
    (unspecified)))


; Assemble a LAP or MAL file to a LOP file.

(define (assemble313 file . rest)
  (let ((outputfile
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type file 
                                (list *lap-file-type* *mal-file-type*)
                                *lop-file-type*)))
        (malfile?
         (file-type=? file *mal-file-type*))
        (user
         (assembly-user-data)))
    (process-file file
                  (list outputfile 'binary)
                  write-lop
                  (lambda (x) (assemble (if malfile? (eval x) x) user)))
    (unspecified)))


; Compile and assemble a Scheme source file to a LOP file.

(define (compile-and-assemble313 input-file . rest)
  (let ((output-file
         (if (not (null? rest))
             (car rest)
             (rewrite-file-type input-file 
                                *scheme-file-types*
                                *lop-file-type*)))
        (user
         (assembly-user-data)))
    (if (benchmark-block-mode)
        (process-file-block input-file
                            (list output-file 'binary)
                            write-lop
                            (lambda (x) (assemble (compile-block x) user)))
        (process-file input-file
                      (list output-file 'binary)
                      write-lop
                      (lambda (x) (assemble (compile x) user))))
    (unspecified)))


; Convert a LOP file to a FASL file.

(define (make-fasl infilename . rest)
  (define (doit)
    (let ((outfilename
           (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename
                                  *lop-file-type*
                                  *fasl-file-type*))))
      (process-file infilename
                    (list outfilename 'binary)
                    dump-fasl-segment-to-port
                    (lambda (x) x))
      (unspecified)))

  (if (eq? (nbuild-parameter 'target-machine) 'standard-c)
      (error "Make-fasl not supported on this target architecture.")
      (doit)))


; Disassemble a procedure's code vector.

(define (disassemble item . rest)
  (let ((output-port (if (null? rest)
                         (current-output-port)
                         (car rest))))
    (disassemble-item item #f output-port)
    (unspecified)))


; The item can be either a procedure or a pair (assumed to be a segment).

(define (disassemble-item item segment-no port)
  
  (define (print . rest)
    (for-each (lambda (x) (display x port)) rest)
    (newline port))
  
  (define (print-constvector cv)
    (do ((i 0 (+ i 1)))
        ((= i (vector-length cv)))
        (print "------------------------------------------")
        (print "Constant vector element # " i)
        (case (car (vector-ref cv i))
          ((codevector)
           (print "Code vector")
           (print-instructions (disassemble-codevector
                                (cadr (vector-ref cv i)))
                               port))
          ((constantvector)	
           (print "Constant vector")
           (print-constvector (cadr (vector-ref cv i))))
          ((global)
           (print "Global: " (cadr (vector-ref cv i))))
          ((data)
           (print "Data: " (cadr (vector-ref cv i)))))))
  
  (define (print-segment segment)
    (print "Segment # " segment-no)
    (print-instructions (disassemble-codevector (car segment)) port)
    (print-constvector (cdr segment))
    (print "========================================"))
  
  (cond ((procedure? item)
         (print-instructions (disassemble-codevector (procedure-ref item 0))
                             port))
        ((and (pair? item)
              (bytevector? (car item))
              (vector? (cdr item)))
         (print-segment item))
        (else
         (error "disassemble-item: " item " is not disassemblable."))))


; Disassemble a ".lop" or ".fasl" file; dump output to screen or 
; other (optional) file.

(define (disassemble-file file . rest)
  
  (define (doit input-port output-port)
    (display "; From " output-port)
    (display file output-port)
    (newline output-port)
    (do ((segment-no 0 (+ segment-no 1))
         (segment (read input-port) (read input-port)))
        ((eof-object? segment))
        (disassemble-item segment segment-no output-port)))

  ; disassemble313

  (call-with-input-file
   file
   (lambda (input-port)
     (if (null? rest)
         (doit input-port (current-output-port))
         (begin
          (delete-file (car rest))
          (call-with-output-file
           (car rest)
           (lambda (output-port) (doit input-port output-port)))))))
  (unspecified))


; Display and manipulate the compiler switches.

(define (compiler-switches . rest)

  (define (slow-code)
    (set-compiler-flags! 'no-optimization)
    (set-assembler-flags! 'no-optimization))

  (define (standard-code)
    (set-compiler-flags! 'standard)
    (set-assembler-flags! 'standard))

  (define (fast-safe-code)
    (set-compiler-flags! 'fast-safe)
    (set-assembler-flags! 'fast-safe))

  (define (fast-unsafe-code)
    (set-compiler-flags! 'fast-unsafe)
    (set-assembler-flags! 'fast-unsafe))

  (cond ((null? rest)
         (display "Debugging:")
         (newline)
         (display-twobit-flags 'debugging)
         (display-assembler-flags 'debugging)
         (newline)
         (display "Safety:")
         (newline)
         (display-twobit-flags 'safety)
         (display-assembler-flags 'safety)
         (newline)
         (display "Speed:")
         (newline)
         (display-twobit-flags 'optimization)
         (display-assembler-flags 'optimization)
         (if #f #f))
        ((null? (cdr rest))
         (case (car rest)
           ((0 slow)             (slow-code))
           ((1 standard)         (standard-code))
           ((2 fast-safe)        (fast-safe-code))
           ((3 fast-unsafe)      (fast-unsafe-code))
           ((default
             factory-settings)   (fast-safe-code)
                                 (include-source-code #t)
                                 (benchmark-mode #f)
                                 (benchmark-block-mode #f)
                                 (common-subexpression-elimination #f)
                                 (representation-inference #f))
           (else 
            (error "Unrecognized flag " (car rest) " to compiler-switches.")))
         (unspecified))
        (else
         (error "Too many arguments to compiler-switches."))))

; Read and process one file, producing another.
; Preserves the global syntactic environment.
; Filenames can be simple strings or list (filename mode) where mode
; is a symbol, "text" or "binary".

(define (process-file infilename outfilename writer processer)
  (process-files (list infilename) outfilename writer processer))

(define (process-files infilenames outfilename writer processer)
  (let ((outfilename (if (pair? outfilename) (car outfilename) outfilename))
	(outfilefn   (if (and (pair? outfilename) 
			      (eq? 'binary (cadr outfilename)))
			 call-with-raw-latin-1-output-file
			 call-with-output-file)))
    (define (attempt-compilation)
      (outfilefn outfilename
                 (lambda (outport)
                   (for-each
                    (lambda (infilename)
                      (let ((infilename  (if (pair? infilename) 
                                             (car infilename) 
                                             infilename))
                            (infilefn    (if (and (pair? infilename)
                                                  (eq? 'binary
                                                       (cadr infilename)))
                                             call-with-raw-latin-1-input-file
                                             call-with-input-file)))
                        (infilefn infilename
                                  (lambda (inport)
                                    (do ((x (read inport) (read inport)))
                                        ((eof-object? x))
                                      (writer (processer x) outport))))))
                    infilenames))))
    
    (delete-file outfilename)
    (let ((compilation-complete #f)
          (current-syntactic-environment
           (syntactic-copy global-syntactic-environment)))
      (dynamic-wind
          (lambda () 
            (cond (compilation-complete
                   (error "Attempted to resume an abandoned compilation."))))
          (lambda () (attempt-compilation) (set! compilation-complete #t))
          (lambda () 
            (set! global-syntactic-environment
                  current-syntactic-environment)
            (cond ((not compilation-complete)
                   (delete-file outfilename)
                   (set! compilation-complete #t))))))))

; Same as above, but passes a list of the entire file's contents
; to the processer.  Note, processes one input file at a time, though
; plausibly it should read all the files and process the collected
; input together.
;
; FIXME:  Both versions of PROCESS-FILE always delete the output file.
; Shouldn't it be left alone if the input file can't be opened?

(define (process-file-block infilename outfilename writer processer)
  (process-files-block (list infilename) outfilename writer processer))

(define (process-files-block infilenames outfilename writer processer)
  (let ((outfilename (if (pair? outfilename) (car outfilename) outfilename))
	(outfilefn   (if (and (pair? outfilename) 
			      (eq? 'binary (cadr outfilename)))
			 call-with-raw-latin-1-output-file
			 call-with-output-file))
        (current-syntactic-environment
         (syntactic-copy global-syntactic-environment)))
    (delete-file outfilename)
    (outfilefn outfilename
      (lambda (outport)
	(for-each
	 (lambda (infilename)
	   (let ((infilename  (if (pair? infilename) 
				  (car infilename)
				  infilename))
		 (infilefn    (if (and (pair? infilename)
				       (eq? 'binary (cadr infilename)))
				  call-with-raw-latin-1-input-file
				  call-with-input-file)))
	     (infilefn infilename
		       (lambda (inport)
			 (do ((x (read inport) (read inport))
			      (forms '() (cons x forms)))
			     ((eof-object? x)
			      (writer (processer (reverse forms)) 
				      outport)))))))
	 infilenames)
       (set! global-syntactic-environment current-syntactic-environment)))))

; Given a file name with some type, produce another with some other type.

(define (rewrite-file-type filename matches new)
  (if (not (pair? matches))
      (rewrite-file-type filename (list matches) new)
      (let ((j (string-length filename)))
        (let loop ((m matches))
          (cond ((null? m)
                 (string-append filename new))
                (else
                 (let* ((n (car m))
                        (l (string-length n)))
                   (if (file-type=? filename n)
                       (string-append (substring filename 0 (- j l)) new)
                       (loop (cdr m))))))))))

(define (file-type=? file-name type-name)
  (let ((fl (string-length file-name))
        (tl (string-length type-name)))
    (and (>= fl tl)
         (string-ci=? type-name
                      (substring file-name (- fl tl) fl)))))

; eof
; Copyright 1998 William Clinger.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Procedures that make .LAP structures human-readable

(define (readify-lap code)
  (map (lambda (x)
	 (let ((iname (cdr (assv (car x) *mnemonic-names*))))
	   (if (not (= (car x) $lambda))
	       (cons iname (cdr x))
	       (list iname (readify-lap (cadr x)) (caddr x)))))
       code))

(define (readify-file f . o)

  (define (doit)
    (let ((i (open-input-file f)))
      (let loop ((x (read i)))
	(if (not (eof-object? x))
	    (begin (pretty-print (readify-lap x))
		   (loop (read i)))))))

  (if (null? o)
      (doit)
      (begin (delete-file (car o))
	     (with-output-to-file (car o) doit))))

; eof
; Copyright 1991 Lightship Software, Incorporated.
; 
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Target-independent part of the assembler.
;
; This is a simple, table-driven, one-pass assembler.
; Part of it assumes a big-endian target machine.
;
; The input to this pass is a list of symbolic MacScheme machine
; instructions and pseudo-instructions.  Each symbolic MacScheme 
; machine instruction or pseudo-instruction is a list whose car
; is a small non-negative fixnum that acts as the mnemonic for the
; instruction.  The rest of the list is interpreted as indicated
; by the mnemonic.
;
; The output is a pair consisting of machine code (a bytevector or 
; string) and a constant vector.
;
; This assembler is table-driven, and may be customized to emit
; machine code for different target machines.  The table consists
; of a vector of procedures indexed by mnemonics.  Each procedure
; in the table should take two arguments: an assembly structure
; and a source instruction.  The procedure should just assemble
; the instruction using the operations defined below.
;
; The table and target can be changed by redefining the following 
; five procedures.

(define (assembly-table) (error "No assembly table defined."))
(define (assembly-start as) #t)
(define (assembly-end as segment) segment)
(define (assembly-user-data) #f)

; The main entry point.

(define (assemble source . rest)
  (let* ((user (if (null? rest) (assembly-user-data) (car rest)))
	 (as   (make-assembly-structure source (assembly-table) user)))
    (assembly-start as)
    (assemble1 as
	       (lambda (as)
		 (let ((segment (assemble-pasteup as)))
		   (assemble-finalize! as)
		   (assembly-end as segment)))
	       #f)))

; The following procedures are to be called by table routines.
;
; The assembly source for nested lambda expressions should be
; assembled by calling this procedure.  This allows an inner
; lambda to refer to labels defined by outer lambdas.
;
; We delay the assembly of the nested lambda until after the outer lambda
; has been finalized so that all labels in the outer lambda are known
; to the inner lambda.
;
; The continuation procedure k is called to backpatch the constant
; vector of the outer lambda after the inner lambda has been
; finalized.  This is necessary because of the delayed evaluation: the
; outer lambda holds code and constants for the inner lambda in its
; constant vector.

(define (assemble-nested-lambda as source doc k . rest)
  (let* ((user (if (null? rest) #f (car rest)))
	 (nested-as (make-assembly-structure source (as-table as) user)))
    (as-parent! nested-as as)
    (as-nested! as (cons (lambda ()
			   (assemble1 nested-as 
				      (lambda (nested-as)
					(let ((segment
					       (assemble-pasteup nested-as)))
					  (assemble-finalize! nested-as)
					  (k nested-as segment)))
				      doc))
			 (as-nested as)))))

(define operand0 car)      ; the mnemonic
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define (operand4 i) (car (cddddr i)))

; Emits the bits contained in the bytevector bv.

(define (emit! as bv)
  (as-code! as (cons bv (as-code as)))
  (as-lc! as (+ (as-lc as) (bytevector-length bv))))

; Emits the characters contained in the string s as code (for C generation).

(define (emit-string! as s)
  (as-code! as (cons s (as-code as)))
  (as-lc! as (+ (as-lc as) (string-length s))))

; Given any Scheme object that may legally be quoted, returns an
; index into the constant vector for that constant.

(define (emit-constant as x)
  (do ((i 0 (+ i 1))
       (y (as-constants as) (cdr y)))
      ((or (null? y) (equal? x (car y)))
       (if (null? y)
	   (as-constants! as (append! (as-constants as) (list x))))
       i)))

(define (emit-datum as x)
  (emit-constant as (list 'data x)))

(define (emit-global as x)
  (emit-constant as (list 'global x)))

(define (emit-codevector as x)
  (emit-constants as (list 'codevector x)))

(define (emit-constantvector as x)
  (emit-constants as (list 'constantvector x)))

; Set-constant changes the datum stored, without affecting the tag.
; It can operate on the list form because the pair stored in the list
; is shared between the list and any vector created from the list.

(define (set-constant! as n datum)
  (let ((pair (list-ref (as-constants as) n)))
    (set-car! (cdr pair) datum)))

; Guarantees that the constants will not share structure
; with any others, and will occupy consecutive positions
; in the constant vector.  Returns the index of the first
; constant.

(define (emit-constants as x . rest)
  (let* ((constants (as-constants as))
         (i         (length constants)))
    (as-constants! as (append! constants (cons x rest)))
    i))

; Defines the given label using the current location counter.

(define (emit-label! as L)
  (set-cdr! L (as-lc as)))

; Adds the integer n to the size code bytes beginning at the
; given byte offset from the current value of the location counter.

(define (emit-fixup! as offset size n)
  (as-fixups! as (cons (list (+ offset (as-lc as)) size n)
		       (as-fixups as))))

; Adds the value of the label L to the size code bytes beginning
; at the given byte offset from the current location counter.

(define (emit-fixup-label! as offset size L)
  (as-fixups! as (cons (list (+ offset (as-lc as)) size (list L))
		       (as-fixups as))))

; Allows the procedure proc of two arguments (code vector and current
; location counter) to modify the code vector at will, at fixup time.

(define (emit-fixup-proc! as proc)
  (as-fixups! as (cons (list (as-lc as) 0 proc)
		       (as-fixups as))))

; Labels.

; The current value of the location counter.

(define (here as) (as-lc as))

; Given a MAL label (a number), create an assembler label.

(define (make-asm-label as label)
  (let ((probe (find-label as label)))
    (if probe
	probe
	(let ((l (cons label #f)))
	  (as-labels! as (cons l (as-labels as)))
	  l))))

; This can use hashed lookup.

(define (find-label as L)

  (define (lookup-label-loop x labels parent)
    (let ((entry (assq x labels)))
      (cond (entry)
	    ((not parent) #f)
	    (else 
	     (lookup-label-loop x (as-labels parent) (as-parent parent))))))
    
  (lookup-label-loop L (as-labels as) (as-parent as)))

; Create a new assembler label, distinguishable from a MAL label.

(define new-label
  (let ((n 0))
    (lambda ()
      (set! n (- n 1))
      (cons n #f))))

; Given a value name (a number), return the label value or #f.

(define (label-value as L) (cdr L))

; For peephole optimization.

(define (next-instruction as)
  (let ((source (as-source as)))
    (if (null? source)
        '(-1)
        (car source))))

(define (consume-next-instruction! as)
  (as-source! as (cdr (as-source as))))

(define (push-instruction as instruction)
  (as-source! as (cons instruction (as-source as))))

; For use by the machine assembler: assoc lists connected to as structure.

(define (assembler-value as key)
  (let ((probe (assq key (as-values as))))
    (if probe
	(cdr probe)
	#f)))

(define (assembler-value! as key value)
  (let ((probe (assq key (as-values as))))
    (if probe
	(set-cdr! probe value)
	(as-values! as (cons (cons key value) (as-values as))))))

; For documentation.
;
; The value must be a documentation structure (a vector).

(define (add-documentation as doc)
  (let* ((existing-constants (cadr (car (as-constants as))))
	 (new-constants 
	  (twobit-sort (lambda (a b)
			 (< (car a) (car b)))
		       (cond ((not existing-constants)
			      (list (cons (here as) doc)))
			     ((pair? existing-constants)
			      (cons (cons (here as) doc)
				    existing-constants))
			     (else
			      (list (cons (here as) doc)
				    (cons 0 existing-constants)))))))
    (set-car! (cdar (as-constants as)) new-constants)))

; This is called when a value is too large to be handled by the assembler.
; Info is a string, expr an assembler expression, and val the resulting
; value.  The default behavior is to signal an error.

(define (asm-value-too-large as info expr val)
  (if (as-retry as)
      ((as-retry as))
      (asm-error info ": Value too large: " expr " = " val)))

; The implementations of asm-error and disasm-error depend on the host
; system. Sigh.

(define (asm-error msg . rest)
  (cond ((eq? host-system 'chez)
	 (error 'assembler "~a" (list msg rest)))
	(else
	 (apply error msg rest))))

(define (disasm-error msg . rest)
  (cond ((eq? host-system 'chez)
	 (error 'disassembler "~a" (list msg rest)))
	(else
	 (apply error msg rest))))

; The remaining procedures in this file are local to the assembler.

; An assembly structure is a vector consisting of
;
;    table          (a table of assembly routines)
;    source         (a list of symbolic instructions)
;    lc             (location counter; an integer)
;    code           (a list of bytevectors)
;    constants      (a list)
;    labels         (an alist of labels and values)
;    fixups         (an alist of locations, sizes, and labels or fixnums)
;    nested         (a list of assembly procedures for nested lambdas)
;    values         (an assoc list)
;    parent         (an assembly structure or #f)
;    retry          (a thunk or #f)
;    user-data      (anything)
;
; In fixups, labels are of the form (<L>) to distinguish them from fixnums.

(define (label? x) (and (pair? x) (fixnum? (car x))))
(define label.ident car)

(define (make-assembly-structure source table user-data)
  (vector table
          source
          0
          '()
          '()
          '()
          '()
          '()
	  '()
	  #f
	  #f
	  user-data))

(define (as-reset! as source)
  (as-source! as source)
  (as-lc! as 0)
  (as-code! as '())
  (as-constants! as '())
  (as-labels! as '())
  (as-fixups! as '())
  (as-nested! as '())
  (as-values! as '())
  (as-retry! as #f))

(define (as-table as)     (vector-ref as 0))
(define (as-source as)    (vector-ref as 1))
(define (as-lc as)        (vector-ref as 2))
(define (as-code as)      (vector-ref as 3))
(define (as-constants as) (vector-ref as 4))
(define (as-labels as)    (vector-ref as 5))
(define (as-fixups as)    (vector-ref as 6))
(define (as-nested as)    (vector-ref as 7))
(define (as-values as)    (vector-ref as 8))
(define (as-parent as)    (vector-ref as 9))
(define (as-retry as)     (vector-ref as 10))
(define (as-user as)      (vector-ref as 11))

(define (as-source! as x)    (vector-set! as 1 x))
(define (as-lc! as x)        (vector-set! as 2 x))
(define (as-code! as x)      (vector-set! as 3 x))
(define (as-constants! as x) (vector-set! as 4 x))
(define (as-labels! as x)    (vector-set! as 5 x))
(define (as-fixups! as x)    (vector-set! as 6 x))
(define (as-nested! as x)    (vector-set! as 7 x))
(define (as-values! as x)    (vector-set! as 8 x))
(define (as-parent! as x)    (vector-set! as 9 x))
(define (as-retry! as x)     (vector-set! as 10 x))
(define (as-user! as x)      (vector-set! as 11 x))

; The guts of the assembler.

(define (assemble1 as finalize doc)
  (let ((assembly-table (as-table as))
	(peep? (peephole-optimization))
	(step? (single-stepping))
	(step-instr (list $.singlestep))
	(end-instr (list $.end)))

    (define (loop)
      (let ((source (as-source as)))
        (if (null? source)
	    (begin ((vector-ref assembly-table $.end) end-instr as)
		   (finalize as))
            (begin (if step?
		       ((vector-ref assembly-table $.singlestep)
			step-instr
			as))
		   (if peep?
		       (let peeploop ((src1 source))
			 (peep as)
			 (let ((src2 (as-source as)))
			   (if (not (eq? src1 src2))
			       (peeploop src2)))))
		   (let ((source (as-source as)))
		     (as-source! as (cdr source))
		     ((vector-ref assembly-table (caar source))
		      (car source)
		      as)
		     (loop))))))

    (define (doit)
      (emit-datum as doc)
      (loop))

    (let* ((source (as-source as))
	   (r (call-with-current-continuation
	       (lambda (k)
		 (as-retry! as (lambda () (k 'retry)))
		 (doit)))))
      (if (eq? r 'retry)
	  (let ((old (short-effective-addresses)))
	    (as-reset! as source)
	    (dynamic-wind
	     (lambda ()
	       (short-effective-addresses #f))
	     doit
	     (lambda ()
	       (short-effective-addresses old))))
	  r))))

(define (assemble-pasteup as)

  (define (pasteup-code)
    (let ((code      (make-bytevector (as-lc as)))
	  (constants (list->vector (as-constants as))))
    
      ; The bytevectors: byte 0 is most significant.

      (define (paste-code! bvs i)
	(if (not (null? bvs))
	    (let* ((bv (car bvs))
		   (n  (bytevector-length bv)))
	      (do ((i i (- i 1))
		   (j (- n 1) (- j 1)))	; (j 0 (+ j 1))
		  ((< j 0)		; (= j n)
		   (paste-code! (cdr bvs) i))
                (bytevector-set! code i (bytevector-ref bv j))))))
    
      (paste-code! (as-code as) (- (as-lc as) 1))
      (as-code! as (list code))
      (cons code constants)))

  (define (pasteup-strings)
    (let ((code      (make-string (as-lc as)))
	  (constants (list->vector (as-constants as))))

      (define (paste-code! strs i)
	(if (not (null? strs))
	    (let* ((s (car strs))
		   (n (string-length s)))
	      (do ((i i (- i 1))
		   (j (- n 1) (- j 1)))	; (j 0 (+ j 1))
		  ((< j 0)		; (= j n)
		   (paste-code! (cdr strs) i))
                (string-set! code i (string-ref s j))))))

      (paste-code! (as-code as) (- (as-lc as) 1))
      (as-code! as (list code))
      (cons code constants)))

  (if (bytevector? (car (as-code as)))
      (pasteup-code)
      (pasteup-strings)))

(define (assemble-finalize! as)
  (let ((code (car (as-code as))))

    (define (apply-fixups! fixups)
      (if (not (null? fixups))
          (let* ((fixup      (car fixups))
                 (i          (car fixup))
                 (size       (cadr fixup))
                 (adjustment (caddr fixup))  ; may be procedure
                 (n          (if (label? adjustment)
				 (lookup-label adjustment)
				 adjustment)))
            (case size
	      ((0) (fixup-proc code i n))
              ((1) (fixup1 code i n))
              ((2) (fixup2 code i n))
              ((3) (fixup3 code i n))
              ((4) (fixup4 code i n))
              (else ???))
            (apply-fixups! (cdr fixups)))))

    (define (lookup-label L)
      (or (label-value as (label.ident L))
	  (asm-error "Assembler error -- undefined label " L)))

    (apply-fixups! (reverse! (as-fixups as)))

    (for-each (lambda (nested-as-proc)
		(nested-as-proc))
	      (as-nested as))))


; These fixup routines assume a big-endian target machine.

(define (fixup1 code i n)
  (bytevector-set! code i (+ n (bytevector-ref code i))))

(define (fixup2 code i n)
  (let* ((x  (+ (* 256 (bytevector-ref code i))
                (bytevector-ref code (+ i 1))))
         (y  (+ x n))
         (y0 (modulo y 256))
         (y1 (modulo (quotient (- y y0) 256) 256)))
    (bytevector-set! code i y1)
    (bytevector-set! code (+ i 1) y0)))

(define (fixup3 code i n)
  (let* ((x  (+ (* 65536 (bytevector-ref code i))
		(* 256 (bytevector-ref code (+ i 1)))
                (bytevector-ref code (+ i 2))))
         (y  (+ x n))
         (y0 (modulo y 256))
         (y1 (modulo (quotient (- y y0) 256) 256))
         (y2 (modulo (quotient (- y (* 256 y1) y0) 256) 256)))
    (bytevector-set! code i y2)
    (bytevector-set! code (+ i 1) y1)
    (bytevector-set! code (+ i 2) y0)))

(define (fixup4 code i n)
  (let* ((x  (+ (* 16777216 (bytevector-ref code i))
		(* 65536 (bytevector-ref code (+ i 1)))
		(* 256 (bytevector-ref code (+ i 2)))
		(bytevector-ref code (+ i 3))))
         (y  (+ x n))
         (y0 (modulo y 256))
         (y1 (modulo (quotient (- y y0) 256) 256))
         (y2 (modulo (quotient (- y (* 256 y1) y0) 256) 256))
         (y3 (modulo (quotient (- y (* 65536 y2)
                                    (* 256 y1)
                                    y0)
                               256)
                     256)))
    (bytevector-set! code i y3)
    (bytevector-set! code (+ i 1) y2)
    (bytevector-set! code (+ i 2) y1)
    (bytevector-set! code (+ i 3) y0)))

(define (fixup-proc code i p)
  (p code i))

; For testing.

(define (view-segment segment)
  (define (display-bytevector bv)
    (let ((n (bytevector-length bv)))
      (do ((i 0 (+ i 1)))
          ((= i n))
          (if (zero? (remainder i 4))
              (write-char #\space))
          (if (zero? (remainder i 8))
              (write-char #\space))
          (if (zero? (remainder i 32))
              (newline))
          (let ((byte (bytevector-ref bv i)))
            (write-char
	     (string-ref (number->string (quotient byte 16) 16) 0))
            (write-char
	     (string-ref (number->string (remainder byte 16) 16) 0))))))
  (if (and (pair? segment)
           (bytevector? (car segment))
           (vector? (cdr segment)))
      (begin (display-bytevector (car segment))
             (newline)
             (write (cdr segment))
             (newline)
             (do ((constants (vector->list (cdr segment))
                             (cdr constants)))
                 ((or (null? constants)
                      (null? (cdr constants))))
                 (if (and (bytevector? (car constants))
                          (vector? (cadr constants)))
                     (view-segment (cons (car constants)
                                         (cadr constants))))))))

; emit is a procedure that takes an as and emits instructions into it.

(define (test-asm emit)
  (let ((as (make-assembly-structure #f #f #f)))
    (emit as)
    (let ((segment (assemble-pasteup as)))
      (assemble-finalize! as)
      (disassemble segment))))

(define (compile&assemble x)
  (view-segment (assemble (compile x))))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Common assembler -- miscellaneous utility procedures.

; Given any Scheme object, return its printable representation as a string.
; This code is largely portable (see comments).

(define (format-object x)

  (define (format-list x)
    (define (loop x)
      (cond ((null? x)
	     '(")"))
	    ((null? (cdr x))
	     (list (format-object (car x)) ")"))
	    (else
	     (cons (format-object (car x))
		   (cons " " 
			 (loop (cdr x)))))))
    (apply string-append (cons "(" (loop x))))

  (define (format-improper-list x)
    (define (loop x)
      (if (pair? (cdr x))
	  (cons (format-object (car x))
		(cons " "
		      (loop (cdr x))))
	  (list (format-object (car x))
		" . "
		(format-object (cdr x))
		")")))
    (apply string-append (cons "(" (loop x))))

  (cond ((null? x)             "()")
	((not x)               "#f")
	((eq? x #t)            "#t")
	((symbol? x)           (symbol->string x))
	((number? x)           (number->string x))
	((char? x)             (string x))
	((string? x)           x)
	((procedure? x)        "#<procedure>")
	((bytevector? x)       "#<bytevector>")     ; Larceny
	((eof-object? x)       "#<eof>")
	((port? x)             "#<port>")
	((eq? x (unspecified)) "#!unspecified")     ; Larceny
	((eq? x (undefined))   "#!undefined")       ; Larceny
	((vector? x)
	 (string-append "#" (format-list (vector->list x))))
	((list? x)
	 (format-list x))
	((pair? x)
	 (format-improper-list x))
	(else                  "#<weird>")))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Larceny assembler -- 32-bit big-endian utility procedures.
;
; 32-bit numbers are represented as 4-byte bytevectors where byte 3
; is the least significant and byte 0 is the most significant.
;
; Logically, the 'big' end is on the left and the 'little' end
; is on the right, so a left shift shifts towards the 'big' end.
;
; Performance: poor, for good reasons.  See asmutil32.sch.

; Identifies the code loaded.

(define asm:endianness 'big)


; Given four bytes, create a length-4 bytevector. 
; N1 is the most significant byte, n4 the least significant.

(define (asm:bv n1 n2 n3 n4)
  (let ((bv (make-bytevector 4)))
    (bytevector-set! bv 0 n1)
    (bytevector-set! bv 1 n2)
    (bytevector-set! bv 2 n3)
    (bytevector-set! bv 3 n4)
    bv))


; Given a length-4 bytevector, convert it to an integer.

(define (asm:bv->int bv)
  (let ((i (+ (* (+ (* (+ (* (bytevector-ref bv 0) 256)
			  (bytevector-ref bv 1))
		       256)
		    (bytevector-ref bv 2))
		 256)
	      (bytevector-ref bv 3))))
    (if (> (bytevector-ref bv 0) 127)
	(- i)
	i)))


; Shift the bits of m left by n bits, shifting in zeroes at the right end.
; Returns a length-4 bytevector.
;
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.

(define (asm:lsh m n)
  (if (not (bytevector? m))
      (asm:lsh (asm:int->bv m) n)
      (let ((m (bytevector-copy m))
	    (n (remainder n 33)))
	(if (>= n 8)
	    (let ((k (quotient n 8)))
	      (do ((i 0 (+ i 1)))
		  ((= (+ i k) 4)
		   (do ((i i (+ i 1)))
		       ((= i 4))
		     (bytevector-set! m i 0)))
		(bytevector-set! m i (bytevector-ref m (+ i k))))))
	(let* ((d0 (bytevector-ref m 0))
	       (d1 (bytevector-ref m 1))
	       (d2 (bytevector-ref m 2))
	       (d3 (bytevector-ref m 3))
	       (n  (remainder n 8))
	       (n- (- 8 n)))
	  (asm:bv (logand (logior (lsh d0 n) (rshl d1 n-)) 255)
		  (logand (logior (lsh d1 n) (rshl d2 n-)) 255)
		  (logand (logior (lsh d2 n) (rshl d3 n-)) 255)
		  (logand (lsh d3 n) 255))))))


; Shift the bits of m right by n bits, shifting in zeroes at the high end.
; Returns a length-4 bytevector.
;
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.

(define (asm:rshl m n)
  (if (not (bytevector? m))
      (asm:rshl (asm:int->bv m) n)
      (let ((m (bytevector-copy m))
	    (n (remainder n 33)))
	(if (>= n 8)
	    (let ((k (quotient n 8)))
	      (do ((i 3 (- i 1)))
		  ((< (- i k) 0)
		   (do ((i i (- i 1)))
		       ((< i 0))
		     (bytevector-set! m i 0)))
		(bytevector-set! m i (bytevector-ref m (- i k))))))
	(let* ((d0 (bytevector-ref m 0))
	       (d1 (bytevector-ref m 1))
	       (d2 (bytevector-ref m 2))
	       (d3 (bytevector-ref m 3))
	       (n  (remainder n 8))
	       (n- (- 8 n)))
	  (asm:bv (rshl d0 n)
		  (logand (logior (rshl d1 n) (lsh d0 n-)) 255)
		  (logand (logior (rshl d2 n) (lsh d1 n-)) 255)
		  (logand (logior (rshl d3 n) (lsh d2 n-)) 255))))))


; Shift the bits of m right by n bits, shifting in the sign bit at the
; high end.  Returns a length-4 bytevector.
;
; M may be an exact integer or a length-4 bytevector.
; N must be an exact nonnegative integer; it's interpreted modulo 33.

(define asm:rsha
  (let ((ones (asm:bv #xff #xff #xff #xff)))
    (lambda (m n)
      (let* ((m (if (bytevector? m) m (asm:int->bv m)))
	     (n (remainder n 33))
	     (h (rshl (bytevector-ref m 0) 7))
	     (k (asm:rshl m n)))
;	(format #t "~a ~a ~a~%" h (bytevector-ref m 0) n)
;	(prnx (asm:lsh ones (- 32 n))) (newline)
	(if (zero? h)
	    k
	    (asm:logior k (asm:lsh ones (- 32 n))))))))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Larceny assembler -- 32-bit endianness-independent utility procedures.
;
; 32-bit numbers are represented as 4-byte bytevectors where the
; exact layout depends on whether the little-endian or big-endian
; module has been loaded.  One of them must be loaded prior to loading
; this module.
;
; Logically, the 'big' end is on the left and the 'little' end
; is on the right, so a left shift shifts towards the big end.
;
; Generally, performance is not a major issue in this module.  The 
; assemblers should use more specialized code for truly good performance.
; These procedures are mainly suitable for one-time construction of 
; instruction templates, and during development.
;
; Endian-ness specific operations are in asmutil32be.sch and asmutil32le.sch:
;
;   (asm:bv n0 n1 n2 n3)    ; Construct bytevector
;   (asm:bv->int bv)        ; Convert bytevector to integer
;   (asm:lsh m k)           ; Shift left logical k bits
;   (asm:rshl m k)          ; Shift right logical k bits
;   (asm:rsha m k)          ; Shirt right arithmetic k bits


; Convert an integer to a length-4 bytevector using two's complement 
; representation for negative numbers.
; Returns length-4 bytevector.
;
; The procedure handles numbers in the range -2^31..2^32-1 [sic].
; It is an error for the number to be outside this range.
;
; FIXME: quotient/remainder may be slow; we could have special fixnum
;        case that uses shifts (that could be in-lined as macro).  It could
;        work for negative numbers too.
; FIXME: should probably check that the number is within range.

(define asm:int->bv
  (let ((two^32 (expt 2 32)))
    (lambda (m)
      (let* ((m  (if (< m 0) (+ two^32 m) m))
	     (b0 (remainder m 256))
	     (m  (quotient m 256))
	     (b1 (remainder m 256))
	     (m  (quotient m 256))
	     (b2 (remainder m 256))
	     (m  (quotient m 256))
	     (b3 (remainder m 256)))
	(asm:bv b3 b2 b1 b0)))))


; `Or' the bits of multiple operands together. 
; Each operand may be an exact integer or a length-4 bytevector.
; Returns a length-4 bytevector.

(define (asm:logior . ops)
  (let ((r (asm:bv 0 0 0 0)))
    (do ((ops ops (cdr ops)))
	((null? ops) r)
      (let* ((op (car ops))
	     (op (if (bytevector? op) op (asm:int->bv op))))
	(bytevector-set! r 0 (logior (bytevector-ref r 0)
				     (bytevector-ref op 0)))
	(bytevector-set! r 1 (logior (bytevector-ref r 1)
				     (bytevector-ref op 1)))
	(bytevector-set! r 2 (logior (bytevector-ref r 2)
				     (bytevector-ref op 2)))
	(bytevector-set! r 3 (logior (bytevector-ref r 3)
				     (bytevector-ref op 3)))))))


; `And' the bits of two operands together.
; Either may be an exact integer or length-4 bytevector.
; Returns length-4 bytevector.

(define (asm:logand op1 op2)
  (let ((op1 (if (bytevector? op1) op1 (asm:int->bv op1)))
	(op2 (if (bytevector? op2) op2 (asm:int->bv op2)))
	(bv  (make-bytevector 4)))
    (bytevector-set! bv 0 (logand (bytevector-ref op1 0)
				  (bytevector-ref op2 0)))
    (bytevector-set! bv 1 (logand (bytevector-ref op1 1)
				  (bytevector-ref op2 1)))
    (bytevector-set! bv 2 (logand (bytevector-ref op1 2)
				  (bytevector-ref op2 2)))
    (bytevector-set! bv 3 (logand (bytevector-ref op1 3)
				  (bytevector-ref op2 3)))
    bv))


; Extract the n low-order bits of m.
; m may be an exact integer or a length-4 bytevector.
; n must be an exact nonnegative integer, interpreted modulo 32.
; Returns length-4 bytevector.
;
; Does not depend on endian-ness.

(define asm:lobits 
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (asm:int->bv (- (expt 2 i) 1))))
    (lambda (m n)
      (asm:logand m (vector-ref v (remainder n 33))))))

; Extract the n high-order bits of m.
; m may be an exact integer or a length-4 bytevector.
; n must be an exact nonnegative integer, interpreted modulo 33.
; Returns length-4 bytevector with the high-order bits of m at low end.
;
; Does not depend on endian-ness.

(define (asm:hibits m n)
  (asm:rshl m (- 32 (remainder n 33))))

; Test that the given number (not! bytevector) m fits in an n-bit 
; signed slot.
;
; Does not depend on endian-ness.

(define asm:fits?
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (expt 2 i)))
    (lambda (m n)
      (<= (- (vector-ref v (- n 1))) m (- (vector-ref v (- n 1)) 1)))))

; Test that the given number (not! bytevector) m fits in an n-bit 
; unsigned slot.
;
; Does not depend on endian-ness.

(define asm:fits-unsigned?
  (let ((v (make-vector 33)))
    (do ((i 0 (+ i 1)))
	((= i 33))
      (vector-set! v i (expt 2 i)))
    (lambda (m n)
      (<= 0 m (- (vector-ref v n) 1)))))

; Add two operands (numbers or bytevectors).
;
; Does not depend on endian-ness.

(define (asm:add a b)
  (asm:int->bv (+ (if (bytevector? a) (asm:bv->int a) a)
		  (if (bytevector? b) (asm:bv->int b) b))))

; Given an unsigned 32-bit number, return it as a signed number
; as appropriate.
;
; Does not depend on endian-ness.

(define (asm:signed n)
  (if (< n 2147483647)
      n
      (- n 4294967296)))


(define (asm:print-bv bv)

  (define hex "0123456789abcdef")

  (define (pdig k)
    (display (string-ref hex (quotient k 16)))
    (display (string-ref hex (remainder k 16)))
    (display " "))
  
  (if (eq? asm:endianness 'little)
      (do ((i 3 (- i 1)))
	  ((< i 0))
	(pdig (bytevector-ref bv i)))
      (do ((i 0 (+ i 1)))
	  ((= i 4))
	(pdig (bytevector-ref bv i)))))


; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Procedure that writes fastload segment.
;
; The procedure 'dump-fasl-segment-to-port' takes a segment and an output
; port as arguments and dumps the segment in fastload format on that port.
; The port must be a binary (untranslated) port.
;
; A fastload segment looks like a Scheme expression, and in fact, 
; fastload files can mix compiled and uncompiled expressions.  A compiled
; expression (as created by dump-fasl-segment-to-port) is a list with
; a literal procedure in the operator position and no arguments.
;
; A literal procedure is a three-element list prefixed by #^P.  The three
; elements are code (a bytevector), constants (a regular vector), and
; R0/static link slot (always #f).  
;
; A bytevector is a string prefixed by #^B. The string may contain 
; control characters; \ and " must be quoted as usual.
;
; A global variable reference in the constant vector is a symbol prefixed
; by #^G.  On reading, the reference is replaced by (a pointer to) the 
; actual cell.
;
; This code is highly bummed.  The procedure write-bytevector-like has the
; same meaning as display, but in Larceny, the former is currently much
; faster than the latter.

(define (dump-fasl-segment-to-port segment outp . rest)
  (let* ((omit-code? (not (null? rest)))
         (controllify
	  (lambda (char)
	    (integer->char (- (char->integer char) (char->integer #\@)))))
	 (CTRLP       (controllify #\P))
	 (CTRLB       (controllify #\B))
	 (CTRLG       (controllify #\G))
	 (DOUBLEQUOTE (char->integer #\"))
	 (BACKSLASH   (char->integer #\\))
	 (len         1024))

    (define buffer (make-string len #\&))
    (define ptr 0)

    (define (flush)
      (write-bytevector-like 
       (string->bytevector (if (< ptr len) 
                               (substring buffer 0 ptr) 
                               buffer))
       outp)
      (set! ptr 0))

    (define (putc c)
      (if (= ptr len) (flush))
      (string-set! buffer ptr c)
      (set! ptr (+ ptr 1)))

    (define (putb b)
      (if (= ptr len) (flush))
      (string-set! buffer ptr (integer->char b))
      (set! ptr (+ ptr 1)))

    (define (puts s)
      (let ((ls (string-length s)))
	(if (>= (+ ptr ls) len)
	    (begin (flush)
		   (write-bytevector-like s outp))
	    (do ((i (- ls 1) (- i 1))
		 (p (+ ptr ls -1) (- p 1)))
		((< i 0)
		 (set! ptr (+ ptr ls)))
	      (string-set! buffer p (string-ref s i))))))

    (define (putd d)
      (flush)
      (write-fasl-datum d outp))

    (define (dump-codevec bv)
      (if omit-code?
          (puts "#f")
          (begin
            (putc #\#)
            (putc CTRLB)
            (putc #\")
            (let ((limit (bytevector-length bv)))
              (do ((i 0 (+ i 1)))
                  ((= i limit) (putc #\")
                               (putc #\newline))
                (let ((c (bytevector-ref bv i)))
                  (cond ((= c DOUBLEQUOTE) (putc #\\))
                        ((= c BACKSLASH)   (putc #\\)))
                  (putb c)))))))

    (define (dump-constvec cv)
      (puts "#(")
      (for-each (lambda (const)
		  (putc #\space)
		  (case (car const)
		    ((data)
		     (putd (cadr const)))
		    ((constantvector)
		     (dump-constvec (cadr const)))
		    ((codevector)
		     (dump-codevec (cadr const)))
		    ((global)
		     (putc #\#)
		     (putc CTRLG)
		     (putd (cadr const)))
		    ((bits)
		     (error "BITS attribute is not supported in fasl files."))
		    (else
		     (error "Faulty .lop file."))))
		(vector->list cv))
      (puts ")")
      (putc #\newline))

    (define (dump-fasl-segment segment)
      (if (not omit-code?) (putc #\())
      (putc #\#)
      (putc CTRLP)
      (putc #\()
      (dump-codevec (car segment))
      (putc #\space)
      (dump-constvec (cdr segment))
      (puts " #f)")
      (if (not omit-code?) (putc #\)))
      (putc #\newline))

    (dump-fasl-segment segment)
    (flush)))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
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

(define heap.version-number 9)		; Heap version number

(define heap.root-names			; Roots in heap version 9
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

  ; FIXME:  Should use binary i/o here...

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
	  '()			     ; thunks
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

(define (heap.word-be! h w)
  (heap.byte! h (quotient w twofiftysix^3))
  (heap.byte! h (quotient (remainder w twofiftysix^3) twofiftysix^2))
  (heap.byte! h (quotient (remainder w twofiftysix^2) twofiftysix))
  (heap.byte! h (remainder w twofiftysix)))

(define (heap.word-el! h w)
  (heap.byte! h (remainder w twofiftysix))
  (heap.byte! h (quotient (remainder w twofiftysix^2) twofiftysix))
  (heap.byte! h (quotient (remainder w twofiftysix^3) twofiftysix^2))
  (heap.byte! h (quotient w twofiftysix^3)))

(define heap.word! heap.word-be!)

(define (dumpheap.set-endianness! which)
  (case which
    ((big) (set! heap.word! heap.word-be!))
    ((little) (set! heap.word! heap.word-el!))
    (else ???)))

(define (heap.byte! h b)
  (write-char (integer->char b) (heap.output-port h))
  (heap.top! h (+ 1 (heap.top h))))


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
  (before-dump-file h filename)
  (call-with-input-file filename
    (lambda (in)
      (do ((segment (read in) (read in))
	   (thunks  '() (cons (dump-segment! h segment) thunks)))
	  ((eof-object? segment)
	   (after-dump-file h filename)
	   (reverse thunks))))))

(define (before-dump-file h filename) #t)
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
  (dump-bytevector! h (string->bytevector s) $tag.string-typetag))

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
    (,$reg 1)				; argv into
    (,$setreg 2)			;   register 2
    (,$const (thunks))			; dummy list of thunks.
    (,$setreg 1)
    (,$.label 0)
    (,$reg 1)
    (,$op1 null?)			; (null? l)
    (,$branchf 2)
    (,$const (symbols))			; dummy list of symbols
    (,$setreg 1)
    (,$global go)
    ;(,$op1 break)
    (,$invoke 2)			; (go <list of symbols> argv)
    (,$.label 2)
    (,$save 2)
    (,$store 0 0)
    (,$store 1 1)
    (,$store 2 2)
    (,$setrtn 3)
    (,$reg 1)
    (,$op1 car)
    (,$invoke 0)			; ((car l))
    (,$.label 3)
    (,$.cont)
    (,$restore 2)
    (,$pop 2)
    (,$reg 1)
    (,$op1 cdr)
    (,$setreg 1)
    (,$branch 0)))			; (loop (cdr l))


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


(define (write-header h output-file)
  (delete-file output-file)
  (call-with-output-file output-file
    (lambda (out)

      (define (write-word w)
	(display (integer->char (quotient w twofiftysix^3)) out)
	(display (integer->char (quotient (remainder w twofiftysix^3) 
					  twofiftysix^2))
		 out)
	(display (integer->char (quotient (remainder w twofiftysix^2) 
					  twofiftysix))
		 out)
	(display (integer->char (remainder w twofiftysix)) out))

      (define (write-roots)
	(let ((assigned-roots (heap.roots h)))
	  (for-each (lambda (root-name)
		      (let ((probe (assq root-name assigned-roots)))
			(if probe
			    (write-word (cdr probe))
			    (write-word $imm.false))))
		    heap.root-names)))

      (write-word heap.version-number)
      (write-roots)
      (write-word (quotient (heap.top h) 4)))))


; This is a gross hack that happens to work very well.

(define (append-file-shell-command file-to-append file-to-append-to)

  (define (message)
    (display "You must execute the command") (newline)
    (display "   cat ") (display file-to-append) 
    (display " >> ") (display file-to-append-to) (newline)
    (display "to create the final heap image.") (newline))

  (case host-system
    ((chez larceny)
     (display "Creating final image in \"")
     (display file-to-append-to) (display "\"...") (newline)
     (if (zero? (system (string-append "cat " file-to-append " >> " 
				       file-to-append-to)))
	 (delete-file file-to-append)
	 (begin (display "Failed to create image!")
		(newline))))
    (else
     (message))))

; eof
; Copyright 1991 Lightship Software, Incorporated.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 11 June 1999 / wdc
;
; Asm/Sparc/pass5p2.sch -- Sparc machine assembler, top level

; Overrides the procedure of the same name in Asm/Common/pass5p1.sch.

(define (assembly-table) $sparc-assembly-table$)

; Controls listing of instructions during assembly.

(define listify? #f)

; Table of assembler procedures.

(define $sparc-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $sparc-assembly-table$ i proc)
  #t)

(define (list-instruction name instruction)
  (if listify?
      (begin (display list-indentation)
             (display "        ")
             (display name)
             (display (make-string (max (- 12 (string-length name)) 1)
                                   #\space))
             (if (not (null? (cdr instruction)))
                 (begin (write (cadr instruction))
                        (do ((operands (cddr instruction)
                                       (cdr operands)))
                            ((null? operands))
                            (write-char #\,)
                            (write (car operands)))))
             (newline)
             (flush-output-port))))

(define (list-label instruction)
  (if listify?
      (begin (display list-indentation)
             (write-char #\L)
             (write (cadr instruction))
             (newline))))

(define (list-lambda-start instruction)
  (list-instruction "lambda" (list $lambda '* (operand2 instruction)))
  (set! list-indentation (string-append list-indentation "|   ")))

(define (list-lambda-end)
  (set! list-indentation
        (substring list-indentation
                   0
                   (- (string-length list-indentation) 4))))

(define list-indentation "")

; Utilities

; Pseudo-instructions.

(define-instruction $.label
  (lambda (instruction as)
    (list-label instruction)
    (sparc.label as (make-asm-label as (operand1 instruction)))))

(define-instruction $.proc
  (lambda (instruction as)
    (list-instruction ".proc" instruction)
    #t))

(define-instruction $.proc-doc
  (lambda (instruction as)
    (list-instruction ".proc-doc" instruction)
    (add-documentation as (operand1 instruction))
    #t))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)
    #t))

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)
    #t))

(define-instruction $.end
  (lambda (instruction as)
    #t))

(define-instruction $.singlestep
  (lambda (instruction as)
    (let ((instr (car (as-source as))))
      
      (define (special?)
        (let ((op (operand0 instr)))
          (or (= op $.label)
              (= op $.proc)
              (= op $.cont)
              (= op $.align)
              (and (= op $load) (= 0 (operand1 instr))))))
      
      (define (readify-instr)
        (if (= (operand0 instr) $lambda)
            (list 'lambda '(...) (caddr instr) (cadddr instr))
            (car (readify-lap (list instr)))))
      
      (if (not (special?))
          (let ((repr   (format-object (readify-instr)))
                (funky? (= (operand0 instr) $restore)))
            (let ((o (emit-datum as repr)))
              (emit-singlestep-instr! as funky? 0 o)))))))


; Instructions.

(define-instruction $op1
  (lambda (instruction as)
    (list-instruction "op1" instruction)
    (emit-primop.1arg! as (operand1 instruction))))

(define-instruction $op2
  (lambda (instruction as)
    (list-instruction "op2" instruction)
    (emit-primop.2arg! as
                       (operand1 instruction)
                       (regname (operand2 instruction)))))

(define-instruction $op3
  (lambda (instruction as)
    (list-instruction "op3" instruction)
    (emit-primop.3arg! as
                       (operand1 instruction)
                       (regname (operand2 instruction))
                       (regname (operand3 instruction)))))

(define-instruction $op2imm
  (lambda (instruction as)
    (list-instruction "op2imm" instruction)
    (let ((op (case (operand1 instruction)
                ((+)    'internal:+/imm)
                ((-)    'internal:-/imm)
                ((fx+)  'internal:fx+/imm)
                ((fx-)  'internal:fx-/imm)
                ((fx=)  'internal:fx=/imm)
                ((fx<)  'internal:fx</imm)
                ((fx<=) 'internal:fx<=/imm)
                ((fx>)  'internal:fx>/imm)
                ((fx>=) 'internal:fx>=/imm)
                ((=:fix:fix)  'internal:=:fix:fix/imm)
                ((<:fix:fix)  'internal:<:fix:fix/imm)
                ((<=:fix:fix) 'internal:<=:fix:fix/imm)
                ((>:fix:fix)  'internal:>:fix:fix/imm)
                ((>=:fix:fix) 'internal:>=:fix:fix/imm)
                (else #f))))
      (if op
          (emit-primop.4arg! as op $r.result (operand2 instruction) $r.result)
          (begin
           (emit-constant->register as (operand2 instruction) $r.argreg2)
           (emit-primop.2arg! as
                              (operand1 instruction)
                              $r.argreg2))))))

(define-instruction $const
  (lambda (instruction as)
    (list-instruction "const" instruction)
    (emit-constant->register as (operand1 instruction) $r.result)))

(define-instruction $global
  (lambda (instruction as)
    (list-instruction "global" instruction)
    (emit-global->register! as
                            (emit-global as (operand1 instruction))
                            $r.result)))

(define-instruction $setglbl
  (lambda (instruction as)
    (list-instruction "setglbl" instruction)
    (emit-register->global! as
                            $r.result
                            (emit-global as (operand1 instruction)))))

; FIXME: A problem is that the listing is messed up because of the delayed
; assembly; somehow we should fix this by putting an identifying label
; in the listing and emitting this label later, with the code.

(define-instruction $lambda
  (lambda (instruction as)
    (let ((code-offset  #f)
          (const-offset #f))
      (list-lambda-start instruction)
      (assemble-nested-lambda as
                              (operand1 instruction)
                              (operand3 instruction)   ; documentation
                              (lambda (nested-as segment)
                                (set-constant! as code-offset (car segment))
                                (set-constant! as const-offset (cdr segment))))
      (list-lambda-end)
      (set! code-offset  (emit-codevector as 0))
      (set! const-offset (emit-constantvector as 0))
      (emit-lambda! as
                    code-offset
                    const-offset
                    (operand2 instruction)))))

(define-instruction $lexes
  (lambda (instruction as)
    (list-instruction "lexes" instruction)
    (emit-lexes! as (operand1 instruction))))

(define-instruction $args=
  (lambda (instruction as)
    (list-instruction "args=" instruction)
    (emit-args=! as (operand1 instruction))))

(define-instruction $args>=
  (lambda (instruction as)
    (list-instruction "args>=" instruction)
    (emit-args>=! as (operand1 instruction))))

(define-instruction $invoke
  (lambda (instruction as)
    (list-instruction "invoke" instruction)
    (emit-invoke as (operand1 instruction) #f $m.invoke-ex)))

(define-instruction $restore
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "restore" instruction)
         (emit-restore! as (operand1 instruction))))))

(define-instruction $pop
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "pop" instruction)
         (let ((next (next-instruction as)))
           (if (and (peephole-optimization)
                    (eqv? $return (operand0 next)))
               (begin (list-instruction "return" next)
                      (consume-next-instruction! as)
                      (emit-pop! as (operand1 instruction) #t))
               (emit-pop! as (operand1 instruction) #f)))))))

(define-instruction $stack
  (lambda (instruction as)
    (list-instruction "stack" instruction)
    (emit-load! as (operand1 instruction) $r.result)))

(define-instruction $setstk
  (lambda (instruction as)
    (list-instruction "setstk" instruction)
    (emit-store! as $r.result (operand1 instruction))))

(define-instruction $load
  (lambda (instruction as)
    (list-instruction "load" instruction)
    (emit-load! as (operand2 instruction) (regname (operand1 instruction)))))

(define-instruction $store
  (lambda (instruction as)
    (list-instruction "store" instruction)
    (emit-store! as (regname (operand1 instruction)) (operand2 instruction))))

(define-instruction $lexical
  (lambda (instruction as)
    (list-instruction "lexical" instruction)
    (emit-lexical! as (operand1 instruction) (operand2 instruction))))

(define-instruction $setlex
  (lambda (instruction as)
    (list-instruction "setlex" instruction)
    (emit-setlex! as (operand1 instruction) (operand2 instruction))))

(define-instruction $reg
  (lambda (instruction as)
    (list-instruction "reg" instruction)
    (emit-register->register! as (regname (operand1 instruction)) $r.result)))

(define-instruction $setreg
  (lambda (instruction as)
    (list-instruction "setreg" instruction)
    (emit-register->register! as $r.result (regname (operand1 instruction)))))

(define-instruction $movereg
  (lambda (instruction as)
    (list-instruction "movereg" instruction)
    (emit-register->register! as 
                              (regname (operand1 instruction))
                              (regname (operand2 instruction)))))

(define-instruction $return
  (lambda (instruction as)
    (list-instruction "return" instruction)
    (emit-return! as)))

(define-instruction $reg/return
  (lambda (instruction as)
    (list-instruction "reg/return" instruction)
    (emit-return-reg! as (regname (operand1 instruction)))))

(define-instruction $const/return
  (lambda (instruction as)
    (list-instruction "const/return" instruction)
    (emit-return-const! as (operand1 instruction))))

(define-instruction $nop
  (lambda (instruction as)
    (list-instruction "nop" instruction)))

(define-instruction $save
  (lambda (instruction as)
    (if (not (negative? (operand1 instruction)))
        (begin
         (list-instruction "save" instruction)
         (let* ((n (operand1 instruction))
                (v (make-vector (+ n 1) #t)))
           (emit-save0! as n)
           (if (peephole-optimization)
               (let loop ((instruction (next-instruction as)))
                 (if (eqv? $store (operand0 instruction))
                     (begin (list-instruction "store" instruction)
                            (emit-store! as
                                         (regname (operand1 instruction))
                                         (operand2 instruction))
                            (consume-next-instruction! as)
                            (vector-set! v (operand2 instruction) #f)
                            (loop (next-instruction as))))))
           (emit-save1! as v))))))

(define-instruction $setrtn
  (lambda (instruction as)
    (list-instruction "setrtn" instruction)
    (emit-setrtn! as (make-asm-label as (operand1 instruction)))))

(define-instruction $apply
  (lambda (instruction as)
    (list-instruction "apply" instruction)
    (emit-apply! as
                 (regname (operand1 instruction))
                 (regname (operand2 instruction)))))

(define-instruction $jump
  (lambda (instruction as)
    (list-instruction "jump" instruction)
    (emit-jump! as
                (operand1 instruction)
                (make-asm-label as (operand2 instruction)))))

(define-instruction $skip
  (lambda (instruction as)
    (list-instruction "skip" instruction)
    (emit-branch! as #f (make-asm-label as (operand1 instruction)))))

(define-instruction $branch
  (lambda (instruction as)
    (list-instruction "branch" instruction)
    (emit-branch! as #t (make-asm-label as (operand1 instruction)))))

(define-instruction $branchf
  (lambda (instruction as)
    (list-instruction "branchf" instruction)
    (emit-branchf! as (make-asm-label as (operand1 instruction)))))

(define-instruction $check
  (lambda (instruction as)
    (list-instruction "check" instruction)
    (if (not (unsafe-code))
        (emit-check! as $r.result
                        (make-asm-label as (operand4 instruction))
                        (list (regname (operand1 instruction))
                              (regname (operand2 instruction))
                              (regname (operand3 instruction)))))))

(define-instruction $trap
  (lambda (instruction as)
    (list-instruction "trap" instruction)
    (emit-trap! as
                (regname (operand1 instruction))
                (regname (operand2 instruction))
                (regname (operand3 instruction))
                (operand4 instruction))))

(define-instruction $const/setreg
  (lambda (instruction as)
    (list-instruction "const/setreg" instruction)
    (let ((x (operand1 instruction))
          (r (operand2 instruction)))
      (if (hwreg? r)
          (emit-constant->register as x (regname r))
          (begin (emit-constant->register as x $r.tmp0)
                 (emit-register->register! as $r.tmp0 (regname r)))))))

; Operations introduced by the peephole optimizer.

(define (peep-regname r)
  (if (eq? r 'RESULT) $r.result (regname r)))

(define-instruction $reg/op1/branchf
  (lambda (instruction as)
    (list-instruction "reg/op1/branchf" instruction)
    (emit-primop.3arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (make-asm-label as (operand3 instruction)))))

(define-instruction $reg/op2/branchf
  (lambda (instruction as)
    (list-instruction "reg/op2/branchf" instruction)
    (emit-primop.4arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (peep-regname (operand3 instruction))
                       (make-asm-label as (operand4 instruction)))))

(define-instruction $reg/op2imm/branchf
  (lambda (instruction as)
    (list-instruction "reg/op2imm/branchf" instruction)
    (emit-primop.4arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (operand3 instruction)
                       (make-asm-label as (operand4 instruction)))))

; These three are like the corresponding branchf sequences except that
; there is a strong prediction that the branch will not be taken.

(define-instruction $reg/op1/check
  (lambda (instruction as)
    (list-instruction "reg/op1/check" instruction)
    (emit-primop.4arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (make-asm-label as (operand3 instruction))
                       (map peep-regname (operand4 instruction)))))

(define-instruction $reg/op2/check
  (lambda (instruction as)
    (list-instruction "reg/op2/check" instruction)
    (emit-primop.5arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (peep-regname (operand3 instruction))
                       (make-asm-label as (operand4 instruction))
                       (map peep-regname (operand5 instruction)))))

(define-instruction $reg/op2imm/check
  (lambda (instruction as)
    (list-instruction "reg/op2imm/check" instruction)
    (emit-primop.5arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (operand3 instruction)
                       (make-asm-label as (operand4 instruction))
                       (map peep-regname (operand5 instruction)))))

;

(define-instruction $reg/op1/setreg
  (lambda (instruction as)
    (list-instruction "reg/op1/setreg" instruction)
    (emit-primop.3arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (peep-regname (operand3 instruction)))))

(define-instruction $reg/op2/setreg
  (lambda (instruction as)
    (list-instruction "reg/op2/setreg" instruction)
    (emit-primop.4arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (peep-regname (operand3 instruction))
                       (peep-regname (operand4 instruction)))))

(define-instruction $reg/op2imm/setreg
  (lambda (instruction as)
    (list-instruction "reg/op2imm/setreg" instruction)
    (emit-primop.4arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (operand3 instruction)
                       (peep-regname (operand4 instruction)))))

(define-instruction $reg/op3 
  (lambda (instruction as)
    (list-instruction "reg/op3" instruction)
    (emit-primop.4arg! as
                       (operand1 instruction)
                       (peep-regname (operand2 instruction))
                       (peep-regname (operand3 instruction))
                       (peep-regname (operand4 instruction)))))

(define-instruction $reg/branchf
  (lambda (instruction as)
    (list-instruction "reg/branchf" instruction)
    (emit-branchfreg! as 
                      (regname (operand1 instruction))
                      (make-asm-label as (operand2 instruction)))))

(define-instruction $setrtn/branch
  (lambda (instruction as)
    (list-instruction "setrtn/branch" instruction)
    (emit-branch-with-setrtn! as (make-asm-label as (operand1 instruction)))))

(define-instruction $setrtn/invoke
  (lambda (instruction as)
    (list-instruction "setrtn/invoke" instruction)
    (emit-invoke as (operand1 instruction) #t $m.invoke-ex)))

(define-instruction $global/setreg
  (lambda (instruction as)
    (list-instruction "global/setreg" instruction)
    (emit-global->register! as
                            (emit-global as (operand1 instruction))
                            (regname (operand2 instruction)))))

(define-instruction $global/invoke
  (lambda (instruction as)
    (list-instruction "global/invoke" instruction)
    (emit-load-global as
                      (emit-global as (operand1 instruction))
                      $r.result
                      #f)
    (emit-invoke as (operand2 instruction) #f $m.global-invoke-ex)))

(define-instruction $reg/setglbl
  (lambda (instruction as)
    (list-instruction "reg/setglbl" instruction)
    (emit-register->global! as
                            (regname (operand1 instruction))
                            (emit-global as (operand2 instruction)))))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 9 May 1999.
;
; Asm/Sparc/peepopt.sch -- MAL peephole optimizer, for the SPARC assembler.
;
; The procedure `peep' is called on the as structure before every
; instruction is assembled.  It may replace the prefix of the instruction
; stream by some other instruction sequence.
;
; Invariant: if the peephole optimizer doesn't change anything, then 
;
;  (let ((x (as-source as)))
;    (peep as)
;    (eq? x (as-source as)))     => #t
;
; Note this still isn't right -- it should be integrated with pass5p2 --
; but it's a step in the right direction.

(define *peephole-table* (make-vector *number-of-mnemonics* #f))

(define (define-peephole n p)
  (vector-set! *peephole-table* n p)
  (unspecified))

(define (peep as)
  (let ((t0 (as-source as)))
    (if (not (null? t0))
        (let ((i1 (car t0)))
          (let ((p (vector-ref *peephole-table* (car i1))))
            (if p
                (let* ((t1 (if (null? t0) t0 (cdr t0)))
                       (i2 (if (null? t1) '(-1 0 0 0) (car t1)))
                       (t2 (if (null? t1) t1 (cdr t1)))
                       (i3 (if (null? t2) '(-1 0 0 0) (car t2)))
                       (t3 (if (null? t2) t2 (cdr t2))))
                  (p as i1 i2 i3 t1 t2 t3))))))))

(define-peephole $reg
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $return)
           (reg-return as i1 i2 t2))
          ((= (car i2) $setglbl)
           (reg-setglbl as i1 i2 t2))
          ((= (car i2) $op1)
           (cond ((= (car i3) $setreg)
                  (reg-op1-setreg as i1 i2 i3 t2 t3))
                 ((= (car i3) $branchf)
                  (reg-op1-branchf as i1 i2 i3 t3))
                 ((= (car i3) $check)
                  (reg-op1-check as i1 i2 i3 t3))
                 (else
                  (reg-op1 as i1 i2 t2))))
          ((= (car i2) $op2)
           (cond ((= (car i3) $setreg)
                  (reg-op2-setreg as i1 i2 i3 t2 t3))
                 ((= (car i3) $branchf)
                  (reg-op2-branchf as i1 i2 i3 t3))
                 ((= (car i3) $check)
                  (reg-op2-check as i1 i2 i3 t3))
                 (else
                  (reg-op2 as i1 i2 t2))))
          ((= (car i2) $op2imm)
           (cond ((= (car i3) $setreg)
                  (reg-op2imm-setreg as i1 i2 i3 t2 t3))
                 ((= (car i3) $branchf)
                  (reg-op2imm-branchf as i1 i2 i3 t3))
                 ((= (car i3) $check)
                  (reg-op2imm-check as i1 i2 i3 t3))
                 (else
                  (reg-op2imm as i1 i2 t2))))
          ((= (car i2) $op3)
           (reg-op3 as i1 i2 t2))
          ((= (car i2) $setreg)
           (reg-setreg as i1 i2 t2))
          ((= (car i2) $branchf)
           (reg-branchf as i1 i2 t2)))))

(define-peephole $op1
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
           (op1-branchf as i1 i2 t2))
          ((= (car i2) $setreg)
           (op1-setreg as i1 i2 t2))
          ((= (car i2) $check)
           (op1-check as i1 i2 t2)))))

(define-peephole $op2
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
           (op2-branchf as i1 i2 t2))
          ((= (car i2) $setreg)
           (op2-setreg as i1 i2 t2))
          ((= (car i2) $check)
           (op2-check as i1 i2 t2)))))

(define-peephole $op2imm
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
           (op2imm-branchf as i1 i2 t2))
          ((= (car i2) $setreg)
           (op2imm-setreg as i1 i2 t2))
          ((= (car i2) $check)
           (op2imm-check as i1 i2 t2)))))

(define-peephole $const
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (const-setreg as i1 i2 t2))
          ((= (car i2) $op2)
           (const-op2 as i1 i2 t2))
          ((= (car i2) $return)
           (const-return as i1 i2 t2)))))

(define-peephole $setrtn
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branch)
           (cond ((= (car i3) $.align)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $.label)
                               (setrtn-branch as i1 i2 i3 i4 t4))))))))
          ((= (car i2) $invoke)
           (cond ((= (car i3) $.align)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $.label)
                               (setrtn-invoke as i1 i2 i3 i4 t4)))))))))))

(define-peephole $branch
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $.align)
           (cond ((= (car i3) $.label)
                  (branch-and-label as i1 i2 i3 t3)))))))

(define-peephole $global
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $setreg)
           (global-setreg as i1 i2 t2))
          ((= (car i2) $invoke)
           (global-invoke as i1 i2 t2))
          ((= (car i2) $setrtn)
           (cond ((= (car i3) $invoke)
                  (global-setrtn-invoke as i1 i2 i3 t3)))))))

(define-peephole $reg/op1/check
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $reg)
           (cond ((= (car i3) $op1)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $setreg)
                               (reg/op1/check-reg-op1-setreg
                                as i1 i2 i3 i4 t4)))))))))))

(define-peephole $reg/op2/check
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $reg)
           (cond ((= (car i3) $op2imm)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $check)
                               (reg/op2/check-reg-op2imm-check
                                as i1 i2 i3 i4 t4)))))))))))

; Worker procedures.

(define (reg-return as i:reg i:return tail)
  (let ((rs (operand1 i:reg)))
    (if (hwreg? rs)
        (as-source! as (cons (list $reg/return rs) tail)))))

(define (reg-op1-setreg as i:reg i:op1 i:setreg tail-1 tail)
  (let ((rs (operand1 i:reg))
        (rd (operand1 i:setreg))
        (op (operand1 i:op1)))
    (if (hwreg? rs)
        (if (hwreg? rd)
            (peep-reg/op1/setreg as op rs rd tail)
            (peep-reg/op1/setreg as op rs 'RESULT tail-1)))))

(define (reg-op1 as i:reg i:op1 tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1)))
    (if (hwreg? rs)
        (peep-reg/op1/setreg as op rs 'RESULT tail))))

(define (op1-setreg as i:op1 i:setreg tail)
  (let ((op (operand1 i:op1))
        (rd (operand1 i:setreg)))
    (if (hwreg? rd)
        (peep-reg/op1/setreg as op 'RESULT rd tail))))

(define (peep-reg/op1/setreg as op rs rd tail)
  (let ((op (case op
              ((car)               'internal:car)
              ((cdr)               'internal:cdr)
              ((car:pair)          'internal:car:pair)
              ((cdr:pair)          'internal:cdr:pair)
              ((cell-ref)          'internal:cell-ref)
              ((vector-length)     'internal:vector-length)
              ((vector-length:vec) 'internal:vector-length:vec)
              ((string-length)     'internal:string-length)
              ((--)                'internal:--)
              ((fx--)              'internal:fx--)
              ((fxpositive?)       'internal:fxpositive?)
              ((fxnegative?)       'internal:fxnegative?)
              ((fxzero?)           'internal:fxzero?)
              (else #f))))
    (if op
        (as-source! as (cons (list $reg/op1/setreg op rs rd) tail)))))

(define (reg-op2-setreg as i:reg i:op2 i:setreg tail-1 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op  (operand1 i:op2))
        (rd  (operand1 i:setreg)))
    (if (hwreg? rs1)
        (if (hwreg? rd)
            (peep-reg/op2/setreg as op rs1 rs2 rd tail)
            (peep-reg/op2/setreg as op rs1 rs2 'RESULT tail-1)))))

(define (reg-op2 as i:reg i:op2 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op  (operand1 i:op2)))
    (if (hwreg? rs1)
        (peep-reg/op2/setreg as op rs1 rs2 'RESULT tail))))

(define (op2-setreg as i:op2 i:setreg tail)
  (let ((op  (operand1 i:op2))
        (rs2 (operand2 i:op2))
        (rd  (operand1 i:setreg)))
    (if (hwreg? rd)
        (peep-reg/op2/setreg as op 'RESULT rs2 rd tail))))

(define (peep-reg/op2/setreg as op rs1 rs2 rd tail)
  (let ((op (case op
              ((+)                  'internal:+)
              ((-)                  'internal:-)
              ((fx+)                'internal:fx+)
              ((fx-)                'internal:fx-)
              ((fx=)                'internal:fx=)
              ((fx>)                'internal:fx>)
              ((fx>=)               'internal:fx>=)
              ((fx<)                'internal:fx<)
              ((fx<=)               'internal:fx<=)
              ((eq?)                'internal:eq?)
              ((cons)               'internal:cons)
              ((vector-ref)         'internal:vector-ref)
              ((vector-ref:trusted) 'internal:vector-ref:trusted)
              ((string-ref)         'internal:string-ref)
              ((set-car!)           'internal:set-car!)
              ((set-cdr!)           'internal:set-cdr!)
              ((cell-set!)          'internal:cell-set!)
              (else #f))))
    (if op
        (as-source! as (cons (list $reg/op2/setreg op rs1 rs2 rd) tail)))))

(define (reg-op2imm-setreg as i:reg i:op2imm i:setreg tail-1 tail)
  (let ((rs  (operand1 i:reg))
        (imm (operand2 i:op2imm))
        (op  (operand1 i:op2imm))
        (rd  (operand1 i:setreg)))
    (if (hwreg? rs)
        (if (hwreg? rd)
            (peep-reg/op2imm/setreg as op rs imm rd tail)
            (peep-reg/op2imm/setreg as op rs imm 'RESULT tail-1)))))

(define (reg-op2imm as i:reg i:op2imm tail)
  (let ((rs  (operand1 i:reg))
        (imm (operand2 i:op2imm))
        (op  (operand1 i:op2imm)))
    (if (hwreg? rs)
        (peep-reg/op2imm/setreg as op rs imm 'RESULT tail))))

(define (op2imm-setreg as i:op2imm i:setreg tail)
  (let ((op  (operand1 i:op2imm))
        (imm (operand2 i:op2imm))
        (rd  (operand1 i:setreg)))
    (if (hwreg? rd)
        (peep-reg/op2imm/setreg as op 'RESULT imm rd tail))))

(define (peep-reg/op2imm/setreg as op rs imm rd tail)
  (let ((op (case op
              ((+)          'internal:+/imm)
              ((-)          'internal:-/imm)
              ((fx+)        'internal:fx+/imm)
              ((fx-)        'internal:fx-/imm)
              ((fx=)        'internal:fx=/imm)
              ((fx<)        'internal:fx</imm)
              ((fx<=)       'internal:fx<=/imm)
              ((fx>)        'internal:fx>/imm)
              ((fx>=)       'internal:fx>=/imm)
              ((eq?)        'internal:eq?/imm)
              ((vector-ref) 'internal:vector-ref/imm)
              ((string-ref) 'internal:string-ref/imm)
              (else #f))))
    (if op
        (as-source! as (cons (list $reg/op2imm/setreg op rs imm rd) tail)))))

(define (reg-op1-branchf as i:reg i:op1 i:branchf tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1))
        (L  (operand1 i:branchf)))
    (if (hwreg? rs)
        (peep-reg/op1/branchf as op rs L tail))))

(define (op1-branchf as i:op1 i:branchf tail)
  (let ((op (operand1 i:op1))
        (L  (operand1 i:branchf)))
    (peep-reg/op1/branchf as op 'RESULT L tail)))

(define (peep-reg/op1/branchf as op rs L tail)
  (let ((op (case op
              ((null?)       'internal:branchf-null?)
              ((pair?)       'internal:branchf-pair?)
              ((zero?)       'internal:branchf-zero?)
              ((eof-object?) 'internal:branchf-eof-object?)
              ((fixnum?)     'internal:branchf-fixnum?)
              ((char?)       'internal:branchf-char?)
              ((fxzero?)     'internal:branchf-fxzero?)
              ((fxnegative?) 'internal:branchf-fxnegative?)
              ((fxpositive?) 'internal:branchf-fxpositive?)
              (else #f))))
    (if op
        (as-source! as (cons (list $reg/op1/branchf op rs L) tail)))))

(define (reg-op2-branchf as i:reg i:op2 i:branchf tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op  (operand1 i:op2))
        (L   (operand1 i:branchf)))
    (if (hwreg? rs1)
        (peep-reg/op2/branchf as op rs1 rs2 L tail))))

(define (op2-branchf as i:op2 i:branchf tail)
  (let ((op  (operand1 i:op2))
        (rs2 (operand2 i:op2))
        (L   (operand1 i:branchf)))
    (peep-reg/op2/branchf as op 'RESULT rs2 L tail)))

(define (peep-reg/op2/branchf as op rs1 rs2 L tail)
  (let ((op (case op
              ((<)       'internal:branchf-<)
              ((>)       'internal:branchf->)
              ((>=)      'internal:branchf->=)
              ((<=)      'internal:branchf-<=)
              ((=)       'internal:branchf-=)
              ((eq?)     'internal:branchf-eq?)
              ((char=?)  'internal:branchf-char=?)
              ((char>=?) 'internal:branchf-char>=?)
              ((char>?)  'internal:branchf-char>?)
              ((char<=?) 'internal:branchf-char<=?)
              ((char<?)  'internal:branchf-char<?)
              ((fx=)     'internal:branchf-fx=)
              ((fx>)     'internal:branchf-fx>)
              ((fx>=)    'internal:branchf-fx>=)
              ((fx<)     'internal:branchf-fx<)
              ((fx<=)    'internal:branchf-fx<=)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op2/branchf op rs1 rs2 L)
                          tail)))))

(define (reg-op2imm-branchf as i:reg i:op2imm i:branchf tail)
  (let ((rs  (operand1 i:reg))
        (imm (operand2 i:op2imm))
        (op  (operand1 i:op2imm))
        (L   (operand1 i:branchf)))
    (if (hwreg? rs)
        (peep-reg/op2imm/branchf as op rs imm L tail))))

(define (op2imm-branchf as i:op2imm i:branchf tail)
  (let ((op  (operand1 i:op2imm))
        (imm (operand2 i:op2imm))
        (L   (operand1 i:branchf)))
    (peep-reg/op2imm/branchf as op 'RESULT imm L tail)))

(define (peep-reg/op2imm/branchf as op rs imm L tail)
  (let ((op (case op
              ((<)       'internal:branchf-</imm)
              ((>)       'internal:branchf->/imm)
              ((>=)      'internal:branchf->=/imm)
              ((<=)      'internal:branchf-<=/imm)
              ((=)       'internal:branchf-=/imm)
              ((eq?)     'internal:branchf-eq?/imm)
              ((char=?)  'internal:branchf-char=?/imm)
              ((char>=?) 'internal:branchf-char>=?/imm)
              ((char>?)  'internal:branchf-char>?/imm)
              ((char<=?) 'internal:branchf-char<=?/imm)
              ((char<?)  'internal:branchf-char<?/imm)
              ((fx=)     'internal:branchf-fx=/imm)
              ((fx>)     'internal:branchf-fx>/imm)
              ((fx>=)    'internal:branchf-fx>=/imm)
              ((fx<)     'internal:branchf-fx</imm)
              ((fx<=)    'internal:branchf-fx<=/imm)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op2imm/branchf op rs imm L)
                          tail)))))

; Check optimization.

(define (reg-op1-check as i:reg i:op1 i:check tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1)))
    (if (hwreg? rs)
        (peep-reg/op1/check as
                            op
                            rs
                            (operand4 i:check)
                            (list (operand1 i:check)
                                  (operand2 i:check)
                                  (operand3 i:check))
                            tail))))

(define (op1-check as i:op1 i:check tail)
  (let ((op (operand1 i:op1)))
    (peep-reg/op1/check as
                        op
                        'RESULT
                        (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (peep-reg/op1/check as op rs L1 liveregs tail)
  (let ((op (case op
              ((fixnum?)      'internal:check-fixnum?)
              ((pair?)        'internal:check-pair?)
              ((vector?)      'internal:check-vector?)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op1/check op rs L1 liveregs)
                          tail)))))

(define (reg-op2-check as i:reg i:op2 i:check tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (if (hwreg? rs1)
        (peep-reg/op2/check as
                            op
                            rs1
                            rs2
                            (operand4 i:check)
                            (list (operand1 i:check)
                                  (operand2 i:check)
                                  (operand3 i:check))
                            tail))))

(define (op2-check as i:op2 i:check tail)
  (let ((rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (peep-reg/op2/check as
                        op
                        'RESULT
                        rs2
                        (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (peep-reg/op2/check as op rs1 rs2 L1 liveregs tail)
  (let ((op (case op
              ((<:fix:fix)   'internal:check-<:fix:fix)
              ((<=:fix:fix)  'internal:check-<=:fix:fix)
              ((>=:fix:fix)  'internal:check->=:fix:fix)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op2/check op rs1 rs2 L1 liveregs)
                          tail)))))

(define (reg-op2imm-check as i:reg i:op2imm i:check tail)
  (let ((rs1 (operand1 i:reg))
        (op (operand1 i:op2imm))
        (imm (operand2 i:op2imm)))
    (if (hwreg? rs1)
        (peep-reg/op2imm/check as
                               op
                               rs1
                               imm
                               (operand4 i:check)
                               (list (operand1 i:check)
                                     (operand2 i:check)
                                     (operand3 i:check))
                               tail))))

(define (op2imm-check as i:op2imm i:check tail)
  (let ((op (operand1 i:op2imm))
        (imm (operand2 i:op2imm)))
    (peep-reg/op2imm/check as
                           op
                           'RESULT
                           imm
                           (operand4 i:check)
                           (list (operand1 i:check)
                                 (operand2 i:check)
                                 (operand3 i:check))
                           tail)))

(define (peep-reg/op2imm/check as op rs1 imm L1 liveregs tail)
  (let ((op (case op
              ((<:fix:fix)   'internal:check-<:fix:fix/imm)
              ((<=:fix:fix)  'internal:check-<=:fix:fix/imm)
              ((>=:fix:fix)  'internal:check->=:fix:fix/imm)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op2imm/check op rs1 imm L1 liveregs)
                          tail)))))

(define (reg/op1/check-reg-op1-setreg as i:ro1check i:reg i:op1 i:setreg tail)
  (let ((o1 (operand1 i:ro1check))
        (r1 (operand2 i:ro1check))
        (r2 (operand1 i:reg))
        (o2 (operand1 i:op1))
        (r3 (operand1 i:setreg)))
    (if (and (eq? o1 'internal:check-vector?)
             (eq? r1 r2)
             (eq? o2 'vector-length:vec)
             (hwreg? r1)
             (hwreg? r3))
        (as-source! as
                    (cons (list $reg/op2/check
                                'internal:check-vector?/vector-length:vec
                                r1
                                r3
                                (operand3 i:ro1check)
                                (operand4 i:ro1check))
                          tail)))))

; Range checks of the form 0 <= i < n can be performed by a single check.
; This peephole optimization recognizes
;         reg     rs1
;         op2     <:fix:fix,rs2
;         check   r1,r2,r3,L
;         reg     rs1                     ; must match earlier reg
;         op2imm  >=:fix:fix,0
;         check   r1,r2,r3,L              ; label must match earlier check

(define (reg/op2/check-reg-op2imm-check
         as i:ro2check i:reg i:op2imm i:check tail)
  (let ((o1   (operand1 i:ro2check))
        (rs1  (operand2 i:ro2check))
        (rs2  (operand3 i:ro2check))
        (L1   (operand4 i:ro2check))
        (live (operand5 i:ro2check))
        (rs3  (operand1 i:reg))
        (o2   (operand1 i:op2imm))
        (x    (operand2 i:op2imm))
        (L2   (operand4 i:check)))
    (if (and (eq? o1 'internal:check-<:fix:fix)
             (eq? o2 '>=:fix:fix)
             (eq? rs1 rs3)
             (eq? x 0)
             (eq? L1 L2))
        (as-source! as
                    (cons (list $reg/op2/check 'internal:check-range
                                                rs1 rs2 L1 live)
                          tail)))))

; End of check optimization.

(define (reg-op3 as i:reg i:op3 tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op3))
        (rs3 (operand3 i:op3))
        (op  (operand1 i:op3)))
    (if (hwreg? rs1)
        (let ((op (case op
                    ((vector-set!) 'internal:vector-set!)
                    ((string-set!) 'internal:string-set!)
                    (else #f))))
          (if op
              (as-source! as (cons (list $reg/op3 op rs1 rs2 rs3) tail)))))))

; Reg-setreg is not restricted to hardware registers, as $movereg is 
; a standard instruction.

(define (reg-setreg as i:reg i:setreg tail)
  (let ((rs (operand1 i:reg))
        (rd (operand1 i:setreg)))
    (if (= rs rd)
        (as-source! as tail)
        (as-source! as (cons (list $movereg rs rd) tail)))))

(define (reg-branchf as i:reg i:branchf tail)
  (let ((rs (operand1 i:reg))
        (L  (operand1 i:branchf)))
    (if (hwreg? rs)
        (as-source! as (cons (list $reg/branchf rs L) tail)))))

(define (const-setreg as i:const i:setreg tail)
  (let ((c  (operand1 i:const))
        (rd (operand1 i:setreg)))
    (if (hwreg? rd)
        (as-source! as (cons (list $const/setreg c rd) tail)))))

; Make-vector on vectors of known short length.

(define (const-op2 as i:const i:op2 tail)
  (let ((vn '#(make-vector:0 make-vector:1 make-vector:2 make-vector:3
               make-vector:4 make-vector:5 make-vector:6 make-vector:7
               make-vector:8 make-vector:9))
        (c  (operand1 i:const))
        (op (operand1 i:op2))
        (r  (operand2 i:op2)))
    (if (and (eq? op 'make-vector)
             (fixnum? c)
             (<= 0 c 9))
        (as-source! as (cons (list $op2 (vector-ref vn c) r) tail)))))

; Constants that can be synthesized in a single instruction can be
; moved into RESULT in the delay slot of the return instruction.

(define (const-return as i:const i:return tail)
  (let ((c (operand1 i:const)))
    (if (or (and (number? c) (immediate-int? c))
            (null? c)
            (boolean? c))
        (as-source! as (cons (list $const/return c) tail)))))

; This allows the use of hardware 'call' instructions.
;    (setrtn Lx)
;    (branch Ly k)
;    (.align k)            Ignored on SPARC
;    (.label Lx)
; => (setrtn/branch Ly k)
;    (.label Lx)

(define (setrtn-branch as i:setrtn i:branch i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (branch-ops   (cdr i:branch))
        (label        (operand1 i:label)))
    (if (= return-label label)
        (as-source! as (cons (cons $setrtn/branch branch-ops)
                             (cons i:label
                                   tail))))))

; Ditto for 'invoke'.
;
; Disabled because it does _not_ pay off on the SPARC currently -- 
; probably, the dependency created between 'jmpl' and 'st' is not 
; handled well on the test machine (an Ultrasparc).  Might work 
; better if the return address were to be kept in a register always.

(define (setrtn-invoke as i:setrtn i:invoke i:align i:label tail)
  (let ((return-label (operand1 i:setrtn))
        (invoke-ops   (operand1 i:invoke))
        (label        (operand1 i:label)))
    (if (and #f				; DISABLED
             (= return-label label))
        (as-source! as (cons (cons $setrtn/invoke invoke-ops)
                             (cons i:label
                                   tail))))))

; Gets rid of spurious branch-to-next-instruction
;    (branch Lx k)
;    (.align y)
;    (.label Lx)
; => (.align y)
;    (.label Lx)

(define (branch-and-label as i:branch i:align i:label tail)
  (let ((branch-label (operand1 i:branch))
        (label        (operand1 i:label)))
    (if (= branch-label label)
        (as-source! as (cons i:align (cons i:label tail))))))

(define (global-setreg as i:global i:setreg tail)
  (let ((global (operand1 i:global))
        (rd     (operand1 i:setreg)))
    (if (hwreg? rd)
        (as-source! as (cons (list $global/setreg global rd) tail)))))

; Obscure guard: unsafe-code = #t implies that global/invoke will not
; check the value of the global variable, yet unsafe-code and
; catch-undefined-globals are supposed to be independent.

(define (global-invoke as i:global i:invoke tail)
  (let ((global (operand1 i:global))
        (argc   (operand1 i:invoke)))
    (if (not (and (unsafe-code) (catch-undefined-globals)))
        (as-source! as (cons (list $global/invoke global argc) tail)))))

; Obscure guard: see comment for previous procedure.
; FIXME!  This implementation is temporary until setrtn-invoke is enabled.

(define (global-setrtn-invoke as i:global i:setrtn i:invoke tail)
  (let ((global (operand1 i:global))
        (argc   (operand1 i:invoke)))
    (if (not (and (unsafe-code) (catch-undefined-globals)))
        (as-source! as (cons i:setrtn 
                             (cons (list $global/invoke global argc)
                                   tail))))))

(define (reg-setglbl as i:reg i:setglbl tail)
  (let ((rs     (operand1 i:reg))
        (global (operand1 i:setglbl)))
    (if (hwreg? rs)
        (as-source! as (cons (list $reg/setglbl rs global) tail)))))



; Test code

(define (peeptest istream)
  (let ((as (make-assembly-structure istream)))
    (let loop ((l '()))
      (if (null? (as-source as))
          (reverse l)
          (begin (peep as)
                 (let ((a (car (as-source as))))
                   (as-source! as (cdr (as-source as)))
                   (loop (cons a l))))))))


; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; SPARC assembler machine parameters & utility procedures.
;
; 13 May 1999 / wdc

; Round up to nearest 8.

(define (roundup8 n)
  (* (quotient (+ n 7) 8) 8))

; Given an integer code for a register, return its register label.
; This register label is the register number for a h.w. register and the
; offsets from GLOBALS[ r0 ] for a s.w. register.

(define regname
  (let ((v (vector $r.reg0  $r.reg1  $r.reg2  $r.reg3  $r.reg4  $r.reg5
                   $r.reg6  $r.reg7  $r.reg8  $r.reg9  $r.reg10 $r.reg11
                   $r.reg12 $r.reg13 $r.reg14 $r.reg15 $r.reg16 $r.reg17
                   $r.reg18 $r.reg19 $r.reg20 $r.reg21 $r.reg22 $r.reg23
                   $r.reg24 $r.reg25 $r.reg26 $r.reg27 $r.reg28 $r.reg29
                   $r.reg30 $r.reg31)))
    (lambda (r)
      (vector-ref v r))))

; Is a general-purpose register mapped to a hardware register?
; This is fragile! FIXME.

(define (hardware-mapped? r)
  (or (and (>= r $r.reg0) (<= r $r.reg7))
      (= r $r.argreg2)
      (= r $r.argreg3)
      (= r $r.result)
      (= r $r.g0)
      (= r $r.tmp0)
      (= r $r.tmp1)
      (= r $r.tmp2)))

; Used by peephole optimizer

(define (hwreg? x)
  (<= 0 x 7))

(define (immediate-int? x)
  (and (exact? x)
       (integer? x)
       (<= -1024 x 1023)))

; Given an exact integer, can it be represented as a fixnum?

(define fixnum-range?
  (let ((n-two^29  (- (expt 2 29)))
        (two^29-1 (- (expt 2 29) 1)))
    (lambda (x)
      (<= n-two^29 x two^29-1))))

; Does the integer x fit in the immediate field of an instruction?

(define (immediate-literal? x)
  (<= -4096 x 4095))

; Return the offset in the %GLOBALS table of the given memory-mapped 
; register. A memory-mapped register is represented by an integer which 
; is its offet, so just return the value.

(define (swreg-global-offset r) r)

; Return a bit representation of a character constant.

(define (char->immediate c)
  (+ (* (char->integer c) 65536) $imm.character))

; Convert an integer to a fixnum.

(define (thefixnum x) (* x 4))

; The offset of data slot 'n' within a procedure structure, not adjusting 
; for tag. The proc is a header followed by code, const, and then data.

(define (procedure-slot-offset n)
  (+ 12 (* n 4)))

; Src is a register, hwreg is a hardware register. If src is a
; hardware register, return src. Otherwise, emit an instruction to load
; src into hwreg and return hwreg.

(define (force-hwreg! as src hwreg)
  (if (hardware-mapped? src)
      src
      (emit-load-reg! as src hwreg)))

; Given an arbitrary constant opd, generate code to load it into a
; register r.

(define (emit-constant->register as opd r)
  (cond ((and (integer? opd) (exact? opd))
         (if (fixnum-range? opd)	
             (emit-immediate->register! as (thefixnum opd) r)
             (emit-const->register! as (emit-datum as opd) r)))
        ((boolean? opd)
         (emit-immediate->register! as
                                    (if (eq? opd #t)
                                        $imm.true
                                        $imm.false)
                                    r))
        ((equal? opd (eof-object))
         (emit-immediate->register! as $imm.eof r))
        ((equal? opd (unspecified))
         (emit-immediate->register! as $imm.unspecified r))
        ((equal? opd (undefined))
         (emit-immediate->register! as $imm.undefined r))
        ((null? opd)
         (emit-immediate->register! as $imm.null r))
        ((char? opd)
         (emit-immediate->register! as (char->immediate opd) r))
        (else
         (emit-const->register! as (emit-datum as opd) r))))


; Stuff a bitpattern or symbolic expression into a register.
; (CONST, for immediate constants.)
;
; FIXME(?): if this had access to eval-expr (currently hidden inside the
; sparc assembler) it could attempt to evaluate symbolic expressions,
; thereby selecting better code sequences when possible.

(define (emit-immediate->register! as i r)
  (let ((dest (if (not (hardware-mapped? r)) $r.tmp0 r)))
    (cond ((and (number? i) (immediate-literal? i))
           (sparc.set as i dest))
          ((and (number? i) (zero? (remainder (abs i) 1024)))
           (sparc.sethi as `(hi ,i) dest))
          (else
           (sparc.sethi as `(hi ,i) dest)
           (sparc.ori as dest `(lo ,i) dest)))
    (if (not (hardware-mapped? r))
        (emit-store-reg! as r dest))))


; Reference the constants vector and put the constant reference in a register.
; `offset' is an integer offset into the constants vector (a constant) for
; the current procedure.
; Destroys $r.tmp0 and $r.tmp1, but either can be the destination register.
; (CONST, for structured constants, GLOBAL, SETGLBL, LAMBDA).

(define (emit-const->register! as offset r)
  (let ((cvlabel (+ 4 (- (* offset 4) $tag.vector-tag))))
    (cond ((hardware-mapped? r)
           (sparc.ldi as $r.reg0 $p.constvector $r.tmp0)
           (if (asm:fits? cvlabel 13)
               (sparc.ldi as $r.tmp0 cvlabel r)
               (begin (sparc.sethi as `(hi ,cvlabel) $r.tmp1)
                      (sparc.addr  as $r.tmp0 $r.tmp1 $r.tmp0)
                      (sparc.ldi   as $r.tmp0 `(lo ,cvlabel) r))))
          (else
           (emit-const->register! as offset $r.tmp0)
           (emit-store-reg! as $r.tmp0 r)))))



; Emit single instruction to load sw-mapped reg into another reg, and return
; the destination reg.

(define (emit-load-reg! as from to)
  (if (or (hardware-mapped? from) (not (hardware-mapped? to)))
      (asm-error "emit-load-reg: " from to)
      (begin (sparc.ldi as $r.globals (swreg-global-offset from) to)
             to)))

(define (emit-store-reg! as from to)
  (if (or (not (hardware-mapped? from)) (hardware-mapped? to))
      (asm-error "emit-store-reg: " from to)
      (begin (sparc.sti as from (swreg-global-offset to) $r.globals)
             to)))

; Generic move-reg-to-HW-reg

(define (emit-move2hwreg! as from to)
  (if (hardware-mapped? from)
      (sparc.move as from to)
      (emit-load-reg! as from to))
  to)

; Evaluation of condition code for value or control.
;
; branchf.a is an annulled conditional branch that tests the condition codes
;     and branches if some condition is false.
; rd is #f or a hardware register.
; target is #f or a label.
; Exactly one of rd and target must be #f.
;
; (Why isn't this split into two separate procedures?  Because dozens of
; this procedure's callers have the value/control duality, and it saves
; space to put the test here instead of putting it in each caller.)

(define (emit-evaluate-cc! as branchf.a rd target)
  (if target
      (begin (branchf.a   as target)
             (sparc.slot  as))
      (let ((target (new-label)))
        (branchf.a   as target)
        (sparc.set   as $imm.false rd)
        (sparc.set   as $imm.true rd)
        (sparc.label as target))))

; Code for runtime safety checking.

(define (emit-check! as rs0 L1 liveregs)
  (sparc.cmpi as rs0 $imm.false)
  (emit-checkcc! as sparc.be L1 liveregs))

; FIXME:  This should call the exception handler for non-continuable exceptions.

(define (emit-trap! as rs1 rs2 rs3 exn)
  (if (not (= rs3 $r.reg0))
      (emit-move2hwreg! as rs3 $r.argreg3))
  (if (not (= rs2 $r.reg0))
      (emit-move2hwreg! as rs2 $r.argreg2))
  (if (not (= rs1 $r.reg0))
      (emit-move2hwreg! as rs1 $r.result))
  (millicode-call/numarg-in-reg as $m.exception (thefixnum exn) $r.tmp0))

; Given:
;     an annulled conditional branch that branches
;         if the check is ok
;     a non-annulled conditional branch that branches
;         if the check is not ok
;     #f, or a procedure that takes an assembly segment as
;         argument and emits an instruction that goes into
;         the delay slot of either branch
;     three registers whose contents should be passed to the
;         exception handler if the check is not ok
;     the exception code
; Emits code to call the millicode exception routine with
; the given exception code if the condition is false.
;
; FIXME:  The nop can often be replaced by the instruction that
; follows it.

(begin
 '
(define (emit-checkcc-and-fill-slot!
         as branch-ok.a branch-bad slot-filler L1)
  (let* ((situation (list exn rs1 rs2 rs3))
         (L1 (exception-label as situation)))
    (if L1
        (begin (branch-bad as L1)
               (if slot-filler
                   (slot-filler as)
                   (sparc.nop as)))
        (let* ((L1 (new-label))
               (L2 (new-label)))
          (exception-label-set! as situation L1)
          (branch-ok.a as L2)
          (if slot-filler
              (slot-filler as)
              (sparc.slot  as))
          (sparc.label as L1)
          (cond ((= rs3 $r.reg0)
                 #f)
                ((hardware-mapped? $r.argreg3)
                 (emit-move2hwreg! as rs3 $r.argreg3))
                ((hardware-mapped? rs3)
                 (emit-store-reg! as rs3 $r.argreg3))
                (else
                 (emit-move2hwreg! as rs3 $r.tmp0)
                 (emit-store-reg! as $r.tmp0 $r.argreg3)))
          (if (not (= rs2 $r.reg0))
              (emit-move2hwreg! as rs2 $r.argreg2))
          (if (not (= rs1 $r.reg0))
              (emit-move2hwreg! as rs1 $r.result))
          ; FIXME:  This should be a non-continuable exception.
          (sparc.jmpli as $r.millicode $m.exception $r.o7)
          (emit-immediate->register! as (thefixnum exn) $r.tmp0)
          (sparc.label as L2)))))
#f
)

(define (emit-checkcc! as branch-bad L1 liveregs)
  (branch-bad as L1)
  (apply sparc.slot2 as liveregs))

; Generation of millicode calls for non-continuable exceptions.

(begin
 '
; To create only one millicode call per code segment per non-continuable
; exception situation, we use the "as-user" feature of assembly segments.
; Could use a hash table here.

(define (exception-label as situation)
  (let ((user-data (as-user as)))
    (if user-data
        (let ((exception-labels (assq 'exception-labels user-data)))
          (if exception-labels
              (let ((probe (assoc situation (cdr exception-labels))))
                (if probe
                    (cdr probe)
                    #f))
              #f))
        #f)))
'
(define (exception-label-set! as situation label)
  (let ((user-data (as-user as)))
    (if user-data
        (let ((exception-labels (assq 'exception-labels user-data)))
          (if exception-labels
              (let ((probe (assoc situation (cdr exception-labels))))
                (if probe
                    (error "COMPILER BUG: Exception situation defined twice")
                    (set-cdr! exception-labels
                              (cons (cons situation label)
                                    (cdr exception-labels)))))
              (begin (as-user! as
                               (cons (list 'exception-labels)
                                     user-data))
                     (exception-label-set! as situation label))))
        (begin (as-user! as '())
               (exception-label-set! as situation label)))))
#f
)

; Millicode calling

(define (millicode-call/0arg as mproc)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (sparc.nop   as))

(define (millicode-call/1arg as mproc r)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (emit-move2hwreg! as r $r.argreg2))

(define (millicode-call/1arg-in-result as mproc r)
  (millicode-call/1arg-in-reg as mproc r $r.result))

(define (millicode-call/1arg-in-reg as mproc rs rd)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (emit-move2hwreg! as rs rd))

(define (millicode-call/numarg-in-result as mproc num)
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (sparc.set   as num $r.result))

(define (millicode-call/numarg-in-reg as mproc num reg)
  (if (not (hardware-mapped? reg))
      (asm-error "millicode-call/numarg-in-reg requires HW register: " reg))
  (sparc.jmpli as $r.millicode mproc $r.o7)
  (sparc.set   as num reg))

(define (millicode-call/2arg as mproc r1 r2)
  (emit-move2hwreg! as r1 $r.argreg2)
  (sparc.jmpli      as $r.millicode mproc $r.o7)
  (emit-move2hwreg! as r2 $r.argreg3))

; NOTE: Don't use TMP0 since TMP0 is sometimes a millicode argument
; register (for example to m_exception).
;
; NOTE: Don't use sparc.set rather than sethi/ori; we need to know that
; two instructions get generated.
;
; FIXME: Should calculate the value if possible to get better precision
; and to avoid generating a fixup.  See emit-return-address! in gen-msi.sch.

(define (millicode-call/ret as mproc label)
  (cond ((short-effective-addresses)
         (sparc.jmpli as $r.millicode mproc $r.o7)
         (sparc.addi  as $r.o7 `(- ,label (- ,(here as) 4) 8) $r.o7))
        (else
         (let ((val `(- ,label (+ ,(here as) 8) 8)))
           (sparc.sethi as `(hi ,val) $r.tmp1)
           (sparc.ori   as $r.tmp1 `(lo ,val) $r.tmp1)
           (sparc.jmpli as $r.millicode mproc $r.o7)
           (sparc.addr  as $r.o7 $r.tmp1 $r.o7)))))

(define (check-timer as DESTINATION RETRY)
  (sparc.subicc as $r.timer 1 $r.timer)
  (sparc.bne.a  as DESTINATION)
  (sparc.slot   as)
  (millicode-call/ret as $m.timer-exception RETRY))

; When the destination and retry labels are the same, and follow the
; timer check immediately, then this code saves two static instructions.

(define (check-timer0 as)
  (sparc.subicc as $r.timer 1 $r.timer)
  (sparc.bne.a  as (+ (here as) 16))
  (sparc.slot   as)
  (sparc.jmpli as $r.millicode $m.timer-exception $r.o7)
  (sparc.nop as))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 9 May 1999 / wdc
;
; SPARC machine assembler.
;
; The procedure `sparc-instruction' takes an instruction class keyword and
; some operands and returns an assembler procedure for the instruction
; denoted by the class and the operands.
;
; All assembler procedures for SPARC mnemonics are defined in sparcasm2.sch.
;
; The SPARC has 32-bit, big-endian words.  All instructions are 1 word.
; This assembler currently accepts a subset of the SPARC v8 instruction set.
;
; Each assembler procedure takes an `as' assembly structure (see 
; Asm/Common/pass5p1.sch) and operands relevant to the instruction, and
; side-effects the assembly structure by emitting bits for the instruction
; and any necessary fixups.  There are separate instruction mnemonics and
; assembler procedures for instructions which in the SPARC instruction set 
; are normally considered the "same".  For example, the `add' instruction is
; split into two operations here: `sparc.addr' takes a register as operand2,
; and `sparc.addi' takes an immediate.  We could remove this restriction
; by using objects with identity rather than numbers for registers, but it
; does not seem to be an important problem.
;
; Operands that denote values (addresses, immediates, offsets) may be
; expressed using symbolic expressions. These expressions must conform
; to the following grammar:
;
;   <expr> --> ( <number> . <obj> )        ; label
;            | <number>                    ; literal value (exact integer)
;            | (+ <expr> ... )             ; sum
;            | (- <expr> ... )             ; difference
;            | (hi <expr>)                 ; high 22 bits
;            | (lo <expr>)                 ; low 10 bits
;
; Each assembler procedure will check that its value operand(s) fit in 
; their instruction fields.  It is a fatal error for an operand not 
; to fit, and the assembler calls `asm-error' to signal this error.  
; However, in some cases the assembler will instead call the error 
; procedure `asm-value-too-large', which allows the higher-level assembler 
; to retry the assembly with different settings (typically, by splitting 
; a jump instruction into an offset calculation and a jump).
;
; Note: the idiom that is seen in this file,
;   (emit-fixup-proc! as (lambda (b l) (fixup b l)))
; when `fixup' is a local procedure, avoids allocation of the closure
; except in the cases where the fixup is in fact needed, for gains in
; speed and reduction in allocation.  (Ask me if you want numbers.)
;
; If FILL-DELAY-SLOTS returns true, then this assembler supports two
; distinct mechanisms for filling branch delay slots.
;
; An annulled conditional branch or an un-annulled unconditional branch
; may be followed by the strange instruction SPARC.SLOT, which turns into
; a nop in the delay slot that may be replaced by copying the instruction
; at the target of the branch into the delay slot and increasing the branch
; offset by 4.
;
; An un-annulled conditional branch whose target depends upon a known set
; of general registers, and does not depend upon the condition codes, may
; be followed by the strange instruction SPARC.SLOT2, which takes any
; number of registers as operands.  This strange instruction turns into
; nothing at all if the following instruction has no side effects except
; to the condition codes and/or to a destination register that is distinct
; from the specified registers plus the stack pointer and %o7; otherwise
; the SPARC.SLOT2 instruction becomes a nop in the delay slot.  The
; implementation of this uses a buffer that must be cleared when a label
; is emitted or when the current offset is obtained.

(define sparc-instruction)

(let ((original-emit-label! emit-label!)
      (original-here here))
  (set! emit-label!
        (lambda (as L)
          (assembler-value! as 'slot2-info #f)
          (original-emit-label! as L)))
  (set! here
        (lambda (as)
          (assembler-value! as 'slot2-info #f)
          (original-here as)))
  'emit-label!)

(let ((emit! (lambda (as bits)
               (assembler-value! as 'slot2-info #f)
               (emit! as bits)))
      (emit-fixup-proc! (lambda (as proc)
                          (assembler-value! as 'slot2-info #f)
                          (emit-fixup-proc! as proc)))
      (goes-in-delay-slot2? (lambda (as rd)
                              (let ((regs (assembler-value as 'slot2-info)))
                                (and regs
                                     (fill-delay-slots)
                                     (not (= rd $r.stkp))
                                     (not (= rd $r.o7))
                                     (not (memv rd regs)))))))
  
  (define ibit (asm:bv 0 0 #x20 0))     ; immediate bit: 2^13
  (define abit (asm:bv #x20 0 0 0))     ; annul bit: 2^29
  (define zero (asm:bv 0 0 0 0))        ; all zero bits
  
  (define two^32 (expt 2 32))
  
  ; Constant expression evaluation. If the expression cannot be 
  ; evaluated, eval-expr returns #f, otherwise a number.
  ; The symbol table lookup must fail by returning #f.
  
  (define (eval-expr as e)
    
    (define (complement x)
      (modulo (+ two^32 x) two^32))
    
    (define (hibits e)
      (cond ((not e) e)
            ((< e 0)
             (complement (quotient (complement e) 1024)))
            (else
             (quotient e 1024))))
    
    (define (lobits e)
      (cond ((not e) e)
            ((< e 0)
             (remainder (complement e) 1024))
            (else
             (remainder e 1024))))
    
    (define (evaluate e)
      (cond ((integer? e)      e)
            ((label? e)        (label-value as e))
            ((eq? 'hi (car e)) (hibits (evaluate (cadr e))))
            ((eq? 'lo (car e)) (lobits (evaluate (cadr e))))
            ((eq? '+ (car e))
             (let loop ((e (cdr e)) (s 0))
               (if (null? e) s
                             (let ((op (evaluate (car e))))
                               (if (not op) op
                                            (loop (cdr e) (+ s op)))))))
            ((eq? '- (car e))  
             (let loop ((e (cdr e)) (d #f))
               (if (null? e) d
                             (let ((op (evaluate (car e))))
                               (if (not op) op
                                            (loop (cdr e) (if d (- d op) op)))))))
            (else
             (signal-error 'badexpr e))))
    
    (evaluate e))
  
  ; Common error handling.
  
  (define (signal-error code . rest)
    (define msg "SPARC assembler: ")
    (case code
      ((badexpr)
       (asm-error msg "invalid expression " (car rest)))
      ((toolarge)
       (asm-error msg "value too large in " (car rest) ": "
                  (cadr rest) " = " (caddr rest)))
      ((fixup)
       (asm-error msg "fixup failed in " (car rest) " for " (cadr rest)))
      ((unaligned)
       (asm-error msg "unaligned target in " (car rest) ": " (cadr rest)))
      (else 
       (error "Invalid error code in assembler: " code))))
  
  ; The following procedures construct instructions by depositing field
  ; values directly into bytevectors; the location parameter in the dep-*!
  ; procedures is the address in the bytevector of the most significant byte.
  
  (define (copy! bv k bits)
    (bytevector-set! bv k (bytevector-ref bits 0))
    (bytevector-set! bv (+ k 1) (bytevector-ref bits 1))
    (bytevector-set! bv (+ k 2) (bytevector-ref bits 2))
    (bytevector-set! bv (+ k 3) (bytevector-ref bits 3))
    bv)
  
  (define (copy bits)
    (let ((bv (make-bytevector 4)))
      (bytevector-set! bv 0 (bytevector-ref bits 0))
      (bytevector-set! bv 1 (bytevector-ref bits 1))
      (bytevector-set! bv 2 (bytevector-ref bits 2))
      (bytevector-set! bv 3 (bytevector-ref bits 3))
      bv))
  
  (define (copy-instr bv from to)
    (bytevector-set! bv to (bytevector-ref bv from))
    (bytevector-set! bv (+ to 1) (bytevector-ref bv (+ from 1)))
    (bytevector-set! bv (+ to 2) (bytevector-ref bv (+ from 2)))
    (bytevector-set! bv (+ to 3) (bytevector-ref bv (+ from 3))))
  
  (define (dep-rs1! bits k rs1)
    (bytevector-set! bits (+ k 1)
                          (logior (bytevector-ref bits (+ k 1))
                                  (rshl rs1 2)))
    (bytevector-set! bits (+ k 2)
                          (logior (bytevector-ref bits (+ k 2))
                                  (lsh (logand rs1 3) 6))))
  
  (define (dep-rs2! bits k rs2)
    (bytevector-set! bits (+ k 3)
                          (logior (bytevector-ref bits (+ k 3)) rs2)))
  
  (define (dep-rd! bits k rd)
    (bytevector-set! bits k
                          (logior (bytevector-ref bits k) (lsh rd 1))))
  
  (define (dep-imm! bits k imm)
    (cond ((fixnum? imm)
           (bytevector-set! bits (+ k 3) (logand imm 255))
           (bytevector-set! bits (+ k 2)
                                 (logior (bytevector-ref bits (+ k 2))
                                         (logand (rsha imm 8) 31))))
          ((bytevector? imm)
           (bytevector-set! bits (+ k 3) (bytevector-ref imm 0))
           (bytevector-set! bits (+ k 2)
                                 (logior (bytevector-ref bits (+ k 2))
                                         (logand (bytevector-ref imm 1)
                                                 31))))
          (else
           (dep-imm! bits k (asm:int->bv imm)))))
  
  (define (dep-branch-offset! bits k offs)
    (cond ((fixnum? offs)
           (if (not (= (logand offs 3) 0))
               (signal-error 'unaligned "branch" offs))
           (dep-imm22! bits k (rsha offs 2)))
          ((bytevector? offs)
           (if (not (= (logand (bytevector-ref offs 3) 3) 0))
               (signal-error 'unaligned "branch" (asm:bv->int offs)))
           (dep-imm22! bits k (asm:rsha offs 2)))
          (else
           (dep-branch-offset! bits k (asm:int->bv offs)))))
  
  (define (dep-imm22! bits k imm)
    (cond ((fixnum? imm)
           (bytevector-set! bits (+ k 3) (logand imm 255))
           (bytevector-set! bits (+ k 2)
                                 (logand (rsha imm 8) 255))
           (bytevector-set! bits (+ k 1)
                                 (logior (bytevector-ref bits (+ k 1))
                                         (logand (rsha imm 16) 63))))
          ((bytevector? imm)
           (bytevector-set! bits (+ k 3) (bytevector-ref imm 3))
           (bytevector-set! bits (+ k 2) (bytevector-ref imm 2))
           (bytevector-set! bits (+ k 1)
                                 (logior (bytevector-ref bits (+ k 1))
                                         (logand (bytevector-ref imm 1)
                                                 63))))
          (else
           (dep-imm22! bits k (asm:int->bv imm)))))
  
  (define (dep-call-offset! bits k offs)
    (cond ((fixnum? offs)
           (if (not (= (logand offs 3) 0))
               (signal-error 'unaligned "call" offs))
           (bytevector-set! bits (+ k 3) (logand (rsha offs 2) 255))
           (bytevector-set! bits (+ k 2) (logand (rsha offs 10) 255))
           (bytevector-set! bits (+ k 1) (logand (rsha offs 18) 255))
           (bytevector-set! bits k (logior (bytevector-ref bits k)
                                           (logand (rsha offs 26) 63))))
          ((bytevector? offs)
           (if (not (= (logand (bytevector-ref offs 3) 3) 0))
               (signal-error 'unaligned "call" (asm:bv->int offs)))
           (let ((offs (asm:rsha offs 2)))
             (bytevector-set! bits (+ k 3) (bytevector-ref offs 3))
             (bytevector-set! bits (+ k 2) (bytevector-ref offs 2))
             (bytevector-set! bits (+ k 1) (bytevector-ref offs 1))
             (bytevector-set! bits k (logior (bytevector-ref bits k)
                                             (logand (bytevector-ref offs 0)
                                                     63)))))
          (else
           (dep-call-offset! bits k (asm:int->bv offs)))))
  
  ; Add 1 to an instruction (to bump a branch offset by 4).
  ; FIXME: should check for field overflow.
  
  (define (add1 bv loc)
    (let* ((r0 (+ (bytevector-ref bv (+ loc 3)) 1))
           (d0 (logand r0 255))
           (c0 (rshl r0 8)))
      (bytevector-set! bv (+ loc 3) d0)
      (let* ((r1 (+ (bytevector-ref bv (+ loc 2)) c0))
             (d1 (logand r1 255))
             (c1 (rshl r1 8)))
        (bytevector-set! bv (+ loc 2) d1)
        (let* ((r2 (+ (bytevector-ref bv (+ loc 1)) c1))
               (d2 (logand r2 255)))
          (bytevector-set! bv (+ loc 1) d2)))))
  
  ; For delay slot filling -- uses the assembler value scratchpad in
  ; the as structure.  Delay slot filling is discussed in the comments
  ; for `branch' and `class-slot', below.
  
  (define (remember-branch-target as obj)
    (assembler-value! as 'branch-target obj))
  
  (define (recover-branch-target as)
    (assembler-value as 'branch-target))
  
  ; Mark the instruction at the current address as not being eligible 
  ; for being lifted into a branch delay slot.
  ;
  ; FIXME: should perhaps be a hash table; see BOOT-STATUS file for details.
  
  (define (not-a-delay-slot-instruction as)
    (assembler-value! as 'not-dsi
                         (cons (here as)
                               (or (assembler-value as 'not-dsi) '()))))
  
  (define (is-a-delay-slot-instruction? as bv addr)
    (and (not (memv addr (or (assembler-value as 'not-dsi) '())))
         (< addr (bytevector-length bv))))
  
  ; SETHI, etc.
  
  (define (class-sethi bits)
    (let ((bits (asm:lsh bits 22)))
      (lambda (as val rd)
        
        (define (fixup bv loc)
          (dep-imm22! bv loc
                         (or (eval-expr as val)
                             (signal-error 'fixup "sethi" val))))
        
        (define (fixup2 bv loc)
          (copy! bv loc bits)
          (dep-rd! bv loc rd)
          (fixup bv loc))
        
        (if (goes-in-delay-slot2? as rd)
            (emit-fixup-proc! as
                              (lambda (b l)
                                (fixup2 b (- l 4))))
            
            (let ((bits (copy bits))
                  (e    (eval-expr as val)))
              (if e
                  (dep-imm22! bits 0 e)
                  (emit-fixup-proc! as (lambda (b l) (fixup b l))))
              (dep-rd! bits 0 rd)
              (emit! as bits))))))
  
  ; NOP is a peculiar sethi
  
  (define (class-nop i)
    (let ((instr (class-sethi i)))
      (lambda (as)
        (instr as 0 $r.g0))))
  
  
  ; Branches
  
  (define (class00b i) (branch #b010 i zero))    ; Un-annulled IU branches.
  (define (class00a i) (branch #b010 i abit))    ; Annulled IU branches.
  (define (classf00b i) (branch #b110 i zero))   ; Un-annulled FP branches.
  (define (classf00a i) (branch #b110 i abit))   ; Annulled FP branches.
  
  ; The `type' parameter is #b010 for IU branches, #b110 for FP branches.
  ; The `bits' parameter is the bits for the cond field.
  ; The `annul' parameter is either `zero' or `abit' (see top of file).
  ;
  ; Annuled branches require special treatement for delay slot
  ; filling based on the `slot' pseudo-instruction.
  ;
  ; Strategy: when a branch with the annul bit set is assembled, remember 
  ; its target in a one-element cache in the AS structure. When a slot
  ; instruction is found (it has its own class) then the cached
  ; value (possibly a delayed expression) is gotten, and a fixup for the
  ; slot is registered.  When the fixup is later evaluated, the branch
  ; target instruction can be found, examined, and evaluated. 
  ; 
  ; The cached value is always valid when the slot instruction is assembled,
  ; because a slot instruction is always directly preceded by an annulled
  ; branch (which will always set the cache).
  
  (define (branch type bits annul)
    ; The delay slot should be filled if this is an annulled branch
    ; or an unconditional branch.
    (let ((fill-delay-slot? (or (not (eq? annul zero))
                                (eq? bits #b1000)))
          (bits (asm:logior (asm:lsh bits 25) (asm:lsh type 22) annul)))
      (lambda (as target0)
        (let ((target `(- ,target0 ,(here as))))
          
          (define (expr)
            (let ((e (eval-expr as target)))
              (cond ((not e)
                     e)
                    ((not (zero? (logand e 3)))
                     (signal-error 'unaligned "branch" target0))
                    ((asm:fits? e 24)
                     e)
                    (else
                     (asm-value-too-large as "branch" target e)))))
          
          (define (fixup bv loc)
            (let ((e (expr)))
              (if e
                  (dep-branch-offset! bv loc e)
                  (signal-error 'fixup "branch" target0))))
          
          (if fill-delay-slot?
              (remember-branch-target as target0)
              (remember-branch-target as #f)) ; Clears the cache.
          (not-a-delay-slot-instruction as)
          (let ((bits (copy bits))
                (e    (expr)))
            (if e
                (dep-branch-offset! bits 0 e)
                (emit-fixup-proc! as (lambda (b l) (fixup b l))))
            (emit! as bits))))))
  
  ; Branch delay slot pseudo-instruction.
  ;
  ; Get the branch target expression from the cache in the AS structure,
  ; and if it is not #f, register a fixup procedure for the delay slot that 
  ; will copy the target instruction to the slot and add 4 to the branch
  ; offset (unless that will overflow the offset or the instruction at the
  ; target is not suitable for lifting).
  ;
  ; It's important that this fixup run _after_ any fixups for the branch
  ; instruction itself!
  
  (define (class-slot)
    (let ((nop-instr (class-nop #b100)))
      (lambda (as)
        
        ; The branch target is the expression denoting the target location.
        
        (define branch-target (recover-branch-target as))
        
        (define (fixup bv loc)
          (let ((bt (or (eval-expr as branch-target)
                        (asm-error "Branch fixup: can't happen: " 
                                   branch-target))))
            (if (is-a-delay-slot-instruction? as bv bt)
                (begin
                 (copy-instr bv bt loc)
                 (add1 bv (- loc 4))))))
        
        (if (and branch-target (fill-delay-slots))
            (emit-fixup-proc! as (lambda (b l) (fixup b l))))
        (nop-instr as))))
  
  ; Branch delay slot pseudo-instruction 2.
  ;
  ; Emit a nop, but record the information that will allow this nop to be
  ; replaced by a sufficiently harmless ALU instruction.
  
  (define (class-slot2)
    (let ((nop-instr (class-nop #b100)))
      (lambda (as . regs)
        (nop-instr as)
        (assembler-value! as 'slot2-info regs))))
  
  ; ALU stuff, register operand, rdy, wryr. Also: jump.
  
  (define (class10r bits . extra)
    (cond ((and (not (null? extra)) (eq? (car extra) 'rdy))
           (let ((op (class10r bits)))
             (lambda (as rd)
               (op as 0 0 rd))))
          ((and (not (null? extra)) (eq? (car extra) 'wry))
           (let ((op (class10r bits)))
             (lambda (as rs)
               (op as rs 0 0))))
          (else
           (let ((bits  (asm:logior (asm:lsh #b10 30) (asm:lsh bits 19)))
                 (jump? (and (not (null? extra)) (eq? (car extra) 'jump))))
             (lambda (as rs1 rs2 rd)
               (let ((bits (copy bits)))
                 (dep-rs1! bits 0 rs1)
                 (dep-rs2! bits 0 rs2)
                 (dep-rd! bits 0 rd)
                 (cond (jump?
                        (not-a-delay-slot-instruction as)
                        (emit! as bits))
                       ((goes-in-delay-slot2? as rd)
                        (emit-fixup-proc!
                         as
                         (lambda (bv loc)
                           (copy! bv (- loc 4) bits))))
                       (else
                        (emit! as bits)))))))))
  
  
  ; ALU stuff, immediate operand, wryi. Also: jump.
  
  (define (class10i bits  . extra)
    (if (and (not (null? extra)) (eq? (car extra) 'wry))
        (let ((op (class10i bits)))
          (lambda (as src)
            (op as 0 src 0)))
        (let ((bits  (asm:logior (asm:lsh #b10 30) (asm:lsh bits 19) ibit))
              (jump? (and (not (null? extra)) (eq? (car extra) 'jump))))
          (lambda (as rs1 e rd)
            
            (define (expr)
              (let ((imm (eval-expr as e)))
                (cond ((not imm)
                       imm)
                      ((asm:fits? imm 13)
                       imm)
                      (jump?
                       (asm-value-too-large as "`jmpli'" e imm))
                      (else
                       (asm-value-too-large as "ALU instruction" e imm)))))
            
            (define (fixup bv loc)
              (let ((e (expr)))
                (if e
                    (dep-imm! bv loc e)
                    (signal-error 'fixup "ALU instruction" e))))
            
            (let ((bits (copy bits))
                  (e    (expr)))
              (if e
                  (dep-imm! bits 0 e)
                  (emit-fixup-proc! as (lambda (b l) (fixup b l))))
              (dep-rs1! bits 0 rs1)
              (dep-rd! bits 0 rd)
              (cond (jump?
                     (not-a-delay-slot-instruction as)
                     (emit! as bits))
                    ((goes-in-delay-slot2? as rd)
                     (emit-fixup-proc!
                      as
                      (lambda (bv loc)
                        (copy! bv (- loc 4) bits))))
                    (else
                     (emit! as bits))))))))
  
  ; Memory stuff, register operand.
  
  (define (class11r bits)
    (let ((bits (asm:logior (asm:lsh #b11 30) (asm:lsh bits 19))))
      (lambda (as rs1 rs2 rd)
        (let ((bits (copy bits)))
          (dep-rs1! bits 0 rs1)
          (dep-rs2! bits 0 rs2)
          (dep-rd! bits 0 rd)
          (emit! as bits)))))
  
  ; Memory stuff, immediate operand.
  
  (define (class11i bits)
    (let ((bits (asm:logior (asm:lsh #b11 30) (asm:lsh bits 19) ibit)))
      (lambda (as rs1 e rd)
        
        (define (expr)
          (let ((imm (eval-expr as e)))
            (cond ((not imm) imm)
                  ((asm:fits? imm 13) imm)
                  (else 
                   (signal-error 'toolarge "Memory instruction" e imm)))))
        
        (define (fixup bv loc)
          (let ((e (expr)))
            (if e
                (dep-imm! bv loc e)
                (signal-error 'fixup "Memory instruction" e))))
        
        (let ((bits (copy bits))
              (e    (expr)))
          (dep-rs1! bits 0 rs1)
          (dep-rd! bits 0 rd)
          (if e
              (dep-imm! bits 0 e)
              (emit-fixup-proc! as (lambda (b l) (fixup b l))))
          (emit! as bits)))))
  
  ; For store instructions.  The syntax is (st a b c) meaning m[ b+c ] <- a.
  ; However, on the Sparc, the destination (rd) field is  the source of
  ; a store, so we transform the instruction into (st c b a) and pass it
  ; to the real store procedure.
  
  (define (class11sr bits)
    (let ((store-instr (class11r bits)))
      (lambda (as a b c)
        (store-instr as c b a))))
  
  (define (class11si bits)
    (let ((store-instr (class11i bits)))
      (lambda (as a b c)
        (store-instr as c b a))))
  
  ; Call is a class all by itself.
  
  (define (class-call)
    (let ((code (asm:lsh #b01 30)))
      (lambda (as target0)
        (let ((target `(- ,target0 ,(here as))))
          
          (define (fixup bv loc)
            (let ((e (eval-expr as target)))
              (if e
                  (dep-call-offset! bv loc e)
                  (signal-error 'fixup "call" target0))))
          
          (let ((bits (copy code))
                (e    (eval-expr as target)))
            (not-a-delay-slot-instruction as)
            (if e
                (dep-call-offset! bits 0 e)
                (emit-fixup-proc! as (lambda (b l) (fixup b l))))
            (emit! as bits))))))
  
  (define (class-label)
    (lambda (as label)
      (emit-label! as label)))
  
  ; FP operation, don't set CC.
  
  (define (class-fpop1 i) (fpop #b110100 i))
  
  ; FP operation, set CC
  
  (define (class-fpop2 i) (fpop #b110101 i))
  
  (define (fpop type opf)
    (let ((bits (asm:logior (asm:lsh #b10 30)
                            (asm:lsh type 19)
                            (asm:lsh opf 5))))
      (lambda (as rs1 rs2 rd)
        (let ((bits (copy bits)))
          (dep-rs1! bits 0 rs1)
          (dep-rs2! bits 0 rs2)
          (dep-rd! bits 0 rd)
          (emit! as bits)))))
  
  (set! sparc-instruction
        (lambda (kwd . ops)
          (case kwd
            ((i11)   (apply class11i ops))
            ((r11)   (apply class11r ops))
            ((si11)  (apply class11si ops))
            ((sr11)  (apply class11sr ops))
            ((sethi) (apply class-sethi ops))
            ((r10)   (apply class10r ops))
            ((i10)   (apply class10i ops))
            ((b00)   (apply class00b ops))
            ((a00)   (apply class00a ops))
            ((call)  (apply class-call ops))
            ((label) (apply class-label ops))
            ((nop)   (apply class-nop ops))
            ((slot)  (apply class-slot ops))
            ((slot2) (apply class-slot2 ops))
            ((fb00)  (apply classf00b ops))
            ((fa00)  (apply classf00a ops))
            ((fp)    (apply class-fpop1 ops))
            ((fpcc)  (apply class-fpop2 ops))
            (else
             (asm-error "sparc-instruction: unrecognized class: " kwd)))))
  'sparc-instruction)

; eof
; Instruction mnemonics

(define sparc.lddi    (sparc-instruction 'i11 #b000011))
(define sparc.lddr    (sparc-instruction 'r11 #b000011))
(define sparc.ldi     (sparc-instruction 'i11 #b000000))
(define sparc.ldr     (sparc-instruction 'r11 #b000000))
(define sparc.ldhi    (sparc-instruction 'i11 #b000010))
(define sparc.ldhr    (sparc-instruction 'r11 #b000010))
(define sparc.ldbi    (sparc-instruction 'i11 #b000001))
(define sparc.ldbr    (sparc-instruction 'r11 #b000001))
(define sparc.lddfi   (sparc-instruction 'i11 #b100011))
(define sparc.lddfr   (sparc-instruction 'r11 #b100011))
(define sparc.stdi    (sparc-instruction 'si11 #b000111))
(define sparc.stdr    (sparc-instruction 'sr11 #b000111))
(define sparc.sti     (sparc-instruction 'si11 #b000100))
(define sparc.str     (sparc-instruction 'sr11 #b000100))
(define sparc.sthi    (sparc-instruction 'si11 #b000110))
(define sparc.sthr    (sparc-instruction 'sr11 #b000110))
(define sparc.stbi    (sparc-instruction 'si11 #b000101))
(define sparc.stbr    (sparc-instruction 'sr11 #b000101))
(define sparc.stdfi   (sparc-instruction 'si11 #b100111))
(define sparc.stdfr   (sparc-instruction 'sr11 #b100111))
(define sparc.sethi   (sparc-instruction 'sethi #b100))
(define sparc.andr    (sparc-instruction 'r10 #b000001))
(define sparc.andrcc  (sparc-instruction 'r10 #b010001))
(define sparc.andi    (sparc-instruction 'i10 #b000001))
(define sparc.andicc  (sparc-instruction 'i10 #b010001))
(define sparc.orr     (sparc-instruction 'r10 #b000010))
(define sparc.orrcc   (sparc-instruction 'r10 #b010010))
(define sparc.ori     (sparc-instruction 'i10 #b000010))
(define sparc.oricc   (sparc-instruction 'i10 #b010010))
(define sparc.xorr    (sparc-instruction 'r10 #b000011))
(define sparc.xorrcc  (sparc-instruction 'r10 #b010011))
(define sparc.xori    (sparc-instruction 'i10 #b000011))
(define sparc.xoricc  (sparc-instruction 'i10 #b010011))
(define sparc.sllr    (sparc-instruction 'r10 #b100101))
(define sparc.slli    (sparc-instruction 'i10 #b100101))
(define sparc.srlr    (sparc-instruction 'r10 #b100110))
(define sparc.srli    (sparc-instruction 'i10 #b100110))
(define sparc.srar    (sparc-instruction 'r10 #b100111))
(define sparc.srai    (sparc-instruction 'i10 #b100111))
(define sparc.addr    (sparc-instruction 'r10 #b000000))
(define sparc.addrcc  (sparc-instruction 'r10 #b010000))
(define sparc.addi    (sparc-instruction 'i10 #b000000))
(define sparc.addicc  (sparc-instruction 'i10 #b010000))
(define sparc.taddrcc (sparc-instruction 'r10 #b100000))
(define sparc.taddicc (sparc-instruction 'i10 #b100000))
(define sparc.subr    (sparc-instruction 'r10 #b000100))
(define sparc.subrcc  (sparc-instruction 'r10 #b010100))
(define sparc.subi    (sparc-instruction 'i10 #b000100))
(define sparc.subicc  (sparc-instruction 'i10 #b010100))
(define sparc.tsubrcc (sparc-instruction 'r10 #b100001))
(define sparc.tsubicc (sparc-instruction 'i10 #b100001))
(define sparc.smulr   (sparc-instruction 'r10 #b001011))
(define sparc.smulrcc (sparc-instruction 'r10 #b011011))
(define sparc.smuli   (sparc-instruction 'i10 #b001011))
(define sparc.smulicc (sparc-instruction 'i10 #b011011))
(define sparc.sdivr   (sparc-instruction 'r10 #b001111))
(define sparc.sdivrcc (sparc-instruction 'r10 #b011111))
(define sparc.sdivi   (sparc-instruction 'i10 #b001111))
(define sparc.sdivicc (sparc-instruction 'i10 #b011111))
(define sparc.b       (sparc-instruction 'b00 #b1000))
(define sparc.b.a     (sparc-instruction 'a00 #b1000))
(define sparc.bne     (sparc-instruction 'b00 #b1001))
(define sparc.bne.a   (sparc-instruction 'a00 #b1001))
(define sparc.be      (sparc-instruction 'b00 #b0001))
(define sparc.be.a    (sparc-instruction 'a00 #b0001))
(define sparc.bg      (sparc-instruction 'b00 #b1010))
(define sparc.bg.a    (sparc-instruction 'a00 #b1010))
(define sparc.ble     (sparc-instruction 'b00 #b0010))
(define sparc.ble.a   (sparc-instruction 'a00 #b0010))
(define sparc.bge     (sparc-instruction 'b00 #b1011))
(define sparc.bge.a   (sparc-instruction 'a00 #b1011))
(define sparc.bl      (sparc-instruction 'b00 #b0011))
(define sparc.bl.a    (sparc-instruction 'a00 #b0011))
(define sparc.bgu     (sparc-instruction 'b00 #b1100))
(define sparc.bgu.a   (sparc-instruction 'a00 #b1100))
(define sparc.bleu    (sparc-instruction 'b00 #b0100))
(define sparc.bleu.a  (sparc-instruction 'a00 #b0100))
(define sparc.bcc     (sparc-instruction 'b00 #b1101))
(define sparc.bcc.a   (sparc-instruction 'a00 #b1101))
(define sparc.bcs     (sparc-instruction 'b00 #b0101))
(define sparc.bcs.a   (sparc-instruction 'a00 #b0101))
(define sparc.bpos    (sparc-instruction 'b00 #b1110))
(define sparc.bpos.a  (sparc-instruction 'a00 #b1110))
(define sparc.bneg    (sparc-instruction 'b00 #b0110))
(define sparc.bneg.a  (sparc-instruction 'a00 #b0110))
(define sparc.bvc     (sparc-instruction 'b00 #b1111))
(define sparc.bvc.a   (sparc-instruction 'a00 #b1111))
(define sparc.bvs     (sparc-instruction 'b00 #b0111))
(define sparc.bvs.a   (sparc-instruction 'a00 #b0111))
(define sparc.call    (sparc-instruction 'call))
(define sparc.jmplr   (sparc-instruction 'r10 #b111000 'jump))
(define sparc.jmpli   (sparc-instruction 'i10 #b111000 'jump))
(define sparc.nop     (sparc-instruction 'nop #b100))
(define sparc.ornr    (sparc-instruction 'r10 #b000110))
(define sparc.orni    (sparc-instruction 'i10 #b000110))
(define sparc.ornrcc  (sparc-instruction 'r10 #b010110))
(define sparc.ornicc  (sparc-instruction 'i10 #b010110))
(define sparc.andni   (sparc-instruction 'i10 #b000101))
(define sparc.andnr   (sparc-instruction 'r10 #b000101))
(define sparc.andnicc (sparc-instruction 'i10 #b010101))
(define sparc.andnrcc (sparc-instruction 'r10 #b010101))
(define sparc.rdy     (sparc-instruction 'r10 #b101000 'rdy))
(define sparc.wryr    (sparc-instruction 'r10 #b110000 'wry))
(define sparc.wryi    (sparc-instruction 'i10 #b110000 'wry))
(define sparc.fb      (sparc-instruction 'fb00 #b1000))
(define sparc.fb.a    (sparc-instruction 'fa00 #b1000))
(define sparc.fbn     (sparc-instruction 'fb00 #b0000))
(define sparc.fbn.a   (sparc-instruction 'fa00 #b0000))
(define sparc.fbu     (sparc-instruction 'fb00 #b0111))
(define sparc.fbu.a   (sparc-instruction 'fa00 #b0111))
(define sparc.fbg     (sparc-instruction 'fb00 #b0110))
(define sparc.fbg.a   (sparc-instruction 'fa00 #b0110))
(define sparc.fbug    (sparc-instruction 'fb00 #b0101))
(define sparc.fbug.a  (sparc-instruction 'fa00 #b0101))
(define sparc.fbl     (sparc-instruction 'fb00 #b0100))
(define sparc.fbl.a   (sparc-instruction 'fa00 #b0100))
(define sparc.fbul    (sparc-instruction 'fb00 #b0011))
(define sparc.fbul.a  (sparc-instruction 'fa00 #b0011))
(define sparc.fblg    (sparc-instruction 'fb00 #b0010))
(define sparc.fblg.a  (sparc-instruction 'fa00 #b0010))
(define sparc.fbne    (sparc-instruction 'fb00 #b0001))
(define sparc.fbne.a  (sparc-instruction 'fa00 #b0001))
(define sparc.fbe     (sparc-instruction 'fb00 #b1001))
(define sparc.fbe.a   (sparc-instruction 'fa00 #b1001))
(define sparc.fbue    (sparc-instruction 'fb00 #b1010))
(define sparc.fbue.a  (sparc-instruction 'fa00 #b1010))
(define sparc.fbge    (sparc-instruction 'fb00 #b1011))
(define sparc.fbge.a  (sparc-instruction 'fa00 #b1011))
(define sparc.fbuge   (sparc-instruction 'fb00 #b1100))
(define sparc.fbuge.a (sparc-instruction 'fa00 #b1100))
(define sparc.fble    (sparc-instruction 'fb00 #b1101))
(define sparc.fble.a  (sparc-instruction 'fa00 #b1101))
(define sparc.fbule   (sparc-instruction 'fb00 #b1110))
(define sparc.fbule.a (sparc-instruction 'fa00 #b1110))
(define sparc.fbo     (sparc-instruction 'fb00 #b1111))
(define sparc.fbo.a   (sparc-instruction 'fa00 #b1111))
(define sparc.faddd   (sparc-instruction 'fp   #b001000010))
(define sparc.fsubd   (sparc-instruction 'fp   #b001000110))
(define sparc.fmuld   (sparc-instruction 'fp   #b001001010))
(define sparc.fdivd   (sparc-instruction 'fp   #b001001110))
(define sparc%fnegs   (sparc-instruction 'fp   #b000000101)) ; See below
(define sparc%fmovs   (sparc-instruction 'fp   #b000000001)) ; See below
(define sparc%fabss   (sparc-instruction 'fp   #b000001001)) ; See below
(define sparc%fcmpdcc (sparc-instruction 'fpcc #b001010010)) ; See below

; Strange instructions.

(define sparc.slot    (sparc-instruction 'slot))
(define sparc.slot2   (sparc-instruction 'slot2))
(define sparc.label   (sparc-instruction 'label))

; Aliases.

(define sparc.bnz     sparc.bne)
(define sparc.bnz.a   sparc.bne.a)
(define sparc.bz      sparc.be)
(define sparc.bz.a    sparc.be.a)
(define sparc.bgeu    sparc.bcc)
(define sparc.bgeu.a  sparc.bcc.a)
(define sparc.blu     sparc.bcs)
(define sparc.blu.a   sparc.bcs.a)

; Abstractions.

(define (sparc.cmpr as r1 r2) (sparc.subrcc as r1 r2 $r.g0))
(define (sparc.cmpi as r imm) (sparc.subicc as r imm $r.g0))
(define (sparc.move as rs rd) (sparc.orr as $r.g0 rs rd))
(define (sparc.set as imm rd) (sparc.ori as $r.g0 imm rd))
(define (sparc.btsti as rs imm) (sparc.andicc as rs imm $r.g0))
(define (sparc.clr as rd) (sparc.move as $r.g0 rd))

(define (sparc.deccc as rs . rest)
  (let ((k (cond ((null? rest) 1)
                 ((null? (cdr rest)) (car rest))
                 (else (asm-error "sparc.deccc: too many operands: " rest)))))
    (sparc.subicc as rs k rs)))

; Floating-point abstractions
;
; For fmovd, fnegd, and fabsd, we must synthesize the instruction from
; fmovs, fnegs, and fabss -- SPARC V8 has only the latter.  (SPARC V9 add
; the former.)

(define (sparc.fmovd as rs rd)
  (sparc%fmovs as rs 0 rd)
  (sparc%fmovs as (+ rs 1) 0 (+ rd 1)))

(define (sparc.fnegd as rs rd)
  (sparc%fnegs as rs 0 rd)
  (if (not (= rs rd))
      (sparc%fmovs as (+ rs 1) 0 (+ rd 1))))

(define (sparc.fabsd as rs rd)
  (sparc%fabss as rs 0 rd)
  (if (not (= rs rd))
      (sparc%fmovs as (+ rs 1) 0 (+ rd 1))))

(define (sparc.fcmpd as rs1 rs2)
  (sparc%fcmpdcc as rs1 rs2 0))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; Asm/Sparc/gen-msi.sch -- SPARC assembler code emitters for 
;    core MacScheme instructions
;
; 9 May 1999 / wdc


; SETGLBL
;
; RS must be a hardware register.
;
; A global cell is a pair, where the car holds the value.

(define (emit-register->global! as rs offset)
  (cond ((= rs $r.result)
	 (sparc.move as $r.result $r.argreg2)
	 (emit-const->register! as offset $r.result)
	 (if (write-barrier)
	     (sparc.jmpli as $r.millicode $m.addtrans $r.o7))
	 (sparc.sti as $r.argreg2 (- $tag.pair-tag) $r.result))
	(else
	 (emit-const->register! as offset $r.result)
	 (sparc.sti as rs (- $tag.pair-tag) $r.result)
	 (if (write-barrier)
	     (millicode-call/1arg as $m.addtrans rs)))))


; GLOBAL
;
; A global cell is a pair, where the car holds the value.
; If (catch-undefined-globals) is true, then code will be emitted to
; check whether the global is #!undefined when loaded. If it is, 
; an exception will be taken, with the global in question in $r.result.

(define (emit-global->register! as offset r)
  (emit-load-global as offset r (catch-undefined-globals)))

; This leaves the cell in ARGREG2.  That fact is utilized by global/invoke
; to signal an appropriate error message.

(define (emit-load-global as offset r check?)
  
  (define (emit-undef-check! as r)
    (if check?
	(let ((GLOBAL-OK (new-label)))
	  (sparc.cmpi   as r $imm.undefined)
	  (sparc.bne.a  as GLOBAL-OK)
	  (sparc.slot   as)
	  (millicode-call/0arg as $m.global-ex)            ; Cell in ARGREG2.
	  (sparc.label  as GLOBAL-OK))))

  (emit-const->register! as offset $r.argreg2)             ; Load cell.
  (if (hardware-mapped? r)
      (begin (sparc.ldi as $r.argreg2 (- $tag.pair-tag) r)
	     (emit-undef-check! as r))
      (begin (sparc.ldi as $r.argreg2 (- $tag.pair-tag) $r.tmp0)
	     (emit-store-reg! as $r.tmp0 r)
	     (emit-undef-check! as $r.tmp0))))


; MOVEREG

(define (emit-register->register! as from to)
  (if (not (= from to))
      (cond ((and (hardware-mapped? from) (hardware-mapped? to))
	     (sparc.move as from to))
	    ((hardware-mapped? from)
	     (emit-store-reg! as from to))
	    ((hardware-mapped? to)
	     (emit-load-reg! as from to))
	    (else
	     (emit-load-reg! as from $r.tmp0)
	     (emit-store-reg! as $r.tmp0 to)))))


; ARGS=

(define (emit-args=! as n)
  (if (not (unsafe-code))
      (let ((L2 (new-label)))
	(sparc.cmpi   as $r.result (thefixnum n))  ; FIXME: limit 1023 args
	(sparc.be.a   as L2)
	(sparc.slot   as)
	(millicode-call/numarg-in-reg as $m.argc-ex (thefixnum n) $r.argreg2)
	(sparc.label  as L2))))


; ARGS>=
;
; The cases for 0 and 1 rest arguments are handled in-line; all other
; cases, including too few, are handled in millicode (really: a C call-out).
;
; The fast path only applies when we don't have to mess with the last
; register, hence the test.

(define (emit-args>=! as n)
  (let ((L0  (new-label))
	(L99 (new-label))
	(L98 (new-label)))
    (if (< n (- *lastreg* 1))
	(let ((dest (regname (+ n 1))))
	  (sparc.cmpi   as $r.result (thefixnum n)) ; n args
	  (if (hardware-mapped? dest)
	      (begin
		(sparc.be.a as L99)
		(sparc.set  as $imm.null dest))
	      (begin
		(sparc.set  as $imm.null $r.tmp0)
		(sparc.be.a as L99)
		(sparc.sti  as $r.tmp0 (swreg-global-offset dest) $r.globals)))
	  (sparc.cmpi   as $r.result (thefixnum (+ n 1))) ; n+1 args
	  (sparc.bne.a  as L98)
	  (sparc.nop    as)
	  (millicode-call/numarg-in-result as $m.alloc 8)
	  (let ((src1 (force-hwreg! as dest $r.tmp1)))
	    (sparc.set as $imm.null $r.tmp0)
	    (sparc.sti as src1 0 $r.result)
	    (sparc.sti as $r.tmp0 4 $r.result)
	    (sparc.addi as $r.result $tag.pair-tag $r.result)
	    (sparc.b as L99)
	    (if (hardware-mapped? dest)
		(sparc.move as $r.result dest)
		(sparc.sti  as $r.result (swreg-global-offset dest)
			    $r.globals)))))
    ; General case
    (sparc.label  as L98)
    (sparc.move   as $r.reg0 $r.argreg3)  ; FIXME in Sparc/mcode.s
    (millicode-call/numarg-in-reg as $m.varargs (thefixnum n) $r.argreg2)
    (sparc.label  as L99)))


; INVOKE
; SETRTN/INVOKE
;
; Bummed.  Can still do better when the procedure to call is in a general
; register (avoids the redundant move to RESULT preceding INVOKE).
;
; Note we must set up the argument count even in unsafe mode, because we 
; may be calling code that was not compiled unsafe.

(define (emit-invoke as n setrtn? mc-exception)
  (let ((START    (new-label))
        (TIMER-OK (new-label))
        (PROC-OK  (new-label)))
    (cond ((not (unsafe-code))
           (sparc.label        as START)
           (sparc.subicc       as $r.timer 1 $r.timer)
           (sparc.bne          as TIMER-OK)
           (sparc.andi         as $r.result $tag.tagmask $r.tmp0)
           (millicode-call/ret as $m.timer-exception START)
           (sparc.label        as TIMER-OK)
           (sparc.cmpi         as $r.tmp0 $tag.procedure-tag)
           (sparc.be.a         as PROC-OK)
           (sparc.ldi          as $r.result $p.codevector $r.tmp0)
           (millicode-call/ret as mc-exception START)
           (sparc.label        as PROC-OK))
          (else
           (sparc.label        as START)
           (sparc.subicc       as $r.timer 1 $r.timer)
           (sparc.bne.a        as TIMER-OK)
           (sparc.ldi          as $r.result $p.codevector $r.tmp0)
           (millicode-call/ret as $m.timer-exception START)
           (sparc.label        as TIMER-OK)))
    (sparc.move                as $r.result $r.reg0)
    ;; FIXME: limit 1023 args
    (cond (setrtn?
           (sparc.set          as (thefixnum n) $r.result)
           (sparc.jmpli        as $r.tmp0 $p.codeoffset $r.o7)
           (sparc.sti          as $r.o7 4 $r.stkp))
          (else
           (sparc.jmpli        as $r.tmp0 $p.codeoffset $r.g0)
           (sparc.set          as (thefixnum n) $r.result)))))

; SAVE -- for new compiler
;
; Create stack frame.  To avoid confusing the garbage collector, the
; slots must be initialized to something definite unless they will
; immediately be initialized by a MacScheme machine store instruction.
; The creation is done by emit-save0!, and the initialization is done
; by emit-save1!.

(define (emit-save0! as n)
  (let* ((L1        (new-label))
	 (L0        (new-label))
	 (framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (sparc.label  as L0)
    (sparc.subi   as $r.stkp realsize $r.stkp)
    (sparc.cmpr   as $r.stklim $r.stkp)
    (sparc.ble.a  as L1)
    (sparc.set    as framesize $r.tmp0)
    (sparc.addi   as $r.stkp realsize $r.stkp)
    (millicode-call/ret as $m.stkoflow L0)
    (sparc.label  as L1)
    ; initialize size and return fields of stack frame
    (sparc.sti    as $r.tmp0 0 $r.stkp)
    (sparc.sti    as $r.g0 4 $r.stkp)))

; Given a vector v of booleans, initializes slot i of the stack frame
; if and only if (vector-ref v i).

(define (emit-save1! as v)
  (let ((n (vector-length v)))
    (let loop ((i 0) (offset 12))
      (cond ((= i n)
             #t)
            ((vector-ref v i)
	     (sparc.sti as $r.g0 offset $r.stkp)
	     (loop (+ i 1) (+ offset 4)))
	    (else
	     (loop (+ i 1) (+ offset 4)))))))


; RESTORE
;
; Restore registers from stack frame
; FIXME: Use ldd/std here; see comments for emit-save!, above.
; We pop only actual registers.

(define (emit-restore! as n)
  (let ((n (min n 31)))
    (do ((i      0  (+ i 1))
	 (offset 12 (+ offset 4)))
	((> i n))
      (let ((r (regname i)))
	(if (hardware-mapped? r)
	    (sparc.ldi as $r.stkp offset r)
	    (begin (sparc.ldi as $r.stkp offset $r.tmp0)
		   (emit-store-reg! as $r.tmp0 r)))))))

; POP -- for new compiler
;
; Pop frame.
; If returning?, then emit the return as well and put the pop
; in its delay slot.

(define (emit-pop! as n returning?)
  (let* ((framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (if returning?
        (begin (sparc.ldi   as $r.stkp (+ realsize 4) $r.o7)
	       (sparc.jmpli as $r.o7 8 $r.g0)
	       (sparc.addi  as $r.stkp realsize $r.stkp))
        (sparc.addi as $r.stkp realsize $r.stkp))))


; SETRTN
;
; Change the return address in the stack frame.

(define (emit-setrtn! as label)
  (emit-return-address! as label)
  (sparc.sti as $r.o7 4 $r.stkp))


; APPLY
;
; `apply' falls into millicode.
;
; The timer check is performed here because it is not very easy for the
; millicode to do this.

(define (emit-apply! as r1 r2)
  (let ((L0 (new-label)))
    (check-timer0        as)
    (sparc.label         as L0)
    (emit-move2hwreg!    as r1 $r.argreg2)
    (emit-move2hwreg!    as r2 $r.argreg3)
    (millicode-call/0arg as $m.apply)))


; LOAD

(define (emit-load! as slot dest-reg)
  (if (hardware-mapped? dest-reg)
      (sparc.ldi as $r.stkp (+ 12 (* slot 4)) dest-reg)
      (begin (sparc.ldi as $r.stkp (+ 12 (* slot 4)) $r.tmp0)
	     (emit-store-reg! as $r.tmp0 dest-reg))))


; STORE

(define (emit-store! as k n)
  (if (hardware-mapped? k)
      (sparc.sti as k (+ 12 (* n 4)) $r.stkp)
      (begin (emit-load-reg! as k $r.tmp0)
	     (sparc.sti as $r.tmp0 (+ 12 (* n 4)) $r.stkp))))


; LEXICAL

(define (emit-lexical! as m n)
  (let ((base (emit-follow-chain! as m)))
    (sparc.ldi as base (- (procedure-slot-offset n) $tag.procedure-tag)
	       $r.result)))


; SETLEX
; FIXME: should allow an in-line barrier

(define (emit-setlex! as m n)
  (let ((base (emit-follow-chain! as m)))
    (sparc.sti as $r.result (- (procedure-slot-offset n) $tag.procedure-tag)
	       base)
    (if (write-barrier)
	(begin
	  (sparc.move as $r.result $r.argreg2)
	  (millicode-call/1arg-in-result as $m.addtrans base)))))


; Follow static links.
;
; By using and leaving the result in ARGREG3 rather than in RESULT, 
; we save a temporary register.

(define (emit-follow-chain! as m)
  (let loop ((q m))
    (cond ((not (zero? q))
	   (sparc.ldi as
		      (if (= q m) $r.reg0 $r.argreg3)
		      $p.linkoffset
		      $r.argreg3)
	   (loop (- q 1)))
	  ((zero? m) 
	   $r.reg0)
	  (else 
	   $r.argreg3))))

; RETURN

(define (emit-return! as)
  (sparc.ldi   as $r.stkp 4 $r.o7)
  (sparc.jmpli as $r.o7 8 $r.g0)
  (sparc.nop   as))


; RETURN-REG k

(define (emit-return-reg! as r)
  (sparc.ldi   as $r.stkp 4 $r.o7)
  (sparc.jmpli as $r.o7 8 $r.g0)
  (sparc.move  as r $r.result))


; RETURN-CONST k
;
; The constant c must be synthesizable in a single instruction.

(define (emit-return-const! as c)
  (sparc.ldi   as $r.stkp 4 $r.o7)
  (sparc.jmpli as $r.o7 8 $r.g0)
  (emit-constant->register as c $r.result))


; MVRTN

(define (emit-mvrtn! as)
  (asm-error "multiple-value return has not been implemented (yet)."))


; LEXES

(define (emit-lexes! as n-slots)
  (emit-alloc-proc! as n-slots)
  (sparc.ldi as $r.reg0 $p.codevector $r.tmp0)
  (sparc.ldi as $r.reg0 $p.constvector $r.tmp1)
  (sparc.sti as $r.tmp0 $p.codevector $r.result)
  (sparc.sti as $r.tmp1 $p.constvector $r.result)
  (emit-init-proc-slots! as n-slots))


; LAMBDA

(define (emit-lambda! as code-offs0 const-offs0 n-slots)
  (let* ((code-offs  (+ 4 (- (* 4 code-offs0) $tag.vector-tag)))
         (const-offs (+ 4 (- (* 4 const-offs0) $tag.vector-tag)))
         (fits? (asm:fits? const-offs 13)))
    (emit-alloc-proc! as n-slots)
    (if fits?
        (begin (sparc.ldi as $r.reg0 $p.constvector $r.tmp0)
               (sparc.ldi as $r.tmp0 code-offs $r.tmp1))
        (emit-const->register! as code-offs0 $r.tmp1))
    (sparc.sti as $r.tmp1 $p.codevector $r.result)
    (if fits?
        (begin (sparc.ldi as $r.reg0 $p.constvector $r.tmp0)
               (sparc.ldi as $r.tmp0 const-offs $r.tmp1))
        (emit-const->register! as const-offs0 $r.tmp1))
    (sparc.sti as $r.tmp1 $p.constvector $r.result)
    (emit-init-proc-slots! as n-slots)))
 
; Allocate procedure with room for n register slots; return tagged pointer.

(define emit-alloc-proc!
  (let ((two^12 (expt 2 12)))
    (lambda (as n)
      (millicode-call/numarg-in-result as $m.alloc (* (+ n 4) 4))
      (let ((header (+ (* (* (+ n 3) 4) 256) $imm.procedure-header)))
	(emit-immediate->register! as header $r.tmp0)
	(sparc.sti  as $r.tmp0 0 $r.result)
	(sparc.addi as $r.result $tag.procedure-tag $r.result)))))

; Initialize data slots in procedure from current registers as specified for
; `lamba' and `lexes'. If there are more data slots than registers, then
; we must generate code to cdr down the list in the last register to obtain
; the rest of the data. The list is expected to have at least the minimal
; length.
;
; The tagged pointer to the procedure is in $r.result.

(define (emit-init-proc-slots! as n)

  (define (save-registers lo hi offset)
    (do ((lo     lo     (+ lo 1))
	 (offset offset (+ offset 4)))
	((> lo hi))
      (let ((r (force-hwreg! as (regname lo) $r.tmp0)))
	(sparc.sti as r offset $r.result))))

  (define (save-list lo hi offset)
    (emit-load-reg! as $r.reg31 $r.tmp0)
    (do ((lo     lo      (+ lo 1))
	 (offset offset (+ offset 4)))
	((> lo hi))
      (sparc.ldi as $r.tmp0 (- $tag.pair-tag) $r.tmp1)
      (sparc.sti as $r.tmp1 offset $r.result)
      (if (< lo hi)
	  (begin 
	    (sparc.ldi as $r.tmp0 (+ (- $tag.pair-tag) 4) $r.tmp0)))))
      
  (cond ((< n *lastreg*)
	 (save-registers 0 n $p.reg0))
	(else
	 (save-registers 0 (- *lastreg* 1) $p.reg0)
	 (save-list      *lastreg* n (+ $p.reg0 (* *lastreg* 4))))))

; BRANCH

(define (emit-branch! as check-timer? label)
  (if check-timer?
      (check-timer as label label)
      (begin (sparc.b    as label)
             (sparc.slot as))))


; BRANCHF

(define (emit-branchf! as label)
  (emit-branchfreg! as $r.result label))


; BRANCHFREG -- introduced by peephole optimization.

(define (emit-branchfreg! as hwreg label)
  (sparc.cmpi as hwreg $imm.false)
  (sparc.be.a as label)
  (sparc.slot as))


; BRANCH-WITH-SETRTN -- introduced by peephole optimization

(define (emit-branch-with-setrtn! as label)
  (check-timer0 as)
  (sparc.call   as label)
  (sparc.sti    as $r.o7 4 $r.stkp))

; JUMP
;
; Given the finalization order (outer is finalized before inner is assembled)
; the label value will always be available when a jump is assembled.  The
; only exception is when m = 0, but does this ever happen?  This code handles
; the case anyway.

(define (emit-jump! as m label)
  (let* ((r      (emit-follow-chain! as m))
	 (labelv (label-value as label))
	 (v      (if (number? labelv)
		     (+ labelv $p.codeoffset)
		     (list '+ label $p.codeoffset))))
    (sparc.ldi as r $p.codevector $r.tmp0)
    (if (and (number? v) (immediate-literal? v))
	(sparc.jmpli as $r.tmp0 v $r.g0)
	(begin (emit-immediate->register! as v $r.tmp1)
	       (sparc.jmplr as $r.tmp0 $r.tmp1 $r.g0)))
    (sparc.move  as r $r.reg0)))


; .SINGLESTEP
;
; Single step: jump to millicode; pass index of documentation string in
; %TMP0. Some instructions execute when reg0 is not a valid pointer to
; the current procedure (because this is just after returning); in this
; case we restore reg0 from the stack location given by 'funkyloc'.

(define (emit-singlestep-instr! as funky? funkyloc cvlabel)
  (if funky?
      (sparc.ldi as $r.stkp (+ (thefixnum funkyloc) 12) $r.reg0))
  (millicode-call/numarg-in-reg as $m.singlestep
				   (thefixnum cvlabel)
				   $r.argreg2))


; Emit the effective address of a label-8 into %o7.
;
; There are multiple ways to do this.  If the call causes an expensive
; bubble in the pipeline it is probably much less expensive to grub
; the code vector address out of the procedure in REG0 and calculate it
; that way.  FIXME: We need to benchmark these options.
;
; In general the point is moot as the common-case sequence
;       setrtn L1
;       invoke n
;   L1:
; should be peephole-optimized into the obvious fast code.

(define (emit-return-address! as label)
  (let* ((loc  (here as))
	 (lloc (label-value as label)))

    (define (emit-short val)
      (sparc.call as (+ loc 8))
      (sparc.addi as $r.o7 val $r.o7))

    (define (emit-long val)
      ; Don't use sparc.set: we need to know that two instructions get
      ; generated.
      (sparc.sethi as `(hi ,val) $r.tmp0)
      (sparc.ori   as $r.tmp0 `(lo ,val) $r.tmp0)
      (sparc.call  as (+ loc 16))
      (sparc.addr  as $r.o7 $r.tmp0 $r.o7))

    (cond (lloc
	   (let ((target-rel-addr (- lloc loc 8)))
	     (if (immediate-literal? target-rel-addr)
		 (emit-short target-rel-addr)
		 (emit-long (- target-rel-addr 8)))))
	  ((short-effective-addresses)
	   (emit-short `(- ,label ,loc 8)))
	  (else
	   (emit-long `(- ,label ,loc 16))))))

; eof
; Copyright 1998 Lars T Hansen.
; 
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 22 April 1999 / wdc
;
; SPARC code generation macros for primitives, part 1:
;   primitives defined in Compiler/sparc.imp.sch.

; These extend Asm/Common/pass5p1.sch.

(define (operand5 instruction)
  (car (cddddr (cdr instruction))))

(define (operand6 instruction)
  (cadr (cddddr (cdr instruction))))

(define (operand7 instruction)
  (caddr (cddddr (cdr instruction))))


; Primop emitters.

(define (emit-primop.1arg! as op)
  ((find-primop op) as))

(define (emit-primop.2arg! as op r)
  ((find-primop op) as r))

(define (emit-primop.3arg! as a1 a2 a3)
  ((find-primop a1) as a2 a3))

(define (emit-primop.4arg! as a1 a2 a3 a4)
  ((find-primop a1) as a2 a3 a4))

(define (emit-primop.5arg! as a1 a2 a3 a4 a5)
  ((find-primop a1) as a2 a3 a4 a5))

(define (emit-primop.6arg! as a1 a2 a3 a4 a5 a6)
  ((find-primop a1) as a2 a3 a4 a5 a6))

(define (emit-primop.7arg! as a1 a2 a3 a4 a5 a6 a7)
  ((find-primop a1) as a2 a3 a4 a5 a6 a7))


; Hash table of primops

(define primop-vector (make-vector 256 '()))

(define (define-primop name proc)
  (let ((h (logand (symbol-hash name) 255)))
    (vector-set! primop-vector h (cons (cons name proc)
				       (vector-ref primop-vector h)))
    name))

(define (find-primop name)
  (let ((h (logand (symbol-hash name) 255)))
    (cdr (assq name (vector-ref primop-vector h)))))

(define (for-each-primop proc)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length primop-vector)))
    (for-each (lambda (p)
                (proc (cdr p)))
              (vector-ref primop-vector i))))

; Primops

(define-primop 'unspecified
  (lambda (as)
    (emit-immediate->register! as $imm.unspecified $r.result)))

(define-primop 'undefined
  (lambda (as)
    (emit-immediate->register! as $imm.undefined $r.result)))

(define-primop 'eof-object
  (lambda (as)
    (emit-immediate->register! as $imm.eof $r.result)))

(define-primop 'enable-interrupts
  (lambda (as)
    (millicode-call/0arg as $m.enable-interrupts)))

(define-primop 'disable-interrupts
  (lambda (as)
    (millicode-call/0arg as $m.disable-interrupts)))

(define-primop 'gc-counter
  (lambda (as)
    (sparc.ldi as $r.globals $g.gccnt $r.result)))

(define-primop 'zero?
  (lambda (as)
    (emit-cmp-primop! as sparc.be.a $m.zerop $r.g0)))

(define-primop '=
  (lambda (as r)
    (emit-cmp-primop! as sparc.be.a $m.numeq r)))

(define-primop '<
  (lambda (as r)
    (emit-cmp-primop! as sparc.bl.a $m.numlt r)))

(define-primop '<=
  (lambda (as r)
    (emit-cmp-primop! as sparc.ble.a $m.numle r)))

(define-primop '>
  (lambda (as r)
    (emit-cmp-primop! as sparc.bg.a $m.numgt r)))

(define-primop '>=
  (lambda (as r)
    (emit-cmp-primop! as sparc.bge.a $m.numge r)))

(define-primop 'complex?
  (lambda (as)
    (millicode-call/0arg as $m.complexp)))

(define-primop 'real?
  (lambda (as)
    (millicode-call/0arg as $m.realp)))

(define-primop 'rational?
  (lambda (as)
    (millicode-call/0arg as $m.rationalp)))

(define-primop 'integer?
  (lambda (as)
    (millicode-call/0arg as $m.integerp)))

(define-primop 'exact?
  (lambda (as)
    (millicode-call/0arg as $m.exactp)))

(define-primop 'inexact?
  (lambda (as)
    (millicode-call/0arg as $m.inexactp)))

(define-primop 'fixnum?
  (lambda (as)
    (sparc.btsti as $r.result 3)
    (emit-set-boolean! as)))

(define-primop '+
  (lambda (as r)
    (emit-primop.4arg! as 'internal:+ $r.result r $r.result)))

(define-primop '-
  (lambda (as r)
    (emit-primop.4arg! as 'internal:- $r.result r $r.result)))

(define-primop '*
  (lambda (as rs2)
    (emit-multiply-code as rs2 #f)))

(define (emit-multiply-code as rs2 fixnum-arithmetic?)
  (if (and (unsafe-code) fixnum-arithmetic?)
      (begin
	(sparc.srai    as $r.result 2 $r.tmp0)
	(sparc.smulr   as $r.tmp0 rs2 $r.result))
      (let ((rs2    (force-hwreg! as rs2 $r.argreg2))
	    (Lstart (new-label))
	    (Ltagok (new-label))
	    (Loflo  (new-label))
	    (Ldone  (new-label)))
	(sparc.label   as Lstart)
	(sparc.orr     as $r.result rs2 $r.tmp0)
	(sparc.btsti   as $r.tmp0 3)
	(sparc.be.a    as Ltagok)
	(sparc.srai    as $r.result 2 $r.tmp0)
	(sparc.label   as Loflo)
	(if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2))
	(if (not fixnum-arithmetic?)
	    (begin
	      (millicode-call/ret as $m.multiply Ldone))
	    (begin
	      (sparc.set as (thefixnum $ex.fx*) $r.tmp0)
	      (millicode-call/ret as $m.exception Lstart)))
	(sparc.label   as Ltagok)
	(sparc.smulr   as $r.tmp0 rs2 $r.tmp0)
	(sparc.rdy     as $r.tmp1)
	(sparc.srai    as $r.tmp0 31 $r.tmp2)
	(sparc.cmpr    as $r.tmp1 $r.tmp2)
	(sparc.bne.a   as Loflo)
	(sparc.slot    as)
	(sparc.move    as $r.tmp0 $r.result)
	(sparc.label   as Ldone))))

(define-primop '/
  (lambda (as r)
    (millicode-call/1arg as $m.divide r)))

(define-primop 'quotient
  (lambda (as r)
    (millicode-call/1arg as $m.quotient r)))

(define-primop 'remainder
  (lambda (as r)
    (millicode-call/1arg as $m.remainder r)))

(define-primop '--
  (lambda (as)
    (emit-negate as $r.result $r.result)))

(define-primop 'round
  (lambda (as)
    (millicode-call/0arg as $m.round)))

(define-primop 'truncate
  (lambda (as)
    (millicode-call/0arg as $m.truncate)))

(define-primop 'lognot
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-fixnum! as $r.result $ex.lognot))
    (sparc.ornr as $r.g0 $r.result $r.result)  ; argument order matters
    (sparc.xori as $r.result 3 $r.result)))

(define-primop 'logand
  (lambda (as x)
    (logical-op as $r.result x $r.result sparc.andr $ex.logand)))

(define-primop 'logior
  (lambda (as x)
    (logical-op as $r.result x $r.result sparc.orr $ex.logior)))

(define-primop 'logxor
  (lambda (as x)
    (logical-op as $r.result x $r.result sparc.xorr $ex.logxor)))

; Fixnum shifts.
;
; Only positive shifts are meaningful.
; FIXME: These are incompatible with MacScheme and MIT Scheme.
; FIXME: need to return to start of sequence after fault.

(define-primop 'lsh
  (lambda (as x)
    (emit-shift-operation as $ex.lsh $r.result x $r.result)))

(define-primop 'rshl
  (lambda (as x)
    (emit-shift-operation as $ex.rshl $r.result x $r.result)))

(define-primop 'rsha
  (lambda (as x)
    (emit-shift-operation as $ex.rsha $r.result x $r.result)))


; fixnums only.
; FIXME: for symmetry with shifts there should be rotl and rotr (?)
;        or perhaps rot should only ever rotate one way.
; FIXME: implement.

(define-primop 'rot
  (lambda (as x)
    (asm-error "Sparcasm: ROT primop is not implemented.")))

(define-primop 'null?
  (lambda (as)
    (sparc.cmpi as $r.result $imm.null)
    (emit-set-boolean! as)))

(define-primop 'pair?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.pair-tag)))

(define-primop 'eof-object?
  (lambda (as)
    (sparc.cmpi as $r.result $imm.eof)
    (emit-set-boolean! as)))

; Tests the specific representation, not 'flonum or compnum with 0i'.

(define-primop 'flonum?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.flonum-typetag))))

(define-primop 'compnum?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.compnum-typetag))))

(define-primop 'symbol?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.symbol-typetag))))

(define-primop 'port?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.port-typetag))))

(define-primop 'structure?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.structure-typetag))))

(define-primop 'char?
  (lambda (as)
    (sparc.andi as $r.result #xFF $r.tmp0)
    (sparc.cmpi as $r.tmp0 $imm.character)
    (emit-set-boolean! as)))

(define-primop 'string?
  (lambda (as)
    (emit-double-tagcheck->bool! as
				 $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.string-typetag))))

(define-primop 'bytevector?
  (lambda (as)
    (emit-double-tagcheck->bool! as
				 $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.bytevector-typetag))))

(define-primop 'bytevector-like?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.bytevector-tag)))

(define-primop 'vector?
  (lambda (as)
    (emit-double-tagcheck->bool! as
				 $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.vector-typetag))))

(define-primop 'vector-like?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.vector-tag)))

(define-primop 'procedure?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.procedure-tag)))

(define-primop 'cons
  (lambda (as r)
    (emit-primop.4arg! as 'internal:cons $r.result r $r.result)))

(define-primop 'car
  (lambda (as)
    (emit-primop.3arg! as 'internal:car $r.result $r.result)))

(define-primop 'cdr
  (lambda (as)
    (emit-primop.3arg! as 'internal:cdr $r.result $r.result)))

(define-primop 'car:pair
  (lambda (as)
    (sparc.ldi as $r.result (- $tag.pair-tag) $r.result)))

(define-primop 'cdr:pair
  (lambda (as)
    (sparc.ldi as $r.result (- 4 $tag.pair-tag) $r.result)))

(define-primop 'set-car!
  (lambda (as x)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert! as $tag.pair-tag $ex.car #f))
    (emit-setcar/setcdr! as $r.result x 0)))

(define-primop 'set-cdr!
  (lambda (as x)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert! as $tag.pair-tag $ex.cdr #f))
    (emit-setcar/setcdr! as $r.result x 4)))

; Cells are internal data structures, represented using pairs.
; No error checking is done on cell references.

(define-primop 'make-cell
  (lambda (as)
    (emit-primop.4arg! as 'internal:cons $r.result $r.g0 $r.result)))

(define-primop 'cell-ref
  (lambda (as)
    (emit-primop.3arg! as 'internal:cell-ref $r.result $r.result)))

(define-primop 'cell-set!
  (lambda (as r)
    (emit-setcar/setcdr! as $r.result r 0)))

(define-primop 'syscall
  (lambda (as)
    (millicode-call/0arg as $m.syscall)))

(define-primop 'break
  (lambda (as)
    (millicode-call/0arg as $m.break)))

(define-primop 'creg
  (lambda (as)
    (millicode-call/0arg as $m.creg)))

(define-primop 'creg-set!
  (lambda (as)
    (millicode-call/0arg as $m.creg-set!)))

(define-primop 'typetag
  (lambda (as)
    (millicode-call/0arg as $m.typetag)))

(define-primop 'typetag-set!
  (lambda (as r)
    (millicode-call/1arg as $m.typetag-set r)))

(define-primop 'exact->inexact
  (lambda (as)
    (millicode-call/0arg as $m.exact->inexact)))

(define-primop 'inexact->exact
  (lambda (as)
    (millicode-call/0arg as $m.inexact->exact)))

(define-primop 'real-part
  (lambda (as)
    (millicode-call/0arg as $m.real-part)))

(define-primop 'imag-part
  (lambda (as)
    (millicode-call/0arg as $m.imag-part)))

(define-primop 'char->integer
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-char! as $ex.char2int #f))
    (sparc.srli as $r.result 14 $r.result)))

(define-primop 'integer->char
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-fixnum! as $r.result $ex.int2char))
    (sparc.andi as $r.result #x3FF $r.result)
    (sparc.slli as $r.result 14 $r.result)
    (sparc.ori  as $r.result $imm.character $r.result)))

(define-primop 'not
  (lambda (as)
    (sparc.cmpi as $r.result $imm.false)
    (emit-set-boolean! as)))

(define-primop 'eq?
  (lambda (as x)
    (emit-primop.4arg! as 'internal:eq? $r.result x $r.result)))

(define-primop 'eqv?
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp0))
	  (L1  (new-label)))
      (sparc.cmpr as $r.result tmp)
      (sparc.be.a as L1)
      (sparc.set  as $imm.true $r.result)
      (millicode-call/1arg as $m.eqv tmp)
      (sparc.label as L1))))

(define-primop 'make-bytevector
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-positive-fixnum! as $r.result $ex.mkbvl))
    (emit-allocate-bytevector as
			      (+ $imm.bytevector-header
				 $tag.bytevector-typetag)
			      #f)
    (sparc.addi as $r.result $tag.bytevector-tag $r.result)))

(define-primop 'bytevector-fill!
  (lambda (as rs2)
    (let* ((fault (emit-double-tagcheck-assert! as
						$tag.bytevector-tag
						(+ $imm.bytevector-header
						   $tag.bytevector-typetag)
						$ex.bvfill
						rs2))
	   (rs2 (force-hwreg! as rs2 $r.argreg2)))
      (sparc.btsti  as rs2 3)
      (sparc.bne    as fault)
      (sparc.srai   as rs2 2 $r.tmp2)
      (sparc.ldi    as $r.result (- $tag.bytevector-tag) $r.tmp0)
      (sparc.addi   as $r.result (- 4 $tag.bytevector-tag) $r.tmp1)
      (sparc.srai   as $r.tmp0 8 $r.tmp0)
      (emit-bytevector-fill as $r.tmp0 $r.tmp1 $r.tmp2))))

(define-primop 'bytevector-length
  (lambda (as)
    (emit-get-length! as 
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvlen
		      $r.result
		      $r.result)))

(define-primop 'bytevector-like-length
  (lambda (as)
    (emit-get-length! as
		      $tag.bytevector-tag
		      #f
		      $ex.bvllen
		      $r.result
		      $r.result)))

(define-primop 'bytevector-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvref
		      r)
		     #f)))
      (emit-bytevector-like-ref! as $r.result r $r.result fault #f #t))))

(define-primop 'bytevector-like-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.bytevector-tag
						   $ex.bvlref
						   r)
		     #f)))
      (emit-bytevector-like-ref! as $r.result r $r.result fault #f #f))))

(define-primop 'bytevector-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvset
		      r1)
		     #f)))
      (emit-bytevector-like-set! as r1 r2 fault #t))))

(define-primop 'bytevector-like-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.bytevector-tag
						   $ex.bvlset
						   r1)
		     #f)))
      (emit-bytevector-like-set! as r1 r2 fault #f))))

(define-primop 'sys$bvlcmp
  (lambda (as x)
    (millicode-call/1arg as $m.bvlcmp x)))

; Strings

; RESULT must have nonnegative fixnum.
; RS2 must have character.

(define-primop 'make-string
  (lambda (as rs2)
    (let ((FAULT (new-label))
	  (START (new-label)))
      (sparc.label as START)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
	(if (not (unsafe-code))
	    (let ((L1 (new-label))
		  (L2 (new-label)))
	      (sparc.tsubrcc as $r.result $r.g0 $r.g0)
	      (sparc.bvc.a   as L1)
	      (sparc.andi    as rs2 255 $r.tmp0)
	      (sparc.label   as FAULT)
	      (if (not (= rs2 $r.argreg2))
		  (sparc.move as rs2 $r.argreg2))
	      (sparc.set     as (thefixnum $ex.mkbvl) $r.tmp0) ; Wrong code.
	      (millicode-call/ret as $m.exception START)
	      (sparc.label   as L1)
	      (sparc.bl      as FAULT)
	      (sparc.cmpi    as $r.tmp0 $imm.character)
	      (sparc.bne     as FAULT)
	      (sparc.move as $r.result $r.argreg3))
	    (begin
	      (sparc.move as $r.result $r.argreg3)))
	(emit-allocate-bytevector as
				  (+ $imm.bytevector-header
				     $tag.string-typetag)
				  $r.argreg3)
	(sparc.srai   as rs2 16 $r.tmp1)
	(sparc.addi   as $r.result 4 $r.result)
	(sparc.srai   as $r.argreg3 2 $r.tmp0)
	(emit-bytevector-fill as $r.tmp0 $r.result $r.tmp1)
	(sparc.addi as $r.result (- $tag.bytevector-tag 4) $r.result)))))

(define-primop 'string-length
  (lambda (as)
    (emit-primop.3arg! as 'internal:string-length $r.result $r.result)))

(define-primop 'string-ref
  (lambda (as r)
    (emit-primop.4arg! as 'internal:string-ref $r.result r $r.result)))

(define-primop 'string-set!
  (lambda (as r1 r2)
    (emit-string-set! as $r.result r1 r2)))

(define-primop 'sys$partial-list->vector
  (lambda (as r)
    (millicode-call/1arg as $m.partial-list->vector r)))

(define-primop 'make-procedure
  (lambda (as)
    (emit-make-vector-like! as
			    '()
			    $imm.procedure-header
			    $tag.procedure-tag)))

(define-primop 'make-vector
  (lambda (as r)
    (emit-make-vector-like! as
			    r
			    (+ $imm.vector-header $tag.vector-typetag)
			    $tag.vector-tag)))

(define-primop 'make-vector:0
  (lambda (as r) (make-vector-n as 0 r)))

(define-primop 'make-vector:1
  (lambda (as r) (make-vector-n as 1 r)))

(define-primop 'make-vector:2
  (lambda (as r) (make-vector-n as 2 r)))

(define-primop 'make-vector:3
  (lambda (as r) (make-vector-n as 3 r)))

(define-primop 'make-vector:4
  (lambda (as r) (make-vector-n as 4 r)))

(define-primop 'make-vector:5
  (lambda (as r) (make-vector-n as 5 r)))

(define-primop 'make-vector:6
  (lambda (as r) (make-vector-n as 6 r)))

(define-primop 'make-vector:7
  (lambda (as r) (make-vector-n as 7 r)))

(define-primop 'make-vector:8
  (lambda (as r) (make-vector-n as 8 r)))

(define-primop 'make-vector:9
  (lambda (as r) (make-vector-n as 9 r)))

(define-primop 'vector-length
  (lambda (as)
    (emit-primop.3arg! as 'internal:vector-length $r.result $r.result)))

(define-primop 'vector-like-length
  (lambda (as)
    (emit-get-length! as $tag.vector-tag #f $ex.vllen $r.result $r.result)))

(define-primop 'vector-length:vec
  (lambda (as)
    (emit-get-length-trusted! as $tag.vector-tag $r.result $r.result)))

(define-primop 'procedure-length
  (lambda (as)
    (emit-get-length! as $tag.procedure-tag #f $ex.plen $r.result $r.result)))

(define-primop 'vector-ref
  (lambda (as r)
    (emit-primop.4arg! as 'internal:vector-ref $r.result r $r.result)))

(define-primop 'vector-like-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.vector-tag
						   $ex.vlref
						   r)
		     #f)))
      (emit-vector-like-ref!
       as $r.result r $r.result fault $tag.vector-tag #f))))

(define-primop 'vector-ref:trusted
  (lambda (as rs2)
    (emit-vector-like-ref-trusted!
     as $r.result rs2 $r.result $tag.vector-tag)))

(define-primop 'procedure-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.procedure-tag
						   $ex.pref
						   r)
		     #f)))
      (emit-vector-like-ref!
       as $r.result r $r.result fault $tag.procedure-tag #f))))

(define-primop 'vector-set!
  (lambda (as r1 r2)
    (emit-primop.4arg! as 'internal:vector-set! $r.result r1 r2)))

(define-primop 'vector-like-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.vector-tag
						   $ex.vlset
						   r1)
		     #f)))
      (emit-vector-like-set! as $r.result r1 r2 fault $tag.vector-tag #f))))

(define-primop 'vector-set!:trusted
  (lambda (as rs2 rs3)
    (emit-vector-like-set-trusted! as $r.result rs2 rs3 $tag.vector-tag)))

(define-primop 'procedure-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.procedure-tag
						   $ex.pset
						   r1)
		     #f)))
      (emit-vector-like-set! as $r.result r1 r2 fault $tag.procedure-tag #f))))

(define-primop 'char<?
  (lambda (as x)
    (emit-char-cmp as x sparc.bl.a $ex.char<?)))

(define-primop 'char<=?
  (lambda (as x)
    (emit-char-cmp as x sparc.ble.a $ex.char<=?)))

(define-primop 'char=?
  (lambda (as x)
    (emit-char-cmp as x sparc.be.a $ex.char=?)))

(define-primop 'char>?
  (lambda (as x)
    (emit-char-cmp as x sparc.bg.a $ex.char>?)))

(define-primop 'char>=?
  (lambda (as x)
    (emit-char-cmp as x sparc.bge.a $ex.char>=?)))

; Experimental (for performance).
; This makes massive assumptions about the layout of the port structure:
; A port is a vector-like where
;   #0 = port.input?
;   #4 = port.buffer
;   #7 = port.rd-lim
;   #8 = port.rd-ptr
; See Lib/iosys.sch for more information.

(define-primop 'sys$read-char
  (lambda (as)
    (let ((Lfinish (new-label))
	  (Lend    (new-label)))
      (if (not (unsafe-code))
	  (begin
	    (sparc.andi as $r.result $tag.tagmask $r.tmp0) ; mask argument tag
	    (sparc.cmpi as $r.tmp0 $tag.vector-tag); vector-like? 
	    (sparc.bne as Lfinish)		   ; skip if not vector-like
	    (sparc.nop as)
	    (sparc.ldbi as $r.RESULT 0 $r.tmp1)))   ; header byte
      (sparc.ldi  as $r.RESULT 1 $r.tmp2)	    ; port.input? or garbage
      (if (not (unsafe-code))
	  (begin
	    (sparc.cmpi as $r.tmp1 $hdr.port)       ; port?
	    (sparc.bne as Lfinish)))		    ; skip if not port
      (sparc.cmpi as $r.tmp2 $imm.false)  	    ; [slot] input port?
      (sparc.be as Lfinish)			    ; skip if not active port
      (sparc.ldi as $r.RESULT (+ 1 32) $r.tmp1)	    ; [slot] port.rd-ptr 
      (sparc.ldi as $r.RESULT (+ 1 28) $r.tmp2)	    ; port.rd-lim
      (sparc.ldi as $r.RESULT (+ 1 16) $r.tmp0)	    ; port.buffer
      (sparc.cmpr as $r.tmp1 $r.tmp2)		    ; rd-ptr < rd-lim?
      (sparc.bge as Lfinish)			    ; skip if rd-ptr >= rd-lim
      (sparc.subi as $r.tmp0 1 $r.tmp0)		    ; [slot] addr of string@0
      (sparc.srai as $r.tmp1 2 $r.tmp2)		    ; rd-ptr as native int
      (sparc.ldbr as $r.tmp0 $r.tmp2 $r.tmp2)	    ; get byte from string
      (sparc.addi as $r.tmp1 4 $r.tmp1)		    ; bump rd-ptr
      (sparc.sti as $r.tmp1 (+ 1 32) $r.RESULT)	    ; store rd-ptr in port
      (sparc.slli as $r.tmp2 16 $r.tmp2)	    ; convert to char #1
      (sparc.b as Lend)
      (sparc.ori as $r.tmp2 $imm.character $r.RESULT) ; [slot] convert to char
      (sparc.label as Lfinish)
      (sparc.set as $imm.false $r.RESULT)	    ; failed
      (sparc.label as Lend))))


; eof
; Copyright 1998 Lars T Hansen.
; 
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 9 May 1999 / wdc
;
; SPARC code generation macros for primitives, part 2:
;   primitives introduced by peephole optimization.

(define-primop 'internal:car
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:car src1 dest)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 #f $ex.car))
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:cdr
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:cdr src1 dest)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 #f $ex.cdr))
    (sparc.ldi as src1 (- 4 $tag.pair-tag) dest)))

(define-primop 'internal:cell-ref
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:cell-ref src1 dest)
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:set-car!
  (lambda (as rs1 rs2 dest-ignored)
    (internal-primop-invariant2 'internal:set-car! rs1 dest-ignored)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as $tag.pair-tag rs1 rs2 $ex.car))
    (emit-setcar/setcdr! as rs1 rs2 0)))

(define-primop 'internal:set-cdr!
  (lambda (as rs1 rs2 dest-ignored)
    (internal-primop-invariant2 'internal:set-cdr! rs1 dest-ignored)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as $tag.pair-tag rs1 rs2 $ex.cdr))
    (emit-setcar/setcdr! as rs1 rs2 4)))

(define-primop 'internal:cell-set!
  (lambda (as rs1 rs2 dest-ignored)
    (internal-primop-invariant2 'internal:cell-set! rs1 dest-ignored)
    (emit-setcar/setcdr! as rs1 rs2 0)))

; CONS
;
; One instruction reduced here translates into about 2.5KB reduction in the
; size of the basic heap image. :-)
;
; In the out-of-line case, if rd != RESULT then a garbage value is left 
; in RESULT, but it always looks like a fixnum, so it's OK.

(define-primop 'internal:cons
  (lambda (as rs1 rs2 rd)
    (if (inline-allocation)
	(let ((ENOUGH-MEMORY (new-label))
	      (START (new-label)))
	  (sparc.label   as START)
	  (sparc.addi    as $r.e-top 8 $r.e-top)
	  (sparc.cmpr    as $r.e-top $r.e-limit)
	  (sparc.ble.a   as ENOUGH-MEMORY)
	  (sparc.sti     as rs1 -8 $r.e-top)
	  (millicode-call/ret as $m.gc START)
	  (sparc.label   as ENOUGH-MEMORY)
	  (sparc.sti     as (force-hwreg! as rs2 $r.tmp0) -4 $r.e-top)
	  (sparc.subi    as $r.e-top (- 8 $tag.pair-tag) rd))
	(begin
	  (if (= rs1 $r.result)
	      (sparc.move as $r.result $r.argreg2))
	  (millicode-call/numarg-in-result as $m.alloc 8)
	  (if (= rs1 $r.result)
	      (sparc.sti as $r.argreg2 0 $r.result)
	      (sparc.sti as rs1 0 $r.result))
	  (sparc.sti as (force-hwreg! as rs2 $r.tmp1) 4 $r.result)
	  (sparc.addi as $r.result $tag.pair-tag rd)))))

(define-primop 'internal:car:pair
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:car src1 dest)
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:cdr:pair
  (lambda (as src1 dest)
    (internal-primop-invariant2 'internal:cdr src1 dest)
    (sparc.ldi as src1 (- 4 $tag.pair-tag) dest)))

; Vector operations.

(define-primop 'internal:vector-length
  (lambda (as rs rd)
    (internal-primop-invariant2 'internal:vector-length rs rd)
    (emit-get-length! as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      $ex.vlen
		      rs
		      rd)))

(define-primop 'internal:vector-ref
  (lambda (as rs1 rs2 rd)
    (internal-primop-invariant2 'internal:vector-ref rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/reg!
		      as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      rs1 
		      rs2
		      $ex.vref))))
      (emit-vector-like-ref! as rs1 rs2 rd fault $tag.vector-tag #t))))

(define-primop 'internal:vector-ref/imm
  (lambda (as rs1 imm rd)
    (internal-primop-invariant2 'internal:vector-ref/imm rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/imm!
		      as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      rs1 
		      imm
		      $ex.vref))))
      (emit-vector-like-ref/imm! as rs1 imm rd fault $tag.vector-tag #t))))

(define-primop 'internal:vector-set!
  (lambda (as rs1 rs2 rs3)
    (internal-primop-invariant1 'internal:vector-set! rs1)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/reg!
		      as
		      $tag.vector-tag
		      (+ $imm.vector-header $tag.vector-typetag)
		      rs1
		      rs2
		      $ex.vset))))
      (emit-vector-like-set! as rs1 rs2 rs3 fault $tag.vector-tag #t))))

(define-primop 'internal:vector-length:vec
  (lambda (as rs1 dst)
    (internal-primop-invariant2 'internal:vector-length:vec rs1 dst)
    (emit-get-length-trusted! as $tag.vector-tag rs1 dst)))

(define-primop 'internal:vector-ref:trusted
  (lambda (as rs1 rs2 dst)
    (emit-vector-like-ref-trusted! as rs1 rs2 dst $tag.vector-tag)))

(define-primop 'internal:vector-set!:trusted
  (lambda (as rs1 rs2 rs3)
    (emit-vector-like-ref-trusted! as rs1 rs2 rs3 $tag.vector-tag)))

; Strings.

(define-primop 'internal:string-length
  (lambda (as rs rd)
    (internal-primop-invariant2 'internal:string-length rs rd)
    (emit-get-length! as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.string-typetag)
		      $ex.slen
		      rs
		      rd)))

(define-primop 'internal:string-ref
  (lambda (as rs1 rs2 rd)
    (internal-primop-invariant2 'internal:string-ref rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/reg!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.string-typetag)
		      rs1 
		      rs2
		      $ex.sref))))
      (emit-bytevector-like-ref! as rs1 rs2 rd fault #t #t))))

(define-primop 'internal:string-ref/imm
  (lambda (as rs1 imm rd)
    (internal-primop-invariant2 'internal:string-ref/imm rs1 rd)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert-reg/imm!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.string-typetag)
		      rs1 
		      imm
		      $ex.sref))))
      (emit-bytevector-like-ref/imm! as rs1 imm rd fault #t #t))))

(define-primop 'internal:string-set!
  (lambda (as rs1 rs2 rs3)
    (internal-primop-invariant1 'internal:string-set! rs1)
      (emit-string-set! as rs1 rs2 rs3)))

(define-primop 'internal:+
  (lambda (as src1 src2 dest)
    (internal-primop-invariant2 'internal:+ src1 dest)
    (emit-arith-primop! as sparc.taddrcc sparc.subr $m.add src1 src2 dest #t)))

(define-primop 'internal:+/imm
  (lambda (as src1 imm dest)
    (internal-primop-invariant2 'internal:+/imm src1 dest)
    (emit-arith-primop! as sparc.taddicc sparc.subi $m.add src1 imm dest #f)))

(define-primop 'internal:-
  (lambda (as src1 src2 dest)
    (internal-primop-invariant2 'internal:- src1 dest)
    (emit-arith-primop! as sparc.tsubrcc sparc.addr $m.subtract 
			src1 src2 dest #t)))

(define-primop 'internal:-/imm
  (lambda (as src1 imm dest)
    (internal-primop-invariant2 'internal:-/imm src1 dest)
    (emit-arith-primop! as sparc.tsubicc sparc.addi $m.subtract
			src1 imm dest #f)))

(define-primop 'internal:--
  (lambda (as rs rd)
    (internal-primop-invariant2 'internal:-- rs rd)
    (emit-negate as rs rd)))

(define-primop 'internal:branchf-null?
  (lambda (as reg label)
    (internal-primop-invariant1 'internal:branchf-null? reg)
    (sparc.cmpi  as reg $imm.null)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-pair?
  (lambda (as reg label)
    (internal-primop-invariant1 'internal:branchf-pair? reg)
    (sparc.andi  as reg $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $tag.pair-tag)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-zero?
  (lambda (as reg label)
    (internal-primop-invariant1 'internal:brancf-zero? reg)
    (emit-bcmp-primop! as sparc.bne.a reg $r.g0 label $m.zerop #t)))

(define-primop 'internal:branchf-eof-object?
  (lambda (as rs label)
    (internal-primop-invariant1 'internal:branchf-eof-object? rs)
    (sparc.cmpi  as rs $imm.eof)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-fixnum?
  (lambda (as rs label)
    (internal-primop-invariant1 'internal:branchf-fixnum? rs)
    (sparc.btsti as rs 3)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-char?
  (lambda (as rs label)
    (internal-primop-invariant1 'internal:branchf-char? rs)
    (sparc.andi  as rs 255 $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $imm.character)
    (sparc.bne.a as label)
    (sparc.slot  as)))

(define-primop 'internal:branchf-=
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-= src1)
    (emit-bcmp-primop! as sparc.bne.a src1 src2 label $m.numeq #t)))

(define-primop 'internal:branchf-<
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-< src1)
    (emit-bcmp-primop! as sparc.bge.a src1 src2 label $m.numlt #t)))

(define-primop 'internal:branchf-<=
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-<= src1)
    (emit-bcmp-primop! as sparc.bg.a src1 src2 label $m.numle #t)))

(define-primop 'internal:branchf->
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-> src1)
    (emit-bcmp-primop! as sparc.ble.a src1 src2 label $m.numgt #t)))

(define-primop 'internal:branchf->=
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf->= src1)
    (emit-bcmp-primop! as sparc.bl.a src1 src2 label $m.numge #t)))

(define-primop 'internal:branchf-=/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf-=/imm src1)
    (emit-bcmp-primop! as sparc.bne.a src1 imm label $m.numeq #f)))

(define-primop 'internal:branchf-</imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf-</imm src1)
    (emit-bcmp-primop! as sparc.bge.a src1 imm label $m.numlt #f)))

(define-primop 'internal:branchf-<=/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf-<=/imm src1)
    (emit-bcmp-primop! as sparc.bg.a src1 imm label $m.numle #f)))

(define-primop 'internal:branchf->/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf->/imm src1)
    (emit-bcmp-primop! as sparc.ble.a src1 imm label $m.numgt #f)))

(define-primop 'internal:branchf->=/imm
  (lambda (as src1 imm label)
    (internal-primop-invariant1 'internal:branchf->=/imm src1)
    (emit-bcmp-primop! as sparc.bl.a src1 imm label $m.numge #f)))

(define-primop 'internal:branchf-char=?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char=? src1)
    (emit-char-bcmp-primop! as sparc.bne.a src1 src2 label $ex.char=?)))

(define-primop 'internal:branchf-char<=?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char<=? src1)
    (emit-char-bcmp-primop! as sparc.bg.a src1 src2 label $ex.char<=?)))

(define-primop 'internal:branchf-char<?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char<? src1)
    (emit-char-bcmp-primop! as sparc.bge.a src1 src2 label $ex.char<?)))

(define-primop 'internal:branchf-char>=?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char>=? src1)
    (emit-char-bcmp-primop! as sparc.bl.a src1 src2 label $ex.char>=?)))

(define-primop 'internal:branchf-char>?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-char>=? src1)
    (emit-char-bcmp-primop! as sparc.ble.a src1 src2 label $ex.char>?)))

(define-primop 'internal:branchf-char=?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char=?/imm src)
    (emit-char-bcmp-primop! as sparc.bne.a src imm label $ex.char=?)))

(define-primop 'internal:branchf-char>=?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char>=?/imm src)
    (emit-char-bcmp-primop! as sparc.bl.a src imm label $ex.char>=?)))

(define-primop 'internal:branchf-char>?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char>?/imm src)
    (emit-char-bcmp-primop! as sparc.ble.a src imm label $ex.char>?)))

(define-primop 'internal:branchf-char<=?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char<=?/imm src)
    (emit-char-bcmp-primop! as sparc.bg.a src imm label $ex.char<=?)))

(define-primop 'internal:branchf-char<?/imm
  (lambda (as src imm label)
    (internal-primop-invariant1 'internal:branchf-char<?/imm src)
    (emit-char-bcmp-primop! as sparc.bge.a src imm label $ex.char<?)))

(define-primop 'internal:eq?
  (lambda (as src1 src2 dest)
    (internal-primop-invariant2 'internal:eq? src1 dest)
    (let ((tmp (force-hwreg! as src2 $r.tmp0)))
      (sparc.cmpr as src1 tmp)
      (emit-set-boolean-reg! as dest))))

(define-primop 'internal:eq?/imm
  (lambda (as rs imm rd)
    (internal-primop-invariant2 'internal:eq?/imm rs rd)
    (cond ((fixnum? imm) (sparc.cmpi as rs (thefixnum imm)))
	  ((eq? imm #t)  (sparc.cmpi as rs $imm.true))
	  ((eq? imm #f)  (sparc.cmpi as rs $imm.false))
	  ((null? imm)   (sparc.cmpi as rs $imm.null))
	  (else ???))
    (emit-set-boolean-reg! as rd)))

(define-primop 'internal:branchf-eq?
  (lambda (as src1 src2 label)
    (internal-primop-invariant1 'internal:branchf-eq? src1)
    (let ((src2 (force-hwreg! as src2 $r.tmp0)))
      (sparc.cmpr  as src1 src2)
      (sparc.bne.a as label)
      (sparc.slot  as))))

(define-primop 'internal:branchf-eq?/imm
  (lambda (as rs imm label)
    (internal-primop-invariant1 'internal:branchf-eq?/imm rs)
    (cond ((fixnum? imm) (sparc.cmpi as rs (thefixnum imm)))
	  ((eq? imm #t)  (sparc.cmpi as rs $imm.true))
	  ((eq? imm #f)  (sparc.cmpi as rs $imm.false))
	  ((null? imm)   (sparc.cmpi as rs $imm.null))
	  (else ???))
    (sparc.bne.a as label)
    (sparc.slot  as)))

; Unary predicates followed by a check.

(define-primop 'internal:check-fixnum?
  (lambda (as src L1 liveregs)
    (sparc.btsti   as src 3)
    (emit-checkcc! as sparc.bne L1 liveregs)))

(define-primop 'internal:check-pair?
  (lambda (as src L1 liveregs)
    (sparc.andi    as src $tag.tagmask $r.tmp0)
    (sparc.cmpi    as $r.tmp0 $tag.pair-tag)
    (emit-checkcc! as sparc.bne L1 liveregs)))

(define-primop 'internal:check-vector?
  (lambda (as src L1 liveregs)
    (sparc.andi    as src $tag.tagmask $r.tmp0)
    (sparc.cmpi    as $r.tmp0 $tag.vector-tag)
    (sparc.bne     as L1)
    (sparc.nop     as)
    (sparc.ldi     as src (- $tag.vector-tag) $r.tmp0)
    (sparc.andi    as $r.tmp0 255 $r.tmp1)
    (sparc.cmpi    as $r.tmp1 $imm.vector-header)
    (emit-checkcc! as sparc.bne L1 liveregs)))

(define-primop 'internal:check-vector?/vector-length:vec
  (lambda (as src dst L1 liveregs)
    (sparc.andi    as src     $tag.tagmask        $r.tmp0)
    (sparc.cmpi    as $r.tmp0 $tag.vector-tag)
    (sparc.bne     as L1)
    (sparc.nop     as)
    (sparc.ldi     as src     (- $tag.vector-tag) $r.tmp0)
    (sparc.andi    as $r.tmp0 255                 $r.tmp1)
    (sparc.cmpi    as $r.tmp1 $imm.vector-header)
    (sparc.bne     as L1)
    (apply sparc.slot2 as liveregs)
    (sparc.srli    as $r.tmp0 8 dst)))

(define (internal-primop-invariant2 name a b)
    (if (not (and (hardware-mapped? a) (hardware-mapped? b)))
	(asm-error "SPARC assembler internal invariant violated by " name
		   " on operands " a " and " b)))

(define (internal-primop-invariant1 name a)
    (if (not (hardware-mapped? a))
	(asm-error "SPARC assembler internal invariant violated by " name
		   " on operand " a)))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; SPARC code generation macros for primitives, part 3a:
;   helper procedures for scalars.


; LOGAND, LOGIOR, LOGXOR: logical operations on fixnums.
;
; Input:  Registers rs1 and rs2, both of which can be general registers.
;         In addition, rs1 can be RESULT, and rs2 can be ARGREG2.
; Output: Register dest, which can be a general register or RESULT.

(define (logical-op as rs1 rs2 dest op excode)

  (define (fail rs1 rs2 L0)
    (if (not (= rs1 $r.result))  (sparc.move as rs1 $r.result))
    (if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2))
    (sparc.set as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L0))

  (let ((L0  (new-label))
        (L1  (new-label)))
    (sparc.label     as L0)
    (let ((rs1 (force-hwreg! as rs1 $r.result))
          (rs2 (force-hwreg! as rs2 $r.argreg2))
          (u   (unsafe-code))
          (d   (hardware-mapped? dest)))
      (cond ((and u d)
             (op as rs1 rs2 dest))
            ((and u (not d))
             (op as rs1 rs2 $r.tmp0)
             (emit-store-reg! as $r.tmp0 dest))
            ((and (not u) d)
             (sparc.orr     as rs1 rs2 $r.tmp0)
             (sparc.btsti   as $r.tmp0 3)
             (sparc.bz.a    as L1)
             (op            as rs1 rs2 dest)
             (fail rs1 rs2 L0)
             (sparc.label   as L1))
            (else
             (sparc.orr     as rs1 rs2 $r.tmp0)
             (sparc.btsti   as $r.tmp0 3)
             (sparc.bz.a    as L1)
             (op            as rs1 rs2 $r.tmp0)
             (fail rs1 rs2 L0)
             (sparc.label   as L1)
             (emit-store-reg! as $r.tmp0 dest))))))


; LSH, RSHA, RSHL: Bitwise shifts on fixnums.
;
; Notes for future contemplation:
;   - The semantics do not match those of MIT Scheme or MacScheme: only 
;     positive shifts are allowed.
;   - The names do not match the fixnum-specific procedures of Chez Scheme
;     that have the same semantics: fxsll, fxsra, fxsrl.
;   - This code checks that the second argument is in range; if it did
;     not, then we could get a MOD for free.  Probably too hardware-dependent
;     to worry about.
;   - The range 0..31 for the shift count is curious given that the fixnum
;     is 30-bit.

(define (emit-shift-operation as exn rs1 rs2 rd)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
        (let ((L0 (new-label))
              (FAULT (new-label))
              (START (new-label)))
          (sparc.label as START)
          (sparc.btsti as rs1 3)          ; RS1 fixnum?
          (sparc.be.a  as L0)
          (sparc.andi  as rs2 #x7c $r.g0) ; RS2 fixnum and 0 <= RS2 < 32?
          (sparc.label as FAULT)
          (if (not (= rs1 $r.result))
              (sparc.move as rs1 $r.result))
          (if (not (= rs2 $r.argreg2))
              (emit-move2hwreg! as rs2 $r.argreg2))
          (sparc.set   as (thefixnum exn) $r.tmp0)
          (millicode-call/ret as $m.exception START)
          (sparc.label as L0)
          (sparc.bne   as FAULT)
          (sparc.srai  as rs2 2 $r.tmp1))
        (begin
          (sparc.srai  as rs2 2 $r.tmp1)))
    (cond ((= exn $ex.lsh)
           (sparc.sllr as rs1 $r.tmp1 rd))
          ((= exn $ex.rshl)
           (sparc.srlr  as rs1 $r.tmp1 rd)
           (sparc.andni as rd 3 rd))
          ((= exn $ex.rsha)
           (sparc.srar  as rs1 $r.tmp1 rd)
           (sparc.andni as rd 3 rd))
          (else ???))))


; Set result on condition code.
;
; The processor's zero bit has been affected by a previous instruction.
; If the bit is set, store #t in RESULT, otherwise store #f in RESULT.

(define (emit-set-boolean! as)
  (emit-set-boolean-reg! as $r.result))


; Set on condition code.
;
; The processor's zero bit has been affected by a previous instruction.
; If the bit is set, store #t in the processor register 'dest', otherwise
; store #f in 'dest'.

(define (emit-set-boolean-reg! as dest)
  (let ((L1 (new-label)))
    (sparc.set   as $imm.true dest)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false dest)
    (sparc.label as L1)))


; Representation predicate.

(define (emit-single-tagcheck->bool! as tag)
  (sparc.andi as $r.result $tag.tagmask $r.tmp0)
  (sparc.cmpi as $r.tmp0 tag)
  (emit-set-boolean! as))

(define (emit-single-tagcheck-assert! as tag1 excode reg2)
  (emit-single-tagcheck-assert-reg! as tag1 $r.result reg2 excode))

(define (emit-single-tagcheck-assert-reg! as tag1 reg reg2 excode)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as reg $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (fault-if-ne as excode #f #f reg reg2 L0)))

; Assert that a machine register has a fixnum in it.
; Returns the label of the fault code.

(define (emit-assert-fixnum! as reg excode)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label  as L0)
    (sparc.btsti  as reg 3)
    (fault-if-ne as excode #f #f reg #f L0)))

; Assert that RESULT has a character in it.
; Returns the label of the fault code.

(define (emit-assert-char! as excode fault-label)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as $r.result #xFF $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $imm.character)
    (fault-if-ne as excode #f fault-label #f #f L0)))

; Generate code for fault handling if the zero flag is not set.
; - excode is the nativeint exception code.
; - cont-label, if not #f, is the label to go to if there is no fault.
; - fault-label, if not #f, is the label of an existing fault handler.
; - reg1, if not #f, is the number of a register which must be
;   moved into RESULT before the fault handler is called.
; - reg2, if not #f, is the number of a register which must be moved
;   into ARGREG2 before the fault handler is called.
; - ret-label, if not #f, is the return address to be set up before calling
;   the fault handler.
;
; Ret-label and fault-label cannot simultaneously be non-#f; in this case
; the ret-label is ignored (since the existing fault handler most likely
; sets up the return in the desired manner).

(define (fault-if-ne as excode cont-label fault-label reg1 reg2 ret-label)
  (if fault-label
      (begin 
        (if (and reg2 (not (= reg2 $r.argreg2)))
            (emit-move2hwreg! as reg2 $r.argreg2))
        (sparc.bne as fault-label)
        (if (and reg1 (not (= reg1 $r.result)))
            (sparc.move as reg1 $r.result)
            (sparc.nop as))
        fault-label)
      (let ((FAULT (new-label))
            (L1    (new-label)))
        (sparc.be.a  as (or cont-label L1))
        (sparc.slot  as)
        (sparc.label as FAULT)
        (if (and reg1 (not (= reg1 $r.result)))
            (sparc.move as reg1 $r.result))
        (if (and reg2 (not (= reg2 $r.argreg2)))
            (emit-move2hwreg! as reg2 $r.argreg2))
        (sparc.set   as (thefixnum excode) $r.tmp0)
        (millicode-call/ret as $m.exception (or ret-label L1))
        (if (or (not cont-label) (not ret-label))
            (sparc.label as L1))
        FAULT)))

; This is more expensive than what is good for it (5 cycles in the usual case),
; but there does not seem to be a better way.

(define (emit-assert-positive-fixnum! as reg excode)
  (let ((L1 (new-label))
        (L2 (new-label))
        (L3 (new-label))) 
    (sparc.label   as L2)
    (sparc.tsubrcc as reg $r.g0 $r.g0)
    (sparc.bvc     as L1)
    (sparc.nop     as)
    (sparc.label   as L3)
    (if (not (= reg $r.result))
        (sparc.move as reg $r.result))
    (sparc.set     as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L2)
    (sparc.label   as L1)
    (sparc.bl      as L3)
    (sparc.nop     as)
    L3))


; Arithmetic comparison with boolean result.

(define (emit-cmp-primop! as branch_t.a generic r)
  (let ((Ltagok (new-label))
        (Lcont  (new-label))
        (r      (force-hwreg! as r $r.argreg2)))
    (sparc.tsubrcc as $r.result r $r.g0)
    (sparc.bvc.a   as Ltagok)
    (sparc.set     as $imm.false $r.result)
    (if (not (= r $r.argreg2))
        (sparc.move    as r $r.argreg2))
    (millicode-call/ret as generic Lcont)
    (sparc.label   as Ltagok)
    (branch_t.a    as Lcont)
    (sparc.set     as $imm.true $r.result)
    (sparc.label   as Lcont)))


; Arithmetic comparison and branch.
;
; This code does not use the chained branch trick (DCTI) that was documented
; in the Sparc v8 manual and deprecated in the v9 manual.  This code executes
; _much_ faster on the Ultra than the code using DCTI, even though it executes
; the same instructions.
;
; Parameters and preconditions.
;   Src1 is a general register, RESULT, ARGREG2, or ARGREG3.
;   Src2 is a general register, RESULT, ARGREG2, ARGREG3, or an immediate.
;   Src2 is an immediate iff src2isreg = #f.
;   Branch_f.a is a branch on condition code that branches if the condition
;     is not true.
;   Generic is the millicode table offset of the generic procedure.

(define (emit-bcmp-primop! as branch_f.a src1 src2 Lfalse generic src2isreg)
  (let ((Ltagok (new-label))
        (Ltrue  (new-label))
        (op2    (if src2isreg
                    (force-hwreg! as src2 $r.tmp1)
                    (thefixnum src2)))
        (sub   (if src2isreg sparc.tsubrcc sparc.tsubicc))
        (mov   (if src2isreg sparc.move sparc.set)))
    (sub         as src1 op2 $r.g0)
    (sparc.bvc.a as Ltagok)
    (sparc.slot  as)

    ; Not both fixnums.
    ; Must move src1 to result if src1 is not result.
    ; Must move src2 to argreg2 if src2 is not argreg2.

    (let ((move-res  (not (= src1 $r.result)))
          (move-arg2 (or (not src2isreg) (not (= op2 $r.argreg2)))))
      (if (and move-arg2 move-res)
          (mov     as op2 $r.argreg2))
      (sparc.jmpli as $r.millicode generic $r.o7)
      (cond (move-res   (sparc.move as src1 $r.result))
            (move-arg2  (mov        as op2 $r.argreg2))
            (else       (sparc.nop  as)))
      (sparc.cmpi  as $r.result $imm.false)
      (sparc.bne.a as Ltrue)
      (sparc.slot  as)
      (sparc.b     as Lfalse)
      (sparc.slot  as))

    (sparc.label as Ltagok)
    (branch_f.a   as Lfalse)
    (sparc.slot  as)
    (sparc.label as Ltrue)))


; Generic arithmetic for + and -.
; Some rules:
;   We have two HW registers src1 and dest.
;   If src2isreg is #t then src2 may be a HW reg or a SW reg
;   If src2isreg is #f then src2 is an immediate fixnum, not shifted.
;   Src1 and dest may be RESULT, but src2 may not.
;   Src2 may be ARGREG2, the others may not.
;
; FIXME! This is incomprehensible.

; New code below.

'(define (emit-arith-primop! as op invop generic src1 src2 dest src2isreg)
  (let ((L1  (new-label))
        (op2 (if src2isreg
                 (force-hwreg! as src2 $r.tmp1)
                 (thefixnum src2))))
    (if (and src2isreg (= op2 dest))
        (begin (op          as src1 op2 $r.tmp0)
               (sparc.bvc.a as L1)
               (sparc.move  as $r.tmp0 dest))
        (begin (op          as src1 op2 dest)
               (sparc.bvc.a as L1)
               (sparc.slot  as)
               (invop       as dest op2 dest)))
    (let ((n    (+ (if (not (= src1 $r.result)) 1 0)
                   (if (or (not src2isreg) (not (= op2 $r.argreg2))) 1 0)))
          (mov2 (if src2isreg sparc.move sparc.set)))
      (if (= n 2)
          (mov2 as op2 $r.argreg2))
      (sparc.jmpli as $r.millicode generic $r.o7)
      (cond ((= n 0) (sparc.nop  as))
            ((= n 1) (mov2       as op2 $r.argreg2))
            (else    (sparc.move as src1 $r.result)))
      ; Generic arithmetic leaves stuff in RESULT, must move to dest if
      ; dest is not RESULT.
      (if (not (= dest $r.result))
          (sparc.move as $r.result dest))
      (sparc.label as L1))))

; Comprehensible, but longer.
;
; Important to be careful not to clobber arguments, and not to leave garbage
; in rd, if millicode is called.
;
; op is the appropriate operation.
; invop is the appropriate inverse operation.
; RS1 can be any general hw register or RESULT.
; RS2/IMM can be any general register or ARGREG2 (op2isreg=#t), or 
;         an immediate (op2isreg=#f)
; RD can be any general hw register or RESULT.
;
; FIXME: split this into two procedures.

(define (emit-arith-primop! as op invop generic rs1 rs2/imm rd op2isreg)
  (let ((L1 (new-label)))
    (if op2isreg
        (let ((rs2 (force-hwreg! as rs2/imm $r.argreg2)))
          (cond ((or (= rs1 rs2 rd)
                     (and (= rs2 rd)
                          (= generic $m.subtract)))
                 (op          as rs1 rs2 $r.tmp0)
                 (sparc.bvc.a as L1)
                 (sparc.move  as $r.tmp0 rd))
                ((= rs1 rd)
                 (op          as rs1 rs2 rs1)
                 (sparc.bvc.a as L1)
                 (sparc.slot  as)
                 (invop       as rs1 rs2 rs1))
                ((= rs2 rd)
                 (op          as rs1 rs2 rs2)
                 (sparc.bvc.a as L1)
                 (sparc.slot  as)
                 (invop       as rs2 rs1 rs2))
                (else
                 (op          as rs1 rs2 rd)
                 (sparc.bvc.a as L1)
                 (sparc.slot  as)
                 (if (and (not (= rd $r.result)) (not (= rd $r.argreg2)))
                     (sparc.clr as rd))))
          (cond ((and (= rs1 $r.result) (= rs2 $r.argreg2))
                 ;; Could peephole the INVOP or CLR into the slot here.
                 (millicode-call/0arg as generic))
                ((= rs1 $r.result)
                 (millicode-call/1arg as generic rs2))
                ((= rs2 $r.argreg2)
                 (millicode-call/1arg-in-result as generic rs1))
                (else
                 (sparc.move as rs2 $r.argreg2)
                 (millicode-call/1arg-in-result as generic rs1))))
        (let ((imm (thefixnum rs2/imm)))
          (op          as rs1 imm rd)
          (sparc.bvc.a as L1)
          (sparc.slot  as)
          (invop       as rd imm rd)
          (if (not (= rs1 $r.result))
              (sparc.move as rs1 $r.result))
          (millicode-call/numarg-in-reg as generic imm $r.argreg2)))
    (if (not (= rd $r.result))
        (sparc.move as $r.result rd))
    (sparc.label as L1)))


; Important to be careful not to leave garbage in rd if millicode is called.

(define (emit-negate as rs rd)
  (let ((L1 (new-label)))
    (cond ((= rs rd)
           (sparc.tsubrcc as $r.g0 rs rs)
           (sparc.bvc.a   as L1)
           (sparc.slot    as)
           (if (= rs $r.result)
               (begin 
                 (sparc.jmpli as $r.millicode $m.negate $r.o7)
                 (sparc.subr  as $r.g0 $r.result $r.result))
               (begin
                 (sparc.subr  as $r.g0 rs rs)
                 (sparc.jmpli as $r.millicode $m.negate $r.o7)
                 (sparc.move  as rs $r.result))))
          (else
           (sparc.tsubrcc as $r.g0 rs rd)
           (sparc.bvc.a   as L1)
           (sparc.slot    as)
           (cond ((= rs $r.result)
                  (sparc.jmpli as $r.millicode $m.negate $r.o7)
                  (sparc.clr   as rd))
                 ((= rd $r.result)
                  (sparc.jmpli as $r.millicode $m.negate $r.o7)
                  (sparc.move  as rs $r.result))
                 (else
                  (sparc.clr   as rd)
                  (sparc.jmpli as $r.millicode $m.negate $r.o7)
                  (sparc.move  as rs $r.result)))))
    (if (not (= rd $r.result))
        (sparc.move as $r.result rd))
    (sparc.label   as L1)))

; Character comparison.

; r is a register or a character constant.

(define (emit-char-cmp as r btrue.a excode)
  (emit-charcmp! as (lambda ()
                      (let ((L2 (new-label)))
                        (sparc.set   as $imm.false $r.result)
                        (btrue.a     as L2)
                        (sparc.set   as $imm.true $r.result)
                        (sparc.label as L2)))
                 $r.result
                 r
                 excode))
 
; op1 is a hw register
; op2 is a register or a character constant

(define (emit-char-bcmp-primop! as bfalse.a op1 op2 L0 excode)
  (emit-charcmp! as (lambda ()
                      (bfalse.a   as L0)
                      (sparc.slot as))
                 op1
                 op2
                 excode))

; We check the tags of both by xoring them and seeing if the low byte is 0.
; If so, then we can subtract one from the other (tag and all) and check the
; condition codes.  
;
; The branch-on-true instruction must have the annull bit set. (???)
;
; op1 is a hw register
; op2 is a register or a character constant.

(define (emit-charcmp! as tail op1 op2 excode)
  (let ((op2 (if (char? op2)
                 op2
                 (force-hwreg! as op2 $r.argreg2))))
    (cond ((not (unsafe-code))
           (let ((L0 (new-label))
                 (L1 (new-label))
                 (FAULT (new-label)))
             (sparc.label as L0)
             (cond ((char? op2)
                    (sparc.xori  as op1 $imm.character $r.tmp0)
                    (sparc.btsti as $r.tmp0 #xFF)
                    (sparc.srli  as op1 16 $r.tmp0)
                    (sparc.be.a  as L1)
                    (sparc.cmpi  as $r.tmp0 (char->integer op2)))
                   (else
                    (sparc.andi  as op1 #xFF $r.tmp0)
                    (sparc.andi  as op2 #xFF $r.tmp1)
                    (sparc.cmpr  as $r.tmp0 $r.tmp1)
                    (sparc.bne   as FAULT)
                    (sparc.cmpi  as $r.tmp0 $imm.character)
                    (sparc.be.a  as L1)
                    (sparc.cmpr  as op1 op2)))
             (sparc.label as FAULT)
             (if (not (eqv? op1 $r.result))
                 (sparc.move as op1 $r.result))
             (cond ((char? op2) 
                    (emit-immediate->register! as
                                               (char->immediate op2)
                                               $r.argreg2))
                   ((not (eqv? op2 $r.argreg2))
                    (sparc.move as op2 $r.argreg2)))
             (sparc.set   as (thefixnum excode) $r.tmp0)
             (millicode-call/ret as $m.exception L0)
             (sparc.label as L1)))
          ((not (char? op2))
           (sparc.cmpr as op1 op2))
          (else
           (sparc.srli as op1 16 $r.tmp0)
           (sparc.cmpi as $r.tmp0 (char->integer op2))))
    (tail)))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; SPARC code generation macros for primitives, part 3b:
;   helper procedures for data structures.


; SET-CAR!, SET-CDR!, CELL-SET!
;
; Input:  RS1: a hardware register; has pair pointer (tag check must be
;         performed by the caller).
;         RS2: any register; has value to store.
; Output: None.
;
; Having rs1 != RESULT is pretty silly with the current write barrier
; but will be less silly with the new barrier.

(define (emit-setcar/setcdr! as rs1 rs2 offs)
  (cond ((and (write-barrier) (hardware-mapped? rs2))
	 (sparc.sti as rs2 (- offs $tag.pair-tag) rs1)
         (if (not (= rs1 $r.result))
             (sparc.move as rs1 $r.result))
         (millicode-call/1arg as $m.addtrans rs2))
        ((write-barrier)
         (emit-move2hwreg! as rs2 $r.argreg2)
         (sparc.sti as $r.argreg2 (- offs $tag.pair-tag) rs1)
         (millicode-call/1arg-in-result as $m.addtrans rs1))
        ((hardware-mapped? rs2)
         (sparc.sti as rs2 (- offs $tag.pair-tag) rs1))
        (else
         (emit-move2hwreg! as rs2 $r.argreg2)
         (sparc.sti as $r.argreg2 (- offs $tag.pair-tag) rs1))))




; Representation predicate.
;
; RESULT has an object.  If the tag of RESULT is 'tag1' and the 
; header byte of the object is 'tag2' then set RESULT to #t, else
; set it to #f.

(define (emit-double-tagcheck->bool! as tag1 tag2)
  (let ((L1 (new-label)))
    (sparc.andi  as $r.result $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.ldbi  as $r.result (+ (- tag1) 3) $r.tmp0)
    (sparc.set   as $imm.true $r.result)
    (sparc.cmpi  as $r.tmp0 tag2)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.label as L1)))


; Check structure tag.
;
; RS1 has an object.  If the tag of RS1 is not 'tag1', or if the tag is 
; 'tag1' but the header byte of the object header is not 'tag2', then an
; exception with code 'excode' is signalled.  The exception call is set
; up to return to the first instruction of the emitted code.
;
; If RS1 is not RESULT then it is moved to RESULT before the exception 
; is signalled.
;
; If RS2/IMM is not #f, then it is a register or immediate that is moved
; to ARGREG2 before the exception is signalled; it is an immediate iff 
; imm? = #t.  
;
; RS1 must be a hardware register.
; RS2/IMM is a general register, ARGREG2, an immediate, or #f.
; RS3 is a general register, ARGREG3, or #f.
;
; The procedure returns the label of the fault address.  If the execution
; falls off the end of the emitted instruction sequence, then the following
; are true:
;  - the tag of the object in RS1 was 'tag1' and its header byte was 'tag2'
;  - the object header word is in TMP0.

(define (double-tagcheck-assert as tag1 tag2 rs1 rs2/imm rs3 excode imm?)
  (let ((L0    (new-label))
        (L1    (new-label))
        (FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as rs1 $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.be.a  as L1)
    (sparc.ldi   as rs1 (- tag1) $r.tmp0)
    (sparc.label as FAULT)
    (if (not (= rs1 $r.result))
        (sparc.move as rs1 $r.result))
    (if rs2/imm 
        (cond (imm?
               (sparc.set as (thefixnum rs2/imm) $r.argreg2))
              ((= rs2/imm $r.argreg2))
              (else
               (emit-move2hwreg! as rs2/imm $r.argreg2))))
    (if (and rs3 (not (= rs3 $r.argreg3)))
        (emit-move2hwreg! as rs3 $r.argreg3))
    (sparc.set   as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception L0)
    (sparc.label as L1)
    (sparc.andi  as $r.tmp0 255 $r.tmp1)
    (sparc.cmpi  as $r.tmp1 tag2)
    (sparc.bne.a as FAULT)
    (sparc.slot  as)
    FAULT))

(define (emit-double-tagcheck-assert! as tag1 tag2 excode reg2)
  (double-tagcheck-assert as tag1 tag2 $r.result reg2 #f excode #f))

(define (emit-double-tagcheck-assert-reg/reg! as tag1 tag2 rs1 rs2 excode)
  (double-tagcheck-assert as tag1 tag2 rs1 rs2 #f excode #f))
  
(define (emit-double-tagcheck-assert-reg/imm! as tag1 tag2 rs1 imm excode)
  (double-tagcheck-assert as tag1 tag2 rs1 imm #f excode #t))
  



; Get the length of a vector or bytevector structure, with tag checking
; included.
;
; Input: RS and RD are both hardware registers.

(define (emit-get-length! as tag1 tag2 excode rs rd)
  (if (not (unsafe-code))
      (if tag2
          (emit-double-tagcheck-assert-reg/reg! as tag1 tag2 rs rd excode)
          (emit-single-tagcheck-assert-reg! as tag1 rs rd excode)))
  (emit-get-length-trusted! as tag1 rs rd))

; Get the length of a vector or bytevector structure, without tag checking.
;
; Input: RS and RD are both hardware registers.

(define (emit-get-length-trusted! as tag1 rs rd)
  (sparc.ldi  as rs (- tag1) $r.tmp0)
  (sparc.srli as $r.tmp0 8 rd)
  (if (= tag1 $tag.bytevector-tag)
      (sparc.slli as rd 2 rd)))


; Allocate a bytevector, leave untagged pointer in RESULT.

(define (emit-allocate-bytevector as hdr preserved-result)

  ; Preserve the length field, then calculate the number of words
  ; to allocate.  The value `28' is an adjustment of 3 (for rounding 
  ; up) plus another 4 bytes for the header, all represented as a fixnum.

  (if (not preserved-result)
      (sparc.move as $r.result $r.argreg2))
  (sparc.addi as $r.result 28 $r.result)
  (sparc.andi as $r.result (asm:signed #xFFFFFFF0) $r.result)

  ; Allocate space

  (sparc.jmpli as $r.millicode $m.alloc-bv $r.o7)
  (sparc.srai  as $r.result 2 $r.result)
  
  ; Setup the header.

  (if (not preserved-result)
      (sparc.slli as $r.argreg2 6 $r.tmp0)
      (sparc.slli as preserved-result 6 $r.tmp0))
  (sparc.addi as $r.tmp0 hdr $r.tmp0)
  (sparc.sti  as $r.tmp0 0 $r.result))


; Given a nativeint count, a pointer to the first element of a 
; bytevector-like structure, and a byte value, fill the bytevector
; with the byte value.

(define (emit-bytevector-fill as r-bytecount r-pointer r-value)
  (let ((L2 (new-label))
        (L1 (new-label)))
    (sparc.label  as L2)
    (sparc.deccc  as r-bytecount)
    (sparc.bge.a  as L2)
    (sparc.stbr   as r-value r-bytecount r-pointer)
    (sparc.label  as L1)))


; BYTEVECTOR-REF, BYTEVECTOR-LIKE-REF, STRING-REF.
;
; The pointer in RS1 is known to be bytevector-like.  RS2 is the fixnum
; index into the structure.  Get the RS2'th element and place it in RD.
;
; RS1 and RD are hardware registers.
; RS2 is a general register or ARGREG2.
; 'fault' is defined iff (unsafe-code) = #f
; header is in TMP0 iff (unsafe-code) = #f and 'header-loaded?' = #t
; if 'charize?' is #t then store result as char, otherwise as fixnum.

(define (emit-bytevector-like-ref! as rs1 rs2 rd fault charize? header-loaded?)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
        (begin
          ; check that index is fixnum
          (sparc.btsti  as rs2 3)
          (sparc.bne    as fault)
          (if (not header-loaded?)
              (sparc.ldi as rs1 (- $tag.bytevector-tag) $r.tmp0))
          ; check length
          (sparc.srai   as rs2 2 $r.tmp1)
          (sparc.srli   as $r.tmp0 8 $r.tmp0)
          (sparc.cmpr   as $r.tmp0 $r.tmp1)
          (sparc.bleu as fault)
          ; No NOP or SLOT -- the SUBI below goes into the slot.
          )
        (begin
          (sparc.srai   as rs2 2 $r.tmp1)))
    ; Pointer is in RS1.
    ; Shifted index is in TMP1.
    (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
    (sparc.ldbr as $r.tmp0 $r.tmp1 $r.tmp0)
    (if (not charize?)
        (sparc.slli as $r.tmp0 2 rd)
        (begin (sparc.slli as $r.tmp0 16 rd)
               (sparc.ori  as rd $imm.character rd)))))

; As above, but RS2 is replaced by an immediate, IMM.
;
; The immediate, represented as a fixnum, is guaranteed fit in the 
; instruction's immediate field.

(define (emit-bytevector-like-ref/imm! as rs1 imm rd fault charize?
                                       header-loaded?)
  (if (not (unsafe-code))
      (begin
        (if (not header-loaded?)
            (sparc.ldi as rs1 (- $tag.bytevector-tag) $r.tmp0))
        ; Range check.
        (sparc.srli   as $r.tmp0 8 $r.tmp0)
        (sparc.cmpi   as $r.tmp0 imm)
        (sparc.bleu.a as fault)
        (sparc.slot   as)))

  ; Pointer is in RS1.

  (let ((adjusted-offset (+ (- 4 $tag.bytevector-tag) imm)))
    (if (immediate-literal? adjusted-offset)
        (begin
          (sparc.ldbi as rs1 adjusted-offset $r.tmp0))
        (begin
          (sparc.addi as rs1 (- 4 $tag.bytevector-tag) $r.tmp0)
          (sparc.ldbr as $r.tmp0 imm $r.tmp0)))
    (if (not charize?)
        (sparc.slli as $r.tmp0 2 rd)
        (begin (sparc.slli as $r.tmp0 16 rd)
               (sparc.ori  as rd $imm.character rd)))))


; BYTEVECTOR-SET!, BYTEVECTOR-LIKE-SET!
;
; Input:  RESULT -- a pointer to a bytevector-like structure.
;         TMP0   -- the header iff (unsafe-code) = #f and header-loaded? = #t
;         IDX    -- a register that holds the second argument
;         BYTE   -- a register that holds the third argument
; Output: Nothing.
;
; 'Fault' is the address of the error code iff (unsafe-code) = #f
;
; FIXME: 
;   - Argument values passed to error handler appear to be bogus 
;     (error message is very strange).
;   - There's no check that the value actually fits in a byte.
;   - Uses ARGREG3 and and TMP2.

(define (emit-bytevector-like-set! as idx byte fault header-loaded?)
  (let ((r1 (force-hwreg! as idx $r.tmp1))
        (r2 (force-hwreg! as byte $r.argreg3)))
    (if (not (unsafe-code))
        (begin
          (if (not header-loaded?)
              (sparc.ldi     as $r.result (- $tag.bytevector-tag) $r.tmp0))
          ; Both index and byte must be fixnums.  
          ; Can't use tsubcc because the computation may really overflow.
          (sparc.orr     as r1 r2 $r.tmp2)
          (sparc.btsti   as $r.tmp2 3)
          (sparc.bnz     as fault)
          ; No NOP -- next instruction is OK in slot.
          ; Index must be in range.
          (sparc.srli    as $r.tmp0 8 $r.tmp0)    ; limit - in slot
          (sparc.srai    as r1 2 $r.tmp1)         ; index
          (sparc.cmpr    as $r.tmp1 $r.tmp0)
          (sparc.bgeu    as fault)
          ; No NOP -- next instruction is OK in slot.
          )
        (begin
          (sparc.srai   as r1 2 $r.tmp1)))
    (sparc.srli as r2 2 $r.tmp0)
    ; Using ARGREG2 as the destination is OK because the resulting pointer
    ; value always looks like a fixnum.  By doing so, we avoid needing TMP2.
    (sparc.addi as $r.result (- 4 $tag.bytevector-tag) $r.argreg2)
    (sparc.stbr as $r.tmp0 $r.tmp1 $r.argreg2)))


; STRING-SET!

(define (emit-string-set! as rs1 rs2 rs3)
  (let* ((rs2 (force-hwreg! as rs2 $r.argreg2))
         (rs3 (force-hwreg! as rs3 $r.argreg3))
         (FAULT (if (not (unsafe-code))
                    (double-tagcheck-assert 
                     as 
                     $tag.bytevector-tag
                     (+ $imm.bytevector-header $tag.string-typetag)
                     rs1 rs2 rs3
                     $ex.sset
                     #f))))
    ; Header is in TMP0; TMP1 and TMP2 are free.
    (if (not (unsafe-code))
        (begin
          ; RS2 must be a fixnum.
          (sparc.btsti  as rs2 3)
          (sparc.bne    as FAULT)
          ; Index (in RS2) must be valid; header is in tmp0.
          (sparc.srli   as $r.tmp0 8 $r.tmp0) ; limit
          (sparc.srai   as rs2 2 $r.tmp1) ; index
          (sparc.cmpr   as $r.tmp1 $r.tmp0)
          (sparc.bgeu   as FAULT)
          ; RS3 must be a character.
          (sparc.andi   as rs3 #xFF $r.tmp0)
          (sparc.cmpi   as $r.tmp0 $imm.character)
          (sparc.bne    as FAULT)
          ; No NOP -- the SRLI below goes in the slot
          )
        (begin
          (sparc.srai as rs2 2 $r.tmp1)))
    ; tmp1 has nativeint index. 
    ; rs3/argreg3 has character.
    ; tmp0 is garbage.
    (sparc.subi as $r.tmp1 (- $tag.bytevector-tag 4) $r.tmp1)
    (sparc.srli as rs3 16 $r.tmp0)
    (sparc.stbr as $r.tmp0 rs1 $r.tmp1)))


; VECTORS and PROCEDURES

; Allocate short vectors of known length; faster than the general case.
; FIXME: can also allocate in-line.

(define (make-vector-n as length r)
  (sparc.jmpli as $r.millicode $m.alloc $r.o7)
  (sparc.set  as (thefixnum (+ length 1)) $r.result)
  (emit-immediate->register! as (+ (* 256 (thefixnum length))
                                   $imm.vector-header
                                   $tag.vector-typetag)
                             $r.tmp0)
  (sparc.sti  as $r.tmp0 0 $r.result)
  (let ((dest (force-hwreg! as r $r.argreg2)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (sparc.sti as dest (* (+ i 1) 4) $r.result)))
  (sparc.addi as $r.result $tag.vector-tag $r.result))


; emit-make-vector-like! assumes argreg3 is not destroyed by alloci.
; FIXME: bug: $ex.mkvl is not right if the operation is make-procedure
; or make-vector.

(define (emit-make-vector-like! as r hdr ptrtag)
  (let ((FAULT (emit-assert-positive-fixnum! as $r.result $ex.mkvl)))
    (sparc.move  as $r.result $r.argreg3)
    (sparc.addi  as $r.result 4 $r.result)
    (sparc.jmpli as $r.millicode $m.alloci $r.o7)
    (if (null? r)
        (sparc.set as $imm.null $r.argreg2)
        (emit-move2hwreg! as r $r.argreg2))
    (sparc.slli  as $r.argreg3 8 $r.tmp0)
    (sparc.addi  as $r.tmp0 hdr $r.tmp0)
    (sparc.sti   as $r.tmp0 0 $r.result)
    (sparc.addi  as $r.result ptrtag $r.result)))


; VECTOR-REF, VECTOR-LIKE-REF, PROCEDURE-REF
;
; FAULT is valid iff (unsafe-code) = #f
; Header is in TMP0 iff (unsafe-code) = #f and header-loaded? = #t.

(define (emit-vector-like-ref! as rs1 rs2 rd FAULT tag header-loaded?)
  (let ((index (force-hwreg! as rs2 $r.argreg2)))
    (if (not (unsafe-code))
        (begin
         (if (not header-loaded?)
             (sparc.ldi   as rs1 (- tag) $r.tmp0))
         ; Index must be fixnum.
         (sparc.btsti as index 3)
         (sparc.bne   as FAULT)
         ; Index must be within bounds.
         (sparc.srai  as $r.tmp0 8 $r.tmp0)
         (sparc.cmpr  as $r.tmp0 index)
         (sparc.bleu  as FAULT)
         ; No NOP; the following instruction is valid in the slot.
         ))
    (emit-vector-like-ref-trusted! as rs1 index rd tag)))

(define (emit-vector-like-ref-trusted! as rs1 rs2 rd tag)
  (let ((index (force-hwreg! as rs2 $r.argreg2)))
    (sparc.addi as rs1 (- 4 tag) $r.tmp0)
    (sparc.ldr  as $r.tmp0 index rd)))


; VECTOR-REF/IMM, VECTOR-LIKE-REF/IMM, PROCEDURE-REF/IMM
;
; 'rs1' is a hardware register containing a vectorish pointer (to a
;       vector-like or procedure).
; 'imm' is a fixnum s.t. (immediate-literal? imm) => #t.
; 'rd' is a hardware register.
; 'FAULT' is the label of the error code iff (unsafe-code) => #f
; 'tag' is the tag of the pointer in rs1.
; 'header-loaded?' is #t iff the structure header word is in $r.tmp0.

(define (emit-vector-like-ref/imm! as rs1 imm rd FAULT tag header-loaded?)
  (if (not (unsafe-code))
      (begin
        (if (not header-loaded?) (sparc.ldi as rs1 (- tag) $r.tmp0))
        ; Check bounds.
        (sparc.srai  as $r.tmp0 10 $r.tmp0)
        (sparc.cmpi  as $r.tmp0 imm)
        (sparc.bleu  as FAULT)
        (sparc.nop   as)))
  (emit-vector-like-ref/imm-trusted! as rs1 imm rd tag))

; 'rs1' is a hardware register containing a vectorish pointer (to a
;       vector-like or procedure).
; 'imm' is a fixnum s.t. (immediate-literal? imm) => #t.
; 'rd' is a hardware register.
; 'tag' is the tag of the pointer in rs1.

(define (emit-vector-like-ref/imm-trusted! as rs1 imm rd tag)
  (let* ((offset (* imm 4))                       ; words->bytes
         (adjusted-offset (+ (- 4 tag) offset)))
    (if (immediate-literal? adjusted-offset)
        (begin
          (sparc.ldi as rs1 adjusted-offset rd))
        (begin
          (sparc.addi as rs1 (- 4 tag) $r.tmp0)
          (sparc.ldi  as $r.tmp0 offset rd)))))



; VECTOR-SET!, VECTOR-LIKE-SET!, PROCEDURE-SET!
;
; It is assumed that the pointer in RESULT is valid. We must check the index
; in register x for validity and then perform the side effect (by calling
; millicode). The tag is the pointer tag to be adjusted for.
;
; The use of vector-set is ok even if it is a procedure.

; fault is valid iff (unsafe-code) = #f
; header is in tmp0 iff (unsafe-code) = #f and header-loaded? = #t

(define (emit-vector-like-set! as rs1 rs2 rs3 fault tag header-loaded?)
  (let ((rs2 (force-hwreg! as rs2 $r.tmp1))
        (rs3 (force-hwreg! as rs3 $r.argreg2)))
    (if (not (unsafe-code))
        (begin 
         (if (not header-loaded?)
             (sparc.ldi as $r.result (- tag) $r.tmp0))
         (sparc.btsti as rs2 3)
         (sparc.bne   as fault)
         (sparc.srai  as $r.tmp0 8 $r.tmp0)
         (sparc.cmpr  as $r.tmp0 rs2)
         (sparc.bleu  as fault)))
    (emit-vector-like-set-trusted! as rs1 rs2 rs3 tag)))

; rs1 must be a hardware register.
; tag is the pointer tag to be adjusted for.

(define (emit-vector-like-set-trusted! as rs1 rs2 rs3 tag)
  (let ((rs2 (force-hwreg! as rs2 $r.tmp1))
        (rs3 (force-hwreg! as rs3 $r.argreg2)))
    ;; The ADDR can go in the delay slot of a preceding BLEU.
    (sparc.addr as rs1 rs2 $r.tmp0)
    (cond ((not (write-barrier))
           (sparc.sti  as rs3 (- 4 tag) $r.tmp0))
          ((= rs1 $r.result)
           (cond ((= rs3 $r.argreg2)
                  (sparc.jmpli as $r.millicode $m.addtrans $r.o7)
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0))
                 (else
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
                  (millicode-call/1arg as $m.addtrans rs3))))
          (else
           (cond ((= rs3 $r.argreg2)
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
                  (millicode-call/1arg-in-result as $m.addtrans rs1))
                 (else
                  (sparc.sti  as rs3 (- 4 tag) $r.tmp0)
                  (sparc.move as rs1 $r.result)
                  (millicode-call/1arg as $m.addtrans rs3)))))))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; 9 May 1999 / wdc
;
; SPARC code generation macros for primitives, part 3:
;   fixnum-specific operations.
;
; Constraints for all the primops.
;
; RS1 is a general hardware register or RESULT.
; RS2 is a general register or ARGREG2.
; IMM is an exact integer in the range -1024 .. 1023.
; RD is a general hardware register or RESULT.

; FIXME
;   Missing fxquotient, fxremainder
;   When new pass1 in place:
;     Must add code to pass1 to allow n-ary calls to be rewritten as binary
;     Must add compiler macro for fxabs.


; most-negative-fixnum, most-positive-fixnum.

(define-primop 'most-negative-fixnum
  (lambda (as)
    (emit-immediate->register! as (asm:signed #x80000000) $r.result)))

(define-primop 'most-positive-fixnum
  (lambda (as)
    (emit-immediate->register! as (asm:signed #x7FFFFFFC) $r.result)))


; fx+, fx- w/o immediates

(define-primop 'fx+
  (lambda (as rs2)
    (emit-fixnum-arithmetic as sparc.taddrcc sparc.addr $r.result rs2 $r.result
			    $ex.fx+)))

(define-primop 'internal:fx+
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-arithmetic as sparc.taddrcc sparc.addr rs1 rs2 rd $ex.fx+)))

(define-primop 'fx-
  (lambda (as rs2)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr $r.result rs2 $r.result
			    $ex.fx-)))

(define-primop 'internal:fx-
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr rs1 rs2 rd $ex.fx-)))

(define-primop 'fx--
  (lambda (as)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr
			    $r.g0 $r.result $r.result $ex.fx--)))

(define-primop 'internal:fx--
  (lambda (as rs rd)
    (emit-fixnum-arithmetic as sparc.tsubrcc sparc.subr $r.g0 rs rd $ex.fx--)))

(define (emit-fixnum-arithmetic as op-check op-nocheck rs1 rs2 rd exn)
  (if (unsafe-code)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
	(op-nocheck as rs1 rs2 rd))
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2))
	    (L0  (new-label))
	    (L1  (new-label)))
	(sparc.label  as L0)
	(op-check     as rs1 rs2 $r.tmp0)
	(sparc.bvc.a  as L1)
	(sparc.move   as $r.tmp0 rd)
        (if (not (= exn $ex.fx--))
            (begin
              (if (not (= rs1 $r.result)) (sparc.move as rs1 $r.result))
              (if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2)))
            (begin
              (if (not (= rs2 $r.result)) (sparc.move as rs2 $r.result))))
	(sparc.set    as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label  as L1))))

; fx* w/o immediate

(define-primop 'fx*
  (lambda (as rs2)
    (emit-multiply-code as rs2 #t)))

; fx+, fx- w/immediates

(define-primop 'internal:fx+/imm
  (lambda (as rs imm rd)
    (emit-fixnum-arithmetic/imm as sparc.taddicc sparc.addi
				rs imm rd $ex.fx+)))

(define-primop 'internal:fx-/imm
  (lambda (as rs imm rd)
    (emit-fixnum-arithmetic/imm as sparc.tsubicc sparc.subi
				rs imm rd $ex.fx-)))

(define (emit-fixnum-arithmetic/imm as op-check op-nocheck rs imm rd exn)
  (if (unsafe-code)
      (op-nocheck as rs (thefixnum imm) rd)
      (let ((L0  (new-label))
	    (L1  (new-label)))
	(sparc.label  as L0)
	(op-check     as rs (thefixnum imm) $r.tmp0)
	(sparc.bvc.a  as L1)
	(sparc.move   as $r.tmp0 rd)
	(if (not (= rs $r.result)) (sparc.move as rs $r.result))
	(sparc.set    as (thefixnum imm) $r.argreg2)
	(sparc.set    as (thefixnum exn) $r.tmp0)
	(millicode-call/ret as $m.exception L0)
	(sparc.label  as L1))))


; fx=, fx<, fx<=, fx>, fx>=, fxpositive?, fxnegative?, fxzero? w/o immediates

(define-primop 'fx=
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bne.a $r.result rs2 $r.result $ex.fx= #f)))

(define-primop 'fx<
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bge.a $r.result rs2 $r.result $ex.fx< #f)))

(define-primop 'fx<=
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bg.a $r.result rs2 $r.result $ex.fx<= #f)))

(define-primop 'fx>
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.ble.a $r.result rs2 $r.result $ex.fx> #f)))

(define-primop 'fx>=
  (lambda (as rs2)
    (emit-fixnum-compare as sparc.bl.a $r.result rs2 $r.result $ex.fx>= #f)))

(define-primop 'internal:fx=
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bne.a rs1 rs2 rd $ex.fx= #f)))

(define-primop 'internal:fx<
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bge.a rs1 rs2 rd $ex.fx< #f)))

(define-primop 'internal:fx<=
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bg.a rs1 rs2 rd $ex.fx<= #f)))

(define-primop 'internal:fx>
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.ble.a rs1 rs2 rd $ex.fx> #f)))

(define-primop 'internal:fx>=
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare as sparc.bl.a rs1 rs2 rd $ex.fx>= #f)))


; Use '/imm' code for these because the generated code is better.

(define-primop 'fxpositive?
  (lambda (as)
    (emit-fixnum-compare/imm as sparc.ble.a $r.result 0 $r.result
			     $ex.fxpositive? #f)))

(define-primop 'fxnegative?
  (lambda (as)
    (emit-fixnum-compare/imm as sparc.bge.a $r.result 0 $r.result
				$ex.fxnegative? #f)))

(define-primop 'fxzero?
  (lambda (as)
    (emit-fixnum-compare/imm as sparc.bne.a $r.result 0 $r.result
				$ex.fxzero? #f)))

(define-primop 'internal:fxpositive?
  (lambda (as rs rd)
    (emit-fixnum-compare/imm as sparc.ble.a rs 0 rd $ex.fxpositive? #f)))

(define-primop 'internal:fxnegative?
  (lambda (as rs rd)
    (emit-fixnum-compare/imm as sparc.bge.a rs 0 rd $ex.fxnegative? #f)))

(define-primop 'internal:fxzero?
  (lambda (as rs rd)
    (emit-fixnum-compare/imm as sparc.bne.a rs 0 rd $ex.fxzero? #f)))


; fx=, fx<, fx<=, fx>, fx>=  w/immediates

(define-primop 'internal:fx=/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bne.a rs imm rd $ex.fx= #f)))

(define-primop 'internal:fx</imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bge.a rs imm rd $ex.fx< #f)))

(define-primop 'internal:fx<=/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bg.a rs imm rd $ex.fx<= #f)))

(define-primop 'internal:fx>/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.ble.a rs imm rd $ex.fx> #f)))

(define-primop 'internal:fx>=/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm as sparc.bl.a rs imm rd $ex.fx>= #f)))

; fx=, fx<, fx<=, fx>, fx>=, fxpositive?, fxnegative?, fxzero? w/o immediates
; for control.

(define-primop 'internal:branchf-fx=
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bne.a rs1 rs2 #f $ex.fx= L)))

(define-primop 'internal:branchf-fx<
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bge.a rs1 rs2 #f $ex.fx< L)))

(define-primop 'internal:branchf-fx<=
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bg.a rs1 rs2 #f $ex.fx<= L)))

(define-primop 'internal:branchf-fx>
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.ble.a rs1 rs2 #f $ex.fx> L)))

(define-primop 'internal:branchf-fx>=
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare as sparc.bl.a rs1 rs2 #f $ex.fx>= L)))

(define-primop 'internal:branchf-fxpositive?
  (lambda (as rs1 L)
    (emit-fixnum-compare/imm as sparc.ble.a rs1 0 #f $ex.fxpositive? L)))

(define-primop 'internal:branchf-fxnegative?
  (lambda (as rs1 L)
    (emit-fixnum-compare/imm as sparc.bge.a rs1 0 #f $ex.fxnegative? L)))

(define-primop 'internal:branchf-fxzero?
  (lambda (as rs1 L)
    (emit-fixnum-compare/imm as sparc.bne.a rs1 0 #f $ex.fxzero? L)))


; fx=, fx<, fx<=, fx>, fx>=  w/immediates for control.

(define-primop 'internal:branchf-fx=/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bne.a rs imm #f $ex.fx= L)))

(define-primop 'internal:branchf-fx</imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bge.a rs imm #f $ex.fx< L)))

(define-primop 'internal:branchf-fx<=/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bg.a rs imm #f $ex.fx<= L)))

(define-primop 'internal:branchf-fx>/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.ble.a rs imm #f $ex.fx> L)))

(define-primop 'internal:branchf-fx>=/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm as sparc.bl.a rs imm #f $ex.fx>= L)))


; Trusted fixnum comparisons.

(define-primop '=:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bne.a $r.result rs2 $r.result #f)))

(define-primop '<:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bge.a $r.result rs2 $r.result #f)))

(define-primop '<=:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bg.a $r.result rs2 $r.result #f)))

(define-primop '>:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.ble.a $r.result rs2 $r.result #f)))

(define-primop '>=:fix:fix
  (lambda (as rs2)
    (emit-fixnum-compare-trusted as sparc.bl.a $r.result rs2 $r.result #f)))

(define-primop 'internal:=:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bne.a rs1 rs2 rd #f)))

(define-primop 'internal:<:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bge.a rs1 rs2 rd #f)))

(define-primop 'internal:<=:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bg.a rs1 rs2 rd #f)))

(define-primop 'internal:>:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.ble.a rs1 rs2 rd #f)))

(define-primop 'internal:>=:fix:fix
  (lambda (as rs1 rs2 rd)
    (emit-fixnum-compare-trusted as sparc.bl.a rs1 rs2 rd #f)))

; With immediates.

(define-primop 'internal:=:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bne.a rs imm rd #f)))

(define-primop 'internal:<:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bge.a rs imm rd #f)))

(define-primop 'internal:<=:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bg.a rs imm rd #f)))

(define-primop 'internal:>:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.ble.a rs imm rd #f)))

(define-primop 'internal:>=:fix:fix/imm
  (lambda (as rs imm rd)
    (emit-fixnum-compare/imm-trusted as sparc.bl.a rs imm rd #f)))

; Without immediates, for control.

(define-primop 'internal:branchf-=:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bne.a rs1 rs2 #f L)))

(define-primop 'internal:branchf-<:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bge.a rs1 rs2 #f L)))

(define-primop 'internal:branchf-<=:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bg.a rs1 rs2 #f L)))

(define-primop 'internal:branchf->:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.ble.a rs1 rs2 #f L)))

(define-primop 'internal:branchf->=:fix:fix
  (lambda (as rs1 rs2 L)
    (emit-fixnum-compare-trusted as sparc.bl.a rs1 rs2 #f L)))

; With immediates, for control.

(define-primop 'internal:branchf-=:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bne.a rs imm #f L)))

(define-primop 'internal:branchf-<:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bge.a rs imm #f L)))

(define-primop 'internal:branchf-<=:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bg.a rs imm #f L)))

(define-primop 'internal:branchf->:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.ble.a rs imm #f L)))

(define-primop 'internal:branchf->=:fix:fix/imm
  (lambda (as rs imm L)
    (emit-fixnum-compare/imm-trusted as sparc.bl.a rs imm #f L)))

; Range check:  0 <= src1 < src2

(define-primop 'internal:check-range
  (lambda (as src1 src2 L1 livregs)
    (let ((src2 (force-hwreg! as src2 $r.argreg2)))
      (emit-fixnum-compare-check
       as src2 src1 sparc.bleu L1 livregs))))

; Trusted fixnum comparisons followed by a check.

(define-primop 'internal:check-=:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bne L1 liveregs)))

(define-primop 'internal:check-<:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bge L1 liveregs)))

(define-primop 'internal:check-<=:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bg L1 liveregs)))

(define-primop 'internal:check->:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.ble L1 liveregs)))

(define-primop 'internal:check->=:fix:fix
  (lambda (as src1 src2 L1 liveregs)
    (emit-fixnum-compare-check
     as src1 src2 sparc.bl L1 liveregs)))

(define-primop 'internal:check-=:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bne L1 liveregs)))

(define-primop 'internal:check-<:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bge L1 liveregs)))

(define-primop 'internal:check-<=:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bg L1 liveregs)))

(define-primop 'internal:check->:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.ble L1 liveregs)))

(define-primop 'internal:check->=:fix:fix/imm
  (lambda (as src1 imm L1 liveregs)
    (emit-fixnum-compare/imm-check
     as src1 imm sparc.bl L1 liveregs)))

; Below, 'target' is a label or #f.  If #f, RD must be a general hardware
; register or RESULT, and a boolean result is generated in RD.

(define (emit-fixnum-compare as branchf.a rs1 rs2 rd exn target)
  (if (unsafe-code)
      (emit-fixnum-compare-trusted as branchf.a rs1 rs2 rd target)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2))
            (L0 (new-label))
            (L1 (new-label)))
        (sparc.label as L0)
        (sparc.orr   as rs1 rs2 $r.tmp0)
        (sparc.btsti as $r.tmp0 3)
        (sparc.be.a  as L1)
        (sparc.cmpr  as rs1 rs2)
        (if (not (= rs1 $r.result)) (sparc.move as rs1 $r.result))
        (if (not (= rs2 $r.argreg2)) (sparc.move as rs2 $r.argreg2))
        (sparc.set   as (thefixnum exn) $r.tmp0)
        (millicode-call/ret as $m.exception L0)
        (sparc.label as L1)
        (emit-evaluate-cc! as branchf.a rd target))))

; Below, 'target' is a label or #f.  If #f, RD must be a general hardware
; register or RESULT, and a boolean result is generated in RD.

(define (emit-fixnum-compare-trusted as branchf.a rs1 rs2 rd target)
  (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
    (sparc.cmpr  as rs1 rs2)
    (emit-evaluate-cc! as branchf.a rd target)))

; rs must be a hardware register.

(define (emit-fixnum-compare/imm as branchf.a rs imm rd exn target)
  (if (unsafe-code)
      (emit-fixnum-compare/imm-trusted as branchf.a rs imm rd target)
      (let ((L0 (new-label))
            (L1 (new-label)))
        (sparc.label as L0)
        (sparc.btsti as rs 3)
        (sparc.be.a  as L1)
        (sparc.cmpi  as rs (thefixnum imm))
        (if (not (= rs $r.result)) (sparc.move as rs $r.result))
        (sparc.set   as (thefixnum imm) $r.argreg2)
        (sparc.set   as (thefixnum exn) $r.tmp0)
        (millicode-call/ret as $m.exception L0)
        (sparc.label as L1)))
  (emit-evaluate-cc! as branchf.a rd target))

; rs must be a hardware register.

(define (emit-fixnum-compare/imm-trusted as branchf.a rs imm rd target)
  (sparc.cmpi  as rs (thefixnum imm))
  (emit-evaluate-cc! as branchf.a rd target))

; Range checks.

(define (emit-fixnum-compare-check
         as src1 src2 branch-bad L1 liveregs)
  (internal-primop-invariant1 'emit-fixnum-compare-check src1)
  (let ((src2 (force-hwreg! as src2 $r.argreg2)))
    (sparc.cmpr    as src1 src2)
    (emit-checkcc! as branch-bad L1 liveregs)))

(define (emit-fixnum-compare/imm-check
         as src1 imm branch-bad L1 liveregs)
  (internal-primop-invariant1 'emit-fixnum-compare/imm-check src1)
  (sparc.cmpi    as src1 imm)
  (emit-checkcc! as branch-bad L1 liveregs))

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; SPARC machine assembler flags.
;
; 12 April 1999


; INTERNAL!
(define short-effective-addresses
  (make-twobit-flag 'short-effective-addresses))

(define runtime-safety-checking
  (make-twobit-flag 'runtime-safety-checking))

(define catch-undefined-globals
  (make-twobit-flag 'catch-undefined-globals))

(define inline-allocation
  (make-twobit-flag 'inline-allocation))
  
;(define inline-assignment
;  (make-twobit-flag 'inline-assignment))

(define write-barrier
  (make-twobit-flag 'write-barrier))  

(define peephole-optimization
  (make-twobit-flag 'peephole-optimization))

(define single-stepping
  (make-twobit-flag 'single-stepping))

(define fill-delay-slots
  (make-twobit-flag 'fill-delay-slots))

; For backward compatibility.

;(define unsafe-code
;  (make-twobit-flag 'unsafe-code))

(define (unsafe-code . args)
  (if (null? args)
      (not (runtime-safety-checking))
      (runtime-safety-checking (not (car args)))))

(define (display-assembler-flags which)
  (case which
    ((debugging)
     (display-twobit-flag single-stepping))
    ((safety)
     (display-twobit-flag write-barrier)
     ;(display-twobit-flag unsafe-code)
     (display-twobit-flag runtime-safety-checking)
     (if (runtime-safety-checking)
         (begin (display "  ")
                (display-twobit-flag catch-undefined-globals))))
    ((optimization)
     (display-twobit-flag peephole-optimization)
     (display-twobit-flag inline-allocation)
     ;  (display-twobit-flag inline-assignment)
     (display-twobit-flag fill-delay-slots))
    (else #t)))

(define (set-assembler-flags! mode)
  (case mode
    ((no-optimization)
     (set-assembler-flags! 'standard)
     (peephole-optimization #f)
     (fill-delay-slots #f))
    ((standard)
     (short-effective-addresses #t)
     (catch-undefined-globals #t)
     (inline-allocation #f)
     ; (inline-assignment #f)
     (peephole-optimization #t)
     (runtime-safety-checking #t)
     (write-barrier #t)
     (single-stepping #f)
     (fill-delay-slots #t))
    ((fast-safe default)
     (set-assembler-flags! 'standard)
     ; (inline-assignment #t)
     (inline-allocation #t))
    ((fast-unsafe)
     (set-assembler-flags! 'fast-safe)
     (catch-undefined-globals #f)
     (runtime-safety-checking #f))
    (else
     (error "set-assembler-flags!: unknown mode " mode))))

(set-assembler-flags! 'default)

; eof
; Copyright 1998 Lars T Hansen.
;
; $Id: twobit.sch,v 1.3 1999/08/23 19:14:26 lth Exp $
;
; SPARC disassembler.
;
; (disassemble-instruction instruction address)
;     => decoded-instruction
;
; (disassemble-codevector codevector)
;     => decoded-instruction-list
;
; (print-instructions decoded-instruction-list)
;     => unspecified
;     Also takes an optional port and optionally the symbol "native-names".
;
; (format-instruction decoded-instruction address larceny-names?) 
;     => string
; 
; A `decoded-instruction' is a list where the car is a mnemonic and
; the operands are appropriate for that mnemonic.
;
; A `mnemonic' is an exact nonnegative integer.  It encodes the name of
; the instruction as well as its attributes (operand pattern and instruction
; type).  See below for specific operations on mnemonics.

(define (disassemble-codevector cv)
  (define (loop addr ilist)
    (if (< addr 0)
	ilist
	(loop (- addr 4)
	      (cons (disassemble-instruction (bytevector-word-ref cv addr)
					     addr)
		    ilist))))
  (loop (- (bytevector-length cv) 4) '()))

(define disassemble-instruction)	    ; Defined below.

; Mnemonics

(define *asm-annul* 1)
(define *asm-immed* 2)
(define *asm-store* 4)
(define *asm-load* 8)
(define *asm-branch* 16)
(define *asm-freg* 32)
(define *asm-fpop* 64)
(define *asm-no-op2* 128)
(define *asm-no-op3* 256)

(define *asm-bits*
  `((a . ,*asm-annul*) (i . ,*asm-immed*) (s . ,*asm-store*)
    (l . ,*asm-load*) (b . ,*asm-branch*) (f . ,*asm-freg*)
    (fpop . ,*asm-fpop*) (no-op2 . ,*asm-no-op2*) (no-op3 . ,*asm-no-op3*)))

(define *asm-mnemonic-table* '())

(define mnemonic 
  (let ((n 0))
    (lambda (name . rest)
      (let* ((probe (assq name *asm-mnemonic-table*))
	     (code  (* 1024 
		       (if probe
			   (cdr probe)
			   (let ((code n))
			     (set! n (+ n 1))
			     (set! *asm-mnemonic-table*
				   (cons (cons name code)
					 *asm-mnemonic-table*))
			     code)))))
	(for-each (lambda (x)
		    (set! code (+ code (cdr (assq x *asm-bits*)))))
		  rest)
	code))))

(define (mnemonic:name mnemonic)
  (let ((mnemonic (quotient mnemonic 1024)))
    (let loop ((t *asm-mnemonic-table*))
      (cond ((null? t) #f)
	    ((= (cdar t) mnemonic) (caar t))
	    (else (loop (cdr t)))))))

(define (mnemonic=? m name)
  (= (quotient m 1024) (quotient (mnemonic name) 1024)))

(define (mnemonic:test bit)
  (lambda (mnemonic)
    (not (zero? (logand mnemonic bit)))))

(define (mnemonic:test-not bit)
  (lambda (mnemonic)
    (zero? (logand mnemonic bit))))

(define mnemonic:annul? (mnemonic:test *asm-annul*))
(define mnemonic:immediate? (mnemonic:test *asm-immed*))
(define mnemonic:store? (mnemonic:test *asm-store*))
(define mnemonic:load? (mnemonic:test *asm-load*))
(define mnemonic:branch? (mnemonic:test *asm-branch*))
(define mnemonic:freg? (mnemonic:test *asm-freg*))
(define mnemonic:fpop? (mnemonic:test *asm-fpop*))
(define mnemonic:op2? (mnemonic:test-not *asm-no-op2*))
(define mnemonic:op3? (mnemonic:test-not *asm-no-op3*))

; Instruction disassembler.

(let ()

  ;; Useful constants

  (define two^3 (expt 2 3))
  (define two^5 (expt 2 5))
  (define two^6 (expt 2 6))
  (define two^8 (expt 2 8))
  (define two^9 (expt 2 9))
  (define two^12 (expt 2 12))
  (define two^13 (expt 2 13))
  (define two^14 (expt 2 14))
  (define two^16 (expt 2 16))
  (define two^19 (expt 2 19))
  (define two^21 (expt 2 21))
  (define two^22 (expt 2 22))
  (define two^24 (expt 2 24))
  (define two^25 (expt 2 25))
  (define two^29 (expt 2 29))
  (define two^30 (expt 2 30))
  (define two^32 (expt 2 32))

  ;; Class 0 has branches and weirdness, like sethi and nop.
  ;; We dispatch first on the op2 field and then on the op3 field.

  (define class00
    (let ((b-table
	   (vector (mnemonic 'bn 'b)
		   (mnemonic 'be 'b)
		   (mnemonic 'ble 'b)
		   (mnemonic 'bl 'b)
		   (mnemonic 'bleu 'b)
		   (mnemonic 'bcs 'b)
		   (mnemonic 'bneg 'b)
		   (mnemonic 'bvs 'b)
		   (mnemonic 'ba 'b)
		   (mnemonic 'bne 'b)
		   (mnemonic 'bg 'b)
		   (mnemonic 'bge 'b)
		   (mnemonic 'bgu 'b)
		   (mnemonic 'bcc 'b)
		   (mnemonic 'bpos 'b)
		   (mnemonic 'bvc 'b)
		   (mnemonic 'bn 'a 'b)
		   (mnemonic 'be 'a 'b)
		   (mnemonic 'ble 'a 'b)
		   (mnemonic 'bl 'a 'b)
		   (mnemonic 'bleu 'a 'b)
		   (mnemonic 'bcs 'a 'b)
		   (mnemonic 'bneg 'a 'b)
		   (mnemonic 'bvs 'a 'b)
		   (mnemonic 'ba 'a 'b)
		   (mnemonic 'bne 'a 'b)
		   (mnemonic 'bg 'a 'b)
		   (mnemonic 'bge 'a 'b)
		   (mnemonic 'bgu 'a 'b)
		   (mnemonic 'bcc 'a 'b)
		   (mnemonic 'bpos 'a 'b)
		   (mnemonic 'bvc 'a 'b)))
	  (fb-table
	   (vector (mnemonic 'fbn 'b)
		   (mnemonic 'fbne 'b)
		   (mnemonic 'fblg 'b)
		   (mnemonic 'fbul 'b)
		   (mnemonic 'fbl 'b)
		   (mnemonic 'fbug 'b)
		   (mnemonic 'fbg 'b)
		   (mnemonic 'fbu 'b)
		   (mnemonic 'fba 'b)
		   (mnemonic 'fbe 'b)
		   (mnemonic 'fbue 'b)
		   (mnemonic 'fbge 'b)
		   (mnemonic 'fbuge 'b)
		   (mnemonic 'fble 'b)
		   (mnemonic 'fbule 'b)
		   (mnemonic 'fbo 'b)
		   (mnemonic 'fbn 'a 'b)
		   (mnemonic 'fbne 'a 'b)
		   (mnemonic 'fblg 'a 'b)
		   (mnemonic 'fbul 'a 'b)
		   (mnemonic 'fbl 'a 'b)
		   (mnemonic 'fbug 'a 'b)
		   (mnemonic 'fbg 'a 'b)
		   (mnemonic 'fbu 'a 'b)
		   (mnemonic 'fba 'a 'b)
		   (mnemonic 'fbe 'a 'b)
		   (mnemonic 'fbue 'a 'b)
		   (mnemonic 'fbge 'a 'b)
		   (mnemonic 'fbuge 'a 'b)
		   (mnemonic 'fble 'a 'b)
		   (mnemonic 'fbule 'a 'b)
		   (mnemonic 'fbo 'a 'b)))
	  (nop (mnemonic 'nop))
	  (sethi (mnemonic 'sethi)))

      (lambda (ip instr)
	(let ((op2 (op2field instr)))
	  (cond ((= op2 #b100)
		 (if (zero? (rdfield instr))
		     `(,nop)
		     `(,sethi ,(imm22field instr) ,(rdfield instr))))
		((= op2 #b010)
		 `(,(vector-ref b-table (rdfield instr))
		   ,(* 4 (imm22field instr))))
		((= op2 #b110)
		 `(,(vector-ref fb-table (rdfield instr))
		   ,(* 4 (imm22field instr))))
		(else
		 (disasm-error "Can't disassemble " (number->string instr 16)
			       " at ip=" ip
			       " with op2=" op2)))))))

  ;; Class 1 is the call instruction; there's no choice.

  (define (class01 ip instr)
    `(,(mnemonic 'call) ,(* 4 (imm30field instr))))

  ;; Class 2 is for the ALU. Dispatch on op3 field.

  (define class10
    (let ((op3-table
	   `#((,(mnemonic 'add)   ,(mnemonic 'add 'i))
	      (,(mnemonic 'and)   ,(mnemonic 'and 'i))
	      (,(mnemonic 'or)    ,(mnemonic 'or 'i))
	      (,(mnemonic 'xor)   ,(mnemonic 'xor 'i))
	      (,(mnemonic 'sub)   ,(mnemonic 'sub 'i))
	      (,(mnemonic 'andn)  ,(mnemonic 'andn 'i))
	      (,(mnemonic 'orn)   ,(mnemonic 'orn 'i))
	      (,(mnemonic 'xnor)  ,(mnemonic 'xnor 'i))
	      (0          0)
	      (0          0)
	      (0          0)                              ; 10
	      (,(mnemonic 'smul)  ,(mnemonic 'smul 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'sdiv)  ,(mnemonic 'sdiv 'i))
	      (,(mnemonic 'addcc) ,(mnemonic 'addcc 'i))
	      (,(mnemonic 'andcc) ,(mnemonic 'andcc 'i))
	      (,(mnemonic 'orcc)  ,(mnemonic 'orcc 'i))
	      (,(mnemonic 'xorcc) ,(mnemonic 'xorcc 'i))
	      (,(mnemonic 'subcc) ,(mnemonic 'subcc 'i))  ; 20
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'smulcc) ,(mnemonic 'smulcc 'i))
	      (0          0)
	      (0          0)
	      (0          0)                               ; 30
	      (,(mnemonic 'sdivcc) ,(mnemonic 'sdivcc 'i))
	      (,(mnemonic 'taddcc) ,(mnemonic 'taddcc 'i))
	      (,(mnemonic 'tsubcc) ,(mnemonic 'tsubcc 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'sll)   ,(mnemonic 'sll 'i))
	      (,(mnemonic 'srl)   ,(mnemonic 'srl 'i))
	      (,(mnemonic 'sra)   ,(mnemonic 'sra 'i))
	      (,(mnemonic 'rd)   0)                       ; 40
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'wr)  ,(mnemonic 'wr 'i))
	      (0          0)
	      (0          0)                               ; 50
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'jmpl)  ,(mnemonic 'jmpl 'i))
	      (0          0)
	      (0          0)
	      (0          0)
	      (,(mnemonic 'save)  ,(mnemonic 'save 'i))   ; 60
	      (,(mnemonic 'restore) ,(mnemonic 'restore 'i))
	      (0          0)
	      (0          0))))

      (lambda (ip instr)
	(let ((op3 (op3field instr)))
	  (if (or (= op3 #b110100) (= op3 #b110101))
	      (fpop-instruction ip instr)
	      (nice-instruction op3-table ip instr))))))


  ;; Class 3 is memory stuff.

  (define class11
    (let ((op3-table
	   `#((,(mnemonic 'ld 'l)    ,(mnemonic 'ld 'i 'l))
	      (,(mnemonic 'ldb 'l)   ,(mnemonic 'ldb 'i 'l))
	      (,(mnemonic 'ldh 'l)   ,(mnemonic 'ldh 'i 'l))
	      (,(mnemonic 'ldd 'l)   ,(mnemonic 'ldd 'i 'l))
	      (,(mnemonic 'st 's)    ,(mnemonic 'st 'i 's))
	      (,(mnemonic 'stb 's)   ,(mnemonic 'stb 'i 's))
	      (,(mnemonic 'sth 's)   ,(mnemonic 'sth 'i 's))
	      (,(mnemonic 'std 's)   ,(mnemonic 'std 'i 's))
	      (0          0)
	      (0          0)
	      (0          0)		; 10
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 20
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 30
	      (0          0)
	      (,(mnemonic 'ldf 'f 'l) ,(mnemonic 'ldf 'i 'f 'l))
	      (0          0)
	      (0          0)
	      (,(mnemonic 'lddf 'f 'l) ,(mnemonic 'lddf 'i 'f 'l))
	      (,(mnemonic 'stf 'f 's)  ,(mnemonic 'stf 'i 'f 's))
	      (0          0)
	      (0          0)
	      (,(mnemonic 'stdf 'f 's) ,(mnemonic 'stdf 'i 'f 's))
	      (0          0)		; 40
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 50
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)
	      (0          0)		; 60
	      (0          0)
	      (0          0)
	      (0          0))))

      (lambda (ip instr)
	(nice-instruction op3-table ip instr))))

  ;; For classes 2 and 3

  (define (nice-instruction op3-table ip instr)
    (let* ((op3  (op3field instr))
	   (imm  (ifield instr))
	   (rd   (rdfield instr))
	   (rs1  (rs1field instr))
	   (src2 (if (zero? imm)
		     (rs2field instr)
		     (imm13field instr))))
      (let ((op ((if (zero? imm) car cadr) (vector-ref op3-table op3))))
	`(,op ,rs1 ,src2 ,rd))))

  ;; Floating-point operate instructions

  (define (fpop-instruction ip instr)
    (let ((rd  (rdfield instr))
	  (rs1 (rs1field instr))
	  (rs2 (rs2field instr))
	  (fpop (fpop-field instr)))
      `(,(cdr (assv fpop fpop-names)) ,rs1 ,rs2 ,rd)))

  (define fpop-names
    `((#b000000001 . ,(mnemonic 'fmovs 'fpop 'no-op2))
      (#b000000101 . ,(mnemonic 'fnegs 'fpop 'no-op2))
      (#b000001001 . ,(mnemonic 'fabss 'fpop 'no-op2))
      (#b001000010 . ,(mnemonic 'faddd 'fpop))
      (#b001000110 . ,(mnemonic 'fsubd 'fpop))
      (#b001001010 . ,(mnemonic 'fmuld 'fpop))
      (#b001001110 . ,(mnemonic 'fdivd 'fpop))
      (#b001010010 . ,(mnemonic 'fcmpd 'fpop 'no-op3))))
      

  ;; The following procedures pick apart an instruction

  (define (op2field instr)
    (remainder (quotient instr two^22) two^3))

  (define (op3field instr)
    (remainder (quotient instr two^19) two^6))

  (define (ifield instr)
    (remainder (quotient instr two^13) 2))

  (define (rs2field instr)
    (remainder instr two^5))

  (define (rs1field instr)
    (remainder (quotient instr two^14) two^5))

  (define (rdfield instr)
    (remainder (quotient instr two^25) two^5))

  (define (imm13field instr)
    (let ((x (remainder instr two^13)))
      (if (not (zero? (quotient x two^12)))
	  (- x two^13)
	  x)))
	
  (define (imm22field instr)
    (let ((x (remainder instr two^22)))
      (if (not (zero? (quotient x two^21)))
	  (- x two^22)
	  x)))

  (define (imm30field instr)
    (let ((x (remainder instr two^30)))
      (if (not (zero? (quotient x two^29)))
	  (- x two^30)
	  x)))

  (define (fpop-field instr)
    (remainder (quotient instr two^5) two^9))

  (set! disassemble-instruction
	(let ((class-table (vector class00 class01 class10 class11)))
	  (lambda (instr addr)
	    ((vector-ref class-table (quotient instr two^30)) addr instr))))

  'disassemble-instruction)


; Instruction printer
;
; It assumes that the first instruction comes from address 0, and prints
; addresses (and relative addresses) based on that assumption.
;
; If the optional symbol native-names is supplied, then SPARC register
; names is used, and millicode calls are not annotated with millicode names.

(define (print-instructions ilist . rest)

  (define port (current-output-port))
  (define larceny-names? #t)

  (define (print-ilist ilist a)
    (if (null? ilist)
	'()
	(begin (display (format-instruction (car ilist) a larceny-names?)
			port)
	       (newline port)
	       (print-ilist (cdr ilist) (+ a 4)))))
  
  (do ((rest rest (cdr rest)))
      ((null? rest))
    (cond ((port? (car rest))
	   (set! port (car rest)))
	  ((eq? (car rest) 'native-names)
	   (set! larceny-names? #f))))
  
  (print-ilist ilist 0))

(define format-instruction)		    ; Defined below

(define *format-instructions-pretty* #t)

; Instruction formatter.

(let ()

  (define use-larceny-registers #t)

  (define sparc-register-table 
    (vector "%g0" "%g1" "%g2" "%g3" "%g4" "%g5" "%g6" "%g7"
	    "%o0" "%o1" "%o2" "%o3" "%o4" "%o5" "%o6" "%o7"
	    "%l0" "%l1" "%l2" "%l3" "%l4" "%l5" "%l6" "%l7"
	    "%i0" "%i1" "%i2" "%i3" "%i4" "%i5" "%i6"  "%i7"))

  (define larceny-register-table
    (make-vector 32 #f))

  (define (larceny-register-name reg . rest)
    (if (null? rest)
	(or (and use-larceny-registers
		 (vector-ref larceny-register-table reg))
	    (vector-ref sparc-register-table reg))
	(vector-set! larceny-register-table reg (car rest))))

  (define millicode-procs '())

  (define (float-register-name reg)
    (string-append "%f" (number->string reg)))
    
  (define op car)
  (define op1 cadr)
  (define op2 caddr)
  (define op3 cadddr)
  (define tabstring (string #\tab))

  (define (heximm n)
    (if (>= n 16)
	(string-append tabstring "! 0x" (number->string n 16))
	""))

  (define (millicode-name offset . rest)
    (if (null? rest)
	(let ((probe (assv offset millicode-procs)))
	  (if probe
	      (cdr probe)
	      "[unknown]"))
	(set! millicode-procs
	      (cons (cons offset (car rest)) millicode-procs))))

  (define (millicode-call offset)
    (string-append tabstring "! " (millicode-name offset)))

  (define (plus/minus n)
    (cond ((< n 0)
	   (string-append " - " (number->string (abs n))))
	  ((and (= n 0) *format-instructions-pretty*) "")
	  (else
	   (string-append " + " (number->string n)))))

  (define (srcreg instr extractor)
    (if (mnemonic:freg? (op instr))
	(float-register-name (extractor instr))
	(larceny-register-name (extractor instr))))
	
  (define (sethi instr)
    (string-append (number->string (* (op1 instr) 1024)) ", "
		   (larceny-register-name (op2 instr))
		   (heximm (* (op1 instr) 1024))))

  (define (rrr instr)
    (string-append (larceny-register-name (op1 instr)) ", "
		   (larceny-register-name (op2 instr)) ", "
		   (larceny-register-name (op3 instr))))

  (define (rir instr)
    (string-append (larceny-register-name (op1 instr)) ", "
		   (number->string (op2 instr)) ", "
		   (larceny-register-name (op3 instr))
		   (heximm (op2 instr))))

  (define (sir instr)
    (string-append (srcreg instr op3) ", [ "
		   (larceny-register-name (op1 instr))
		   (plus/minus (op2 instr)) " ]"))

  (define (srr instr)
    (string-append (srcreg instr op3) ", [ "
		   (larceny-register-name (op1 instr)) "+"
		   (larceny-register-name (op2 instr)) " ]"))
      
  (define (lir instr)
    (string-append "[ " (larceny-register-name (op1 instr))
		   (plus/minus (op2 instr)) " ], "
		   (srcreg instr op3)))

  (define (lrr instr)
    (string-append "[ " (larceny-register-name (op1 instr)) "+"
		   (larceny-register-name (op2 instr)) " ], "
		   (srcreg instr op3)))

  (define (bimm instr addr)
    (string-append "#" (number->string (+ (op1 instr) addr))))

  (define (jmpli instr)
    (string-append (larceny-register-name (op1 instr)) 
		   (plus/minus (op2 instr)) ", "
		   (larceny-register-name (op3 instr))
		   (if (and (= (op1 instr) $r.globals)
			    use-larceny-registers)
		       (millicode-call (op2 instr))
		       (heximm (op2 instr)))))

  (define (jmplr instr)
    (string-append (larceny-register-name (op1 instr)) "+"
		   (larceny-register-name (op2 instr)) ", "
		   (larceny-register-name (op3 instr))))

  (define (call instr addr)
    (string-append "#" (number->string (+ (op1 instr) addr))))

  (define (rd instr)
    (string-append "%y, " (srcreg instr op3)))

  (define (wr instr imm?)
    (if imm?
	(string-append (larceny-register-name (op1 instr)) ", "
		       (number->string (op2 instr)) ", %y"
		       (larceny-register-name (op3 instr)))
	(string-append (larceny-register-name (op1 instr)) ", "
		       (larceny-register-name (op2 instr)) ", %y")))

  (define (fpop instr op2-used? op3-used?)
    (string-append (float-register-name (op1 instr)) ", "
		   (cond ((and op2-used? op3-used?)
			  (string-append
			   (float-register-name (op2 instr)) ", "
			   (float-register-name (op3 instr))))
			 (op2-used?
			  (float-register-name (op2 instr)))
			 (else
			  (float-register-name (op3 instr))))))

  ;; If we want to handle instruction aliases (clr, mov, etc) then
  ;; the structure of this procedure must change, because as it is,
  ;; the printing of the name is independent of the operand values.

  (define (format-instr i a larceny-names?)
    (set! use-larceny-registers larceny-names?)
    (let ((m (car i)))
      (string-append (number->string a)
		     tabstring
		     (symbol->string (mnemonic:name m))
		     (if (mnemonic:annul? m) ",a" "")
		     tabstring
		     (cond ((mnemonic:store? m) 
			    (if (mnemonic:immediate? m) (sir i) (srr i)))
			   ((mnemonic:load? m)
			    (if (mnemonic:immediate? m) (lir i) (lrr i)))
			   ((mnemonic:fpop? m)
			    (fpop i (mnemonic:op2? m) (mnemonic:op3? m)))
			   ((mnemonic:branch? m) (bimm i a))
			   ((mnemonic=? m 'sethi) (sethi i))
			   ((mnemonic=? m 'nop) "")
			   ((mnemonic=? m 'jmpl)
			    (if (mnemonic:immediate? m) (jmpli i) (jmplr i)))
			   ((mnemonic=? m 'call) (call i a))
			   ((mnemonic=? m 'rd) (rd i))
			   ((mnemonic=? m 'wr) (wr i (mnemonic:immediate? m)))
			   ((mnemonic:immediate? m) (rir i))
			   (else (rrr i))))))

  (larceny-register-name $r.tmp0 "%tmp0")
  (larceny-register-name $r.result "%result")
  (larceny-register-name $r.argreg2 "%argreg2")
  (larceny-register-name $r.argreg3 "%argreg3")
  (larceny-register-name $r.tmp1 "%tmp1")
  (larceny-register-name $r.tmp2 "%tmp2")
  (larceny-register-name $r.reg0 "%r0")
  (larceny-register-name $r.reg1 "%r1")
  (larceny-register-name $r.reg2 "%r2")
  (larceny-register-name $r.reg3 "%r3")
  (larceny-register-name $r.reg4 "%r4")
  (larceny-register-name $r.reg5 "%r5")
  (larceny-register-name $r.reg6 "%r6")
  (larceny-register-name $r.reg7 "%r7")
  (larceny-register-name $r.e-top "%etop")
  (larceny-register-name $r.e-limit "%elim")
  (larceny-register-name $r.timer "%timer")
  (larceny-register-name $r.millicode "%millicode")
  (larceny-register-name $r.globals "%globals")
  (larceny-register-name $r.stkp "%stkp")       ; note: after elim

  (millicode-name $m.alloc "alloc")
  (millicode-name $m.alloci "alloci")
  (millicode-name $m.gc "gc")
  (millicode-name $m.addtrans "addtrans")
  (millicode-name $m.stkoflow "stkoflow")
  (millicode-name $m.stkuflow "stkuflow")
  (millicode-name $m.creg "creg")
  (millicode-name $m.creg-set! "creg-set!")
  (millicode-name $m.add "+")
  (millicode-name $m.subtract "- (binary)")
  (millicode-name $m.multiply "*")
  (millicode-name $m.quotient "quotient")
  (millicode-name $m.remainder "remainder")
  (millicode-name $m.divide "/")
  (millicode-name $m.modulo "modulo")
  (millicode-name $m.negate "- (unary)")
  (millicode-name $m.numeq "=")
  (millicode-name $m.numlt "<")
  (millicode-name $m.numle "<=")
  (millicode-name $m.numgt ">")
  (millicode-name $m.numge ">=")
  (millicode-name $m.zerop "zero?")
  (millicode-name $m.complexp "complex?")
  (millicode-name $m.realp "real?")
  (millicode-name $m.rationalp "rational?")
  (millicode-name $m.integerp "integer?")
  (millicode-name $m.exactp "exact?")
  (millicode-name $m.inexactp "inexact?")
  (millicode-name $m.exact->inexact "exact->inexact")
  (millicode-name $m.inexact->exact "inexact->exact")
  (millicode-name $m.make-rectangular "make-rectangular")
  (millicode-name $m.real-part "real-part")
  (millicode-name $m.imag-part "imag-part")
  (millicode-name $m.sqrt "sqrt")
  (millicode-name $m.round "round")
  (millicode-name $m.truncate "truncate")
  (millicode-name $m.apply "apply")
  (millicode-name $m.varargs "varargs")
  (millicode-name $m.typetag "typetag")
  (millicode-name $m.typetag-set "typetag-set")
  (millicode-name $m.break "break")
  (millicode-name $m.eqv "eqv?")
  (millicode-name $m.partial-list->vector "partial-list->vector")
  (millicode-name $m.timer-exception "timer-exception")
  (millicode-name $m.exception "exception")
  (millicode-name $m.singlestep "singlestep")
  (millicode-name $m.syscall "syscall")
  (millicode-name $m.bvlcmp "bvlcmp")
  (millicode-name $m.enable-interrupts "enable-interrupts")
  (millicode-name $m.disable-interrupts "disable-interrupts")
  (millicode-name $m.alloc-bv "alloc-bv")
  (millicode-name $m.global-ex "global-exception")
  (millicode-name $m.invoke-ex "invoke-exception")
  (millicode-name $m.global-invoke-ex "global-invoke-exception")
  (millicode-name $m.argc-ex "argc-exception")

  (set! format-instruction format-instr)
  'format-instruction)


; eof


; ----------------------------------------------------------------------

(define (twobit-benchmark type . rest)
  (let* ((k (if (null? rest) 1 (car rest)))
         (name-prefix (if (= k 1) "" (number->string k)))
         (name-suffix (symbol->string type)))
    (run-benchmark 
     (string-append name-prefix "twobit" ":" name-suffix)
     k
     (lambda () 
       (case type
         ((long) 
          (compiler-switches 'fast-safe)
          (benchmark-block-mode #f)
          (compile-file "twobit-input-long.sch"))
         ((short) 
          (compiler-switches 'fast-safe)
          (benchmark-block-mode #t)
          (compile-file "twobit-input-short.sch"))
         (else
          (error "Benchmark type must be `long' or `short': " type))))
     (lambda (result)
       #t))))

; eof
