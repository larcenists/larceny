;;; - Ported from MIT Scheme runtime by Brian D. Carlstrom.
;;; - Massively rehacked & extended by Olin Shivers 6/98.
;;; - Massively redesigned and rehacked 5/2000 during SRFI process.
;;; - Rewritten for Unicode, bytevectors, and R7RS by Will Clinger in 2015.
;;; At this point, the code bears the following relationship to the
;;; MIT Scheme code: "This is my grandfather's axe. My father replaced
;;; the head, and I have replaced the handle." Nonetheless, we preserve
;;; the MIT Scheme copyright:
;;;     Copyright (c) 1988-1995 Massachusetts Institute of Technology
;;; The MIT Scheme license is a "free software" license. See the end of
;;; this file for the tedious details. 

;;; Larceny

; Larceny porting notes:
;  - extracted source code to be included within R7RS .sld file
;  - rewrote LET-OPTIONALS* and :OPTIONAL
;  - made a check-arg macro that removes the test, since this code
;    will generally be compiled "safe"
;  - changed %latin1->char and %char->latin1 to integer->char, char->integer
;  - rewrote to use bytevectors and to support full Unicode
;
; Character sets are represented as records containing a bytevector
; of length >= 16, with one bit per character.  Characters whose
; Unicode encoding is greater than or equal to 8 times the length
; of the bytevector are not in the character set.
;
; A character set containing every Unicode character occupies
; a little over 139264 bytes of memory.  A character set with
; every character in the Basic Multilingual Plane (BMP) occupies
; a little over 8192 bytes of memory.
;
; Character sets are immutable, as are the bitvectors placed within
; them.
;
; Additional invariant: the bitvector is as short as possible,
; so it is of minimum size or its last byte is nonzero.

; Returns an equivalent bitvector of minimum size.

(define (bitvector-minimized bitv)
  (let ((n (bytevector-length bitv)))

    (define (loop n)
      (cond ((fx=? n (%char-set:minsize-in-bytes))
             (bytevector-copy bitv 0 n))
            ((fx<? 0 (bytevector-ref bitv (fx- n 1)))
             (bytevector-copy bitv 0 n))
            (else
             (loop (fx- n 1)))))             

    (cond ((fx=? n (%char-set:minsize-in-bytes))
           bitv)
          ((fx<? 0 (bytevector-ref bitv (fx- n 1)))
           bitv)
          (else
           (loop n)))))

(define (make-bitvector n0 init)
  (let* ((n (div (+ n0 7) 8))
         (n8 (* 8 n))
         (bitv (make-bytevector n
                                (case init
                                 ((0) 0)
                                 ((1) 255)
                                 (else
                                  (error "make-bitvector: bad init" init))))))
    (do ((i n0 (+ i 1)))
        ((>= i n8))
      (bitvector-set! bitv i 0))
    bitv))

(define (bitvector-length bitv)
  (* 8 (bytevector-length bitv)))

(define (bitvector-ref bitv i)
  (let* ((j (div i 8))
         (k (fxand i 7))
         (mask (case k
                ((0) 1)
                ((1) 2)
                ((2) 4)
                ((3) 8)
                ((4) 16)
                ((5) 32)
                ((6) 64)
                ((7) 128))))
    (if (fx=? 0 (fxand (bytevector-ref bitv j) mask))
        0
        1)))

; This mutator can be applied only to bitvectors that have not yet
; been encapsulated within a character set.

(define (bitvector-set! bitv i b)
  (let* ((j (div i 8))
         (k (fxand i 7))
         (mask (case k
                ((0) 1)
                ((1) 2)
                ((2) 4)
                ((3) 8)
                ((4) 16)
                ((5) 32)
                ((6) 64)
                ((7) 128)))
         (bitvalue (* b mask)))
    (bytevector-set! bitv
                     j
                     (fxior bitvalue
                            (fxand (bytevector-ref bitv j) (fxnot mask))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (error msg . irritants)
  (apply assertion-violation #f msg irritants))

(define-record-type :char-set
  (make-char-set bitv)
  char-set?
  (bitv char-set:bitv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %char-set:empty
  (make-char-set (make-bitvector (%char-set:minsize) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax check-arg
  (syntax-rules ()
   ((check-arg pred val caller) val)))

;;; End Larceny

;;; Exports:
;;; char-set? char-set= char-set<=
;;; char-set-hash 
;;; char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
;;; char-set-fold char-set-unfold char-set-unfold!
;;; char-set-for-each char-set-map
;;; char-set-copy char-set
;;;
;;; list->char-set  string->char-set 
;;; list->char-set! string->char-set! 
;;;
;;; filterchar-set  ucs-range->char-set  ->char-set
;;; filterchar-set! ucs-range->char-set!
;;;
;;; char-set->list char-set->string
;;;
;;; char-set-size char-set-count char-set-contains?
;;; char-set-every char-set-any
;;;
;;; char-set-adjoin  char-set-delete 
;;; char-set-adjoin! char-set-delete!
;;; 

;;; char-set-complement  char-set-union  char-set-intersection  
;;; char-set-complement! char-set-union! char-set-intersection! 
;;;
;;; char-set-difference  char-set-xor  char-set-diff+intersection
;;; char-set-difference! char-set-xor! char-set-diff+intersection!
;;;
;;; char-set:lower-case                char-set:upper-case        char-set:title-case
;;; char-set:letter                char-set:digit                char-set:letter+digit
;;; char-set:graphic                char-set:printing        char-set:whitespace
;;; char-set:iso-control        char-set:punctuation        char-set:symbol
;;; char-set:hex-digit                char-set:blank                char-set:ascii
;;; char-set:empty                char-set:full

;;; Imports
;;; This code has the following non-R5RS dependencies:
;;; - ERROR
;;; - %LATIN1->CHAR %CHAR->LATIN1
;;; - LET-OPTIONALS* and :OPTIONAL macros for parsing, checking & defaulting
;;;   optional arguments from rest lists.
;;; - BITWISE-AND for CHAR-SET-HASH
;;; - The SRFI-9 DEFINE-RECORD-TYPE record macro
;;; - A simple CHECK-ARG procedure: 
;;;   (lambda (pred val caller) (if (not (pred val)) (error val caller)))

;;; This is simple code, not great code. Char sets are represented as 256-char
;;; strings. If char I is ASCII/Latin-1 0, then it isn't in the set; if char I
;;; is ASCII/Latin-1 1, then it is in the set.
;;; - Should be rewritten to use bit strings or byte vecs.
;;; - Is Latin-1 specific. Would certainly have to be rewritten for Unicode.

;;; See the end of the file for porting and performance-tuning notes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Parse, type-check & default a final optional BASE-CS parameter from
;;; a rest argument. Return a *fresh copy* of the underlying string.
;;; The default is the empty set. The PROC argument is to help us
;;; generate informative error exceptions.

(define (%default-base maybe-base proc)
  (if (pair? maybe-base)
      (let ((bcs  (car maybe-base))
            (tail (cdr maybe-base)))
        (if (null? tail)
            (if (char-set? bcs)
                bcs
                (error "BASE-CS parameter not a char-set" proc bcs))
            (error "Expected final base char set -- too many parameters"
                   proc maybe-base)))
      %char-set:empty))

;;; If CS is really a char-set, return its encapsulated bitvector.
;;; Otherwise report an error msg on
;;; behalf of our caller, PROC. This procedure exists basically to provide
;;; explicit error-checking & reporting.

(define (%char-set:bitv/check cs proc)
  (let lp ((cs cs))
    (if (char-set? cs) (char-set:bitv cs)
        (lp (error "Not a char-set" cs proc)))))



;;; These internal functions hide a lot of the dependency on the
;;; underlying representation of char sets. They should be
;;; inlined if possible.

(define (si=0? bitv i)
  (cond ((fx<? i (%char-set:minsize))
         (fx=? 0 (bitvector-ref bitv i)))
        ((fx<? i (* 8 (bytevector-length bitv)))
         (fx=? 0 (bitvector-ref bitv i)))
        (else
         #t)))

(define (si=1? bitv i)
  (cond ((fx<? i (%char-set:minsize))
         (fx=? 1 (bitvector-ref bitv i)))
        ((fx<? i (* 8 (bytevector-length bitv)))
         (fx=? 1 (bitvector-ref bitv i)))
        (else
         #f)))

;; FIXME: no longer used

#;
(define (si bitv i)
  (cond ((fx<? i (%char-set:minsize))
         (bitvector-ref bitv i))
        ((fx<? i (* 8 (bytevector-length bitv)))
         (bitvector-ref bitv i))
        (else
         0)))

(define (%set0! s i) (bitvector-set! s i 0))
(define (%set1! s i) (bitvector-set! s i 1))

;;; Exported procedures.

(define (char-set-copy cs) cs)

(define (char-set= . rest)
  (or (null? rest)
      (let* ((cs1  (car rest))
             (rest (cdr rest))
             (bitv1 (%char-set:bitv/check cs1 char-set=)))
        (let lp ((rest rest))
          (or (not (pair? rest))
              (eq? cs1 (car rest))
              (and (let* ((bitv2 (%char-set:bitv/check (car rest) char-set=))
                          (n1 (bytevector-length bitv1))
                          (n2 (bytevector-length bitv2)))
                     (and (fx=? n1 n2)
                          (let loop ((i 0))
                            (cond ((fx=? i n1)
                                   #t)
                                  ((fx=? (bytevector-ref bitv1 i)
                                         (bytevector-ref bitv2 i))
                                   (loop (fx+ i 1)))
                                  (else
                                   #f)))))
                   (lp (cdr rest))))))))

(define (char-set<= . rest)
  (or (null? rest)
      (let ((cs1  (car rest))
            (rest (cdr rest)))
        (let lp ((bitv1 (%char-set:bitv/check cs1 char-set<=))
                 (rest rest))
          (or (not (pair? rest))
              (let ((bitv2 (%char-set:bitv/check (car rest) char-set<=))
                    (rest (cdr rest)))
                (if (eq? bitv1 bitv2)
                    (lp bitv2 rest)                        ; Fast path
                    (let* ((n1 (bytevector-length bitv1))
                           (n2 (bytevector-length bitv2))) ; Real test
                      (and (fx<=? n1 n2)
                           (let loop ((i 0))
                             (cond ((fx=? i n1)
                                    #t)
                                   ((fx=? (fxior (bytevector-ref bitv1 i)
                                                 (bytevector-ref bitv2 i))
                                          (bytevector-ref bitv2 i))
                                    (loop (fx+ i 1)))
                                   (else
                                    #f)))
                           (lp bitv2 rest))))))))))

;;; Hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compute (c + 37 c + 37^2 c + ...) modulo BOUND, with sleaze thrown in
;;; to keep the intermediate values small. (We do the calculation with just
;;; enough bits to represent BOUND, masking off high bits at each step in
;;; calculation. If this screws up any important properties of the hash
;;; function I'd like to hear about it. -Olin)
;;;
;;; If you keep BOUND small enough, the intermediate calculations will 
;;; always be fixnums. How small is dependent on the underlying Scheme system; 
;;; we use a default BOUND of 2^22 = 4194304, which should hack it in
;;; Schemes that give you at least 29 signed bits for fixnums. The core 
;;; calculation that you don't want to overflow is, worst case,
;;;     (+ 65535 (* 37 (- bound 1)))
;;; where 65535 is the max character code. Choose the default BOUND to be the
;;; biggest power of two that won't cause this expression to fixnum overflow, 
;;; and everything will be copacetic.

(define (char-set-hash cs . maybe-bound)
  (let* ((bound (if (and (pair? maybe-bound)
                         ((lambda (n)
                            (and (integer? n) ; original code was wrong here
                                 (exact? n)
                                 (<= 0 n)))
                          (car maybe-bound)))
                    (car maybe-bound)
                    4194304))
         (bound (if (zero? bound) 4194304 bound))        ; 0 means default.
         (bitv (%char-set:bitv/check cs char-set-hash))
         (n (bytevector-length bitv))
         ;; Compute a 111...1 mask that will cover BOUND-1:
         (mask (let lp ((i #x10000)) ; Let's skip first 16 iterations, eh?
                 (if (>= i bound) (- i 1) (lp (+ i i))))))

    (let lp ((i 0) (ans 0))
      (if (fx<? i n)
          (lp (+ i 1)
              (bitwise-and mask
                           (+ (* 37 ans) (* i (bytevector-ref bitv i)))))
          (mod ans bound)))))

(define (char-set-contains? cs char)
  (check-arg char? char char-set-contains?)
  (si=1? (%char-set:bitv/check cs char-set-contains?)
         (char->integer char)))

(define (char-set-size cs)
  (let* ((bitv (%char-set:bitv/check cs char-set-size))
         (n (bytevector-length bitv)))
    (let lp ((i 0) (size 0))
      (if (fx=? i n)
          size
          (lp (fx+ i 1)
              (fx+ size (fxbit-count (bytevector-ref bitv i))))))))

(define (char-set-count pred cset)
  (check-arg procedure? pred char-set-count)
  (let* ((bitv (%char-set:bitv/check cset char-set-count))
         (n (* 8 (bytevector-length bitv))))
    (let lp ((i 0) (count 0))
      (cond ((fx=? i n)
             count)
            ((and (si=1? bitv i)
                  (pred (integer->char i)))
             (lp (fx+ i 1) (fx+ count 1)))
            (else
             (lp (fx+ i 1) count))))))

;;; -- Adjoin & delete

(define (char-set-adjoin cs . chars)
  (char-set-union cs (list->char-set chars)))
(define (char-set-adjoin! cs . chars)
  (char-set-union cs (list->char-set chars)))
(define (char-set-delete cs . chars)
  (char-set-difference cs (list->char-set chars)))
(define (char-set-delete! cs . chars)
  (char-set-difference cs (list->char-set chars)))


;;; Cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple implementation. A cursors is an integer index into the
;;; mark vector, and -1 for the end-of-char-set cursor.
;;;
;;; If we represented char sets as a bit set, we could do the following
;;; trick to pick the lowest bit out of the set: 
;;;   (count-bits (xor (- cset 1) cset))
;;; (But first mask out the bits already scanned by the cursor first.)

(define (char-set-cursor cset)
  (%char-set-cursor-next cset -1 char-set-cursor))
  
(define (end-of-char-set? cursor) (< cursor 0))

(define (char-set-ref cset cursor) (integer->char cursor))

(define (char-set-cursor-next cset cursor)
  (check-arg (lambda (i) (and (integer? i) (exact? i) (<= 0 i)))
             cursor
             char-set-cursor-next)
  (%char-set-cursor-next cset cursor char-set-cursor-next))

(define (%char-set-cursor-next cset cursor proc)        ; Internal
  (let* ((bitv (%char-set:bitv/check cset proc))
         (n (bytevector-length bitv)))

    ;; i is a bytevector index

    (define (fast-loop i)
      (cond ((fx=? i n) -1)
            ((fx<? 0 (bytevector-ref bitv i))
             (slow-loop (bytevector-ref bitv i) i 1 (* 8 i)))
            (else
             (fast-loop (fx+ i 1)))))

    ;; i is a bytevector index, j is a power of two <= 256,
    ;; and k = 8*i+(lg j) is a bitvector index.

    (define (slow-loop byte i j k)
      (cond ((fx=? j 256)
             (fast-loop (fx+ i 1)))
            ((fx=? 0 (fxand byte j))
             (slow-loop byte i (fx+ j j) (fx+ k 1)))
            (else
             k)))

    (if (< cursor 0)
        (fast-loop 0)
        (let* ((i (div cursor 8))
               (m (mod cursor 8))
               (j (expt 2 m)))
          (slow-loop (bytevector-ref bitv i)
                     i (+ j j) (+ cursor 1))))))


;;; -- for-each map fold unfold every any

(define (char-set-for-each proc cs)
  (check-arg procedure? proc char-set-for-each)
  (let lp ((cursor (char-set-cursor cs)))
    (if (not (end-of-char-set? cursor))
        (begin (proc (char-set-ref cs cursor))
               (lp (char-set-cursor-next cs cursor))))))

(define (char-set-map proc cs)
  (check-arg procedure? proc char-set-map)
  (let lp ((cursor (char-set-cursor cs))
           (chars '()))
    (if (end-of-char-set? cursor)
        (list->char-set chars)
        (lp (char-set-cursor-next cs cursor)
            (cons (proc (char-set-ref cs cursor)) chars)))))

(define (char-set-fold kons knil cs)
  (check-arg procedure? kons char-set-fold)
  (let lp ((cursor (char-set-cursor cs))
           (ans knil))
    (if (end-of-char-set? cursor)
        ans
        (lp (char-set-cursor-next cs cursor)
            (kons (char-set-ref cs cursor) ans)))))

(define (char-set-every pred cs)
  (check-arg procedure? pred char-set-every)
  (let lp ((cursor (char-set-cursor cs)))
    (if (end-of-char-set? cursor)
        #t
        (and (pred (char-set-ref cs cursor))
             (lp (char-set-cursor-next cs cursor))))))

(define (char-set-any pred cs)
  (check-arg procedure? pred char-set-any)
  (let lp ((cursor (char-set-cursor cs)))
    (if (end-of-char-set? cursor)
        #f
        (or (pred (char-set-ref cs cursor))
            (lp (char-set-cursor-next cs cursor))))))


;;; SRFI 14 has the order of these arguments all screwed up,
;;; but the examples and reference implementation appear to
;;; use this order of arguments.

(define (%char-set-unfold caller p f g seed base-cs)
  (check-arg procedure? p caller)
  (check-arg procedure? f caller)
  (check-arg procedure? g caller)
  (let lp ((seed seed)
           (chars (char-set->list base-cs)))
    (if (p seed)
        (list->char-set chars)                      ; P says we are done.
        (lp (g seed)                                ; Loop on (G SEED).
            (cons (f seed) chars)))))               ; Add (F SEED) to set.

(define (char-set-unfold p f g seed . maybe-base)
  (let ((bs (%default-base maybe-base char-set-unfold)))
    (%char-set-unfold char-set-unfold p f g seed bs)))

(define (char-set-unfold! p f g seed base-cs)
  (%char-set-unfold char-set-unfold! p f g seed base-cs))



;;; list <--> char-set

(define (%list->char-set chars base)
  (let* ((chars (list-sort char>? chars))
         (bitv0 (%char-set:bitv/check base list->char-set))
         (n (bytevector-length bitv0)))
    (cond ((null? chars)
           base)
          ((fx<? (char->integer (car chars)) (* 8 n))
           (let ((bitv (bytevector-copy bitv0)))
             (for-each (lambda (char) (%set1! bitv (char->integer char)))
                       chars)
             (make-char-set bitv)))
          (else
           (let* ((n (+ 1 (div (+ 7 (char->integer (car chars))) 8)))
                  (bitv (make-bytevector n 0)))
             (bytevector-copy! bitv 0 bitv0 0 (bytevector-length bitv0))
             (for-each (lambda (char) (%set1! bitv (char->integer char)))
                       chars)
             (make-char-set bitv))))))

(define (char-set . chars)
  (%list->char-set chars %char-set:empty))

(define (list->char-set chars . maybe-base)
  (let ((bs (%default-base maybe-base list->char-set)))
    (%list->char-set chars bs)))

(define (list->char-set! chars base-cs)
  (%list->char-set chars base-cs))


(define (char-set->list cs)
  (let lp ((cursor (char-set-cursor cs))
           (chars '()))
    (if (end-of-char-set? cursor)
        (reverse chars)
        (lp (char-set-cursor-next cs cursor)
            (cons (char-set-ref cs cursor) chars)))))


;;; string <--> char-set

(define (%string->char-set str bs proc)
  (check-arg string? str proc)
  (list->char-set (string->list str) bs))

(define (string->char-set str . maybe-base)
  (let ((bs (%default-base maybe-base string->char-set)))
    (%string->char-set str bs string->char-set)))

(define (string->char-set! str base-cs)
  (%string->char-set str base-cs))


(define (char-set->string cs)
  (list->string (char-set->list cs)))


;;; -- UCS-range -> char-set

(define (%ucs-range->char-set lower upper error? bs proc)
  (check-arg (lambda (x) (and (integer? x) (exact? x) (<= 0 x)))
             lower
             proc)
  (check-arg (lambda (x) (and (integer? x) (exact? x) (<= lower x)))
             upper
             proc)
  (cond ((<= %unicode:limit lower)
         %char-set:empty)
        ((< %unicode:limit upper)
         (%ucs-range->char-set lower %unicode:limit error? bs proc))
        ((<= %excluded:min lower %excluded:max)
         (%ucs-range->char-set (+ 1 %excluded:max)
                               (max upper (+ 1 %excluded:max))
                               error? bs proc))
        ((<= %excluded:min upper %excluded:max)
         (%ucs-range->char-set (min lower (- %excluded:min 1))
                               (- %excluded:min 1)
                               error? bs proc))
        ((< (%char-set:maxsize) upper)
         (if error?
             (error (string-append
                     "this instance of SRFI 14 is limited to "
                     "Unicode characters below U+"
                     (number->string (%char-set:maxsize) 16)
                     "\nFor character sets over full Unicode, "
                     "import (srfi 14 unicode).")
                    proc lower upper))
         (%ucs-range->char-set (min lower (%char-set:maxsize))
                              (%char-set:maxsize)
                              error? bs proc))
        ((= lower upper)
         bs)
        (else
         (let* ((bitv0 (%char-set:bitv/check bs proc))
                (n0 (bytevector-length bitv0))
                (n (max n0 (div (+ upper 7) 8)))
                (bitv (if (= n n0)
                          (bytevector-copy bitv0)
                          (let ((bitv (make-bytevector n 0)))
                            (bytevector-copy! bitv 0 bitv0)
                            bitv)))
                (lower/8 (div lower 8))
                (upper/8 (div upper 8)))
           (let loop ((i lower))
             (cond ((>= i upper) #t)
                   ((>= i (+ lower 8)) #t)
                   ((<= %excluded:min i %excluded:max) #t)
                   (else
                    (bitvector-set! bitv i 1)
                    (loop (+ i 1)))))
           (let loop ((i (max lower (- upper 8))))
             (cond ((>= i upper) #t)
                   ((<= %excluded:min i %excluded:max) #t)
                   (else
                    (bitvector-set! bitv i 1)
                    (loop (+ i 1)))))
           (let loop ((i (+ lower/8 1)))
             (cond ((fx>=? i upper/8) #t)
                   ((<= %excluded:min/8 i %excluded:max/8)
                    (loop (+ 1 %excluded:max/8)))
                   (else
                    (bytevector-set! bitv i 255)
                    (loop (+ i 1)))))
           (make-char-set bitv)))))

(define (ucs-range->char-set lower upper . rest)
  (let ((error? (if (null? rest) #f (car rest)))
        (bs (%default-base (if (pair? rest) (cdr rest) '())
                           ucs-range->char-set)))
    (%ucs-range->char-set lower upper error? bs ucs-range->char-set)))

(define (ucs-range->char-set! lower upper error? base-cs)
 (%ucs-range->char-set lower upper error? base-cs ucs-range->char-set!))


;;; -- predicate -> char-set

(define (%char-set-filter pred cs base proc)
  (check-arg procedure? pred proc)
  (let* ((bitv0 (%char-set:bitv/check base proc))
         (bitv1 (%char-set:bitv/check cs proc))
         (n0 (bytevector-length bitv0))
         (n1 (bytevector-length bitv1))
         (n (max n0 n1))
         (bitv (if (>= n0 n1)
                   (bytevector-copy bitv0)
                   (let ((bitv (make-bytevector n 0)))
                     (bytevector-copy! bitv 0 bitv0)
                     bitv))))
    (char-set-for-each (lambda (c)
                         (if (pred c)
                             (bitvector-set! bitv (char->integer c) 1)))
                       cs)
    (make-char-set (bitvector-minimized bitv))))

(define (char-set-filter predicate domain . maybe-base)
  (let ((bs (%default-base maybe-base char-set-filter)))
    (%char-set-filter predicate
                      domain
                      bs
                      char-set-filter)))

(define (char-set-filter! predicate domain base-cs)
  (%char-set-filter predicate
                    domain
                    base-cs
                    char-set-filter!))


;;; {string, char, char-set, char predicate} -> char-set

(define (->char-set x)
  (cond ((char-set? x) x)
        ((string? x) (string->char-set x))
        ((char? x) (char-set x))
        (else (error "->char-set: Not a charset, string or char." x))))



;;; Set algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The exported ! procs are "linear update" -- allowed, but not required, to
;;; side-effect their first argument when computing their result. In other
;;; words, you must use them as if they were completely functional, just like
;;; their non-! counterparts, and you must additionally ensure that their
;;; first arguments are "dead" at the point of call. In return, we promise a
;;; more efficient result, plus allowing you to always assume char-sets are
;;; unchangeable values.
;;;
;;; In Larceny's implementation, the "linear update" versions are identical
;;; to the purely functional versions.

;;; -- Complement

(define (char-set-complement cs)
  (issue-warning-deprecated 'char-set-complement)
  (char-set-difference char-set:full cs))

(define (char-set-complement! cset)
  (issue-warning-deprecated 'char-set-complement!)
  (char-set-difference char-set:full cset))


;;; -- Union

(define (char-set-union! cset1 . csets)
  (apply char-set-union cset1 csets))

(define (char-set-union . csets)
  (cond ((null? csets)
         %char-set:empty)
        ((null? (cdr csets))
         (car csets))
        (else
         (let* ((bitvectors (map (lambda (cs)
                                   (%char-set:bitv/check cs char-set-union))
                                 csets))
                (lengths (map bytevector-length bitvectors))
                (n (apply max lengths))
                (bitv (make-bytevector n 0)))
           (for-each (lambda (bv0)
                       (let ((n0 (bytevector-length bv0)))
                         (do ((i 0 (+ i 1)))
                             ((= i n0))
                           (bytevector-set! bitv
                                            i
                                            (fxior (bytevector-ref bitv i)
                                                   (bytevector-ref bv0 i))))))
                     bitvectors)
           (make-char-set bitv)))))


;;; -- Intersection

(define (char-set-intersection! cset1 . csets)
  (apply char-set-intersection cset1 csets))

(define (char-set-intersection . csets)
  (cond ((null? csets)
         char-set:full)
        ((null? (cdr csets))
         (car csets))
        (else
         (let* ((bitvectors (map (lambda (cs)
                                   (%char-set:bitv/check
                                    cs char-set-intersection))
                                 csets))
                (lengths (map bytevector-length bitvectors))
                (n (apply min lengths))
                (bitv (make-bytevector n 255)))
           (for-each (lambda (bv0)
                       (let ((n0 (bytevector-length bv0)))
                         (do ((i 0 (+ i 1)))
                             ((= i n))
                           (bytevector-set! bitv
                                            i
                                            (fxand (bytevector-ref bitv i)
                                                   (bytevector-ref bv0 i))))))
                     bitvectors)
           (make-char-set (bitvector-minimized bitv))))))


;;; -- Difference

(define (char-set-difference! cset1 . csets)
  (apply char-set-difference cset1 csets))

(define (char-set-difference cs1 . csets)
  (let* ((bitvectors (map (lambda (cs)
                            (%char-set:bitv/check cs char-set-difference))
                          csets))
         (lengths (map bytevector-length bitvectors))
         (bitv (bytevector-copy
                (%char-set:bitv/check cs1 char-set-difference)))
         (n (bytevector-length bitv)))
    (for-each (lambda (bv0)
                (let* ((n0 (bytevector-length bv0))
                       (n0 (min n0 n)))
                  (do ((i 0 (+ i 1)))
                      ((= i n0))
                    (bytevector-set! bitv
                                     i
                                     (fxand (bytevector-ref bitv i)
                                            (fxnot (bytevector-ref bv0 i)))))))
              bitvectors)
    (make-char-set (bitvector-minimized bitv))))


;;; -- Xor

(define (char-set-xor! cset1 . csets)
  (apply char-set-xor cset1 csets))

(define (char-set-xor . csets)
  (cond ((null? csets)
         %char-set:empty)
        ((null? (cdr csets))
         (car csets))
        (else
         (let* ((bitvectors (map (lambda (cs)
                                   (%char-set:bitv/check cs char-set-xor))
                                 csets))
                (lengths (map bytevector-length bitvectors))
                (n (apply max lengths))
                (bitv (make-bytevector n 0)))
           (for-each (lambda (bv0)
                       (let ((n0 (bytevector-length bv0)))
                         (do ((i 0 (+ i 1)))
                             ((= i n0))
                           (bytevector-set! bitv
                                            i
                                            (fxxor (bytevector-ref bitv i)
                                                   (bytevector-ref bv0 i))))))
                     bitvectors)
           (make-char-set bitv)))))


;;; -- Difference & intersection

;;; More likely to be correct this way.

(define (char-set-diff+intersection! cs1 cs2 . csets)
  (apply char-set-diff+intersection cs1 cs2 csets))

(define (char-set-diff+intersection cs1 . csets)
  (values (apply char-set-difference cs1 csets)
          (apply char-set-intersection cs1 csets)))


;;;; System character sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These definitions are for Unicode.  Ouch.

(define char-set:empty %char-set:empty)

(define char-set:full
  (ucs-range->char-set 0 (%char-set:maxsize)))

(define char-set:lower-case
  (char-set-filter char-lower-case?
                   char-set:full))

(define char-set:upper-case
  (char-set-filter char-upper-case?
                   char-set:full))

(define char-set:title-case
  (char-set-filter char-title-case?
                   char-set:full))

(define char-set:letter
  (char-set-filter (lambda (c)
                     (memq (char-general-category c) '(Lu Ll Lt Lm Lo)))
                   char-set:full))

(define char-set:digit
  (char-set-filter (lambda (c)
                     (eq? (char-general-category c) 'Nd))
                   char-set:full))

(define char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))

(define char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))

(define char-set:punctuation
  (char-set-filter (lambda (c)
                     (memq (char-general-category c) '(Pc Pd Ps Pe Pi Pf Po)))
                   char-set:full))

(define char-set:symbol
  (char-set-filter (lambda (c)
                     (memq (char-general-category c) '(Sm Sc Sk So)))
                   char-set:full))

;;; FIXME: this may not be right for Unicode.  

(define char-set:graphic
  (char-set-union char-set:letter+digit char-set:punctuation char-set:symbol))

(define char-set:whitespace
  (char-set-union
   (char-set-filter (lambda (c)
                      (memq (char-general-category c) '(Zs Zl Zp)))
                    char-set:full)
   (list->char-set (map integer->char '(#x09 ; HORIZONTAL TABULATION
                                        #x0A ; LINE FEED
                                        #x0B ; VERTICAL TABULATION
                                        #x0C ; FORM FEED
                                        #x0D ; CARRIAGE RETURN
                                        #x20 ; SPACE
                                        #xA0)))))

(define char-set:printing
  (char-set-union char-set:whitespace char-set:graphic)) ; NO-BREAK SPACE

(define char-set:blank
  (char-set-union
   (char-set-filter (lambda (c) (eq? (char-general-category c) 'Zs))
                    char-set:full)
   (char-set #\tab)))

(define char-set:iso-control
  (ucs-range->char-set! #x7F #xA0 #t (ucs-range->char-set 0 32)))

(define char-set:ascii (ucs-range->char-set 0 128))


;;; Porting & performance-tuning notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See the section at the beginning of this file on external dependencies.
;;;
;;; First and foremost, rewrite this code to use bit vectors of some sort.
;;; This will give big speedup and memory savings.
;;;
;;; - LET-OPTIONALS* macro.
;;; This is only used once. You can rewrite the use, port the hairy macro
;;; definition (which is implemented using a Clinger-Rees low-level
;;; explicit-renaming macro system), or port the simple, high-level
;;; definition, which is less efficient.
;;;
;;; - :OPTIONAL macro
;;; Very simply defined using an R5RS high-level macro.
;;;
;;; Implementations that can arrange for the base char sets to be immutable
;;; should do so. (E.g., Scheme 48 allows one to mark a string as immutable,
;;; which can be used to protect the underlying strings.) It would be very,
;;; very bad if a client's buggy code corrupted these constants.
;;;
;;; There is a fair amount of argument checking. This is, strictly speaking,
;;; unnecessary -- the actual body of the procedures will blow up if an
;;; illegal value is passed in. However, the error message will not be as good
;;; as if the error were caught at the "higher level." Also, a very, very
;;; smart Scheme compiler may be able to exploit having the type checks done
;;; early, so that the actual body of the procedures can assume proper values.
;;; This isn't likely; this kind of compiler technology isn't common any
;;; longer.
;;; 
;;; The overhead of optional-argument parsing is irritating. The optional
;;; arguments must be consed into a rest list on entry, and then parsed out.
;;; Function call should be a matter of a few register moves and a jump; it
;;; should not involve heap allocation! Your Scheme system may have a superior
;;; non-R5RS optional-argument system that can eliminate this overhead. If so,
;;; then this is a prime candidate for optimising these procedures,
;;; *especially* the many optional BASE-CS parameters.
;;;
;;; Note that optional arguments are also a barrier to procedure integration.
;;; If your Scheme system permits you to specify alternate entry points
;;; for a call when the number of optional arguments is known in a manner
;;; that enables inlining/integration, this can provide performance 
;;; improvements.
;;;
;;; There is enough *explicit* error checking that *all* internal operations
;;; should *never* produce a type or index-range error. Period. Feel like
;;; living dangerously? *Big* performance win to be had by replacing string
;;; and record-field accessors and setters with unsafe equivalents in the
;;; code. Similarly, fixnum-specific operators can speed up the arithmetic
;;; done on the index values in the inner loops. The only arguments that are
;;; not completely error checked are
;;;   - string lists (complete checking requires time proportional to the
;;;     length of the list)
;;;   - procedure arguments, such as char->char maps & predicates.
;;;     There is no way to check the range & domain of procedures in Scheme.
;;; Procedures that take these parameters cannot fully check their
;;; arguments. But all other types to all other procedures are fully
;;; checked.
;;;
;;; This does open up the alternate possibility of simply *removing* these 
;;; checks, and letting the safe primitives raise the errors. On a dumb
;;; Scheme system, this would provide speed (by eliminating the redundant
;;; error checks) at the cost of error-message clarity.
;;;
;;; In an interpreted Scheme, some of these procedures, or the internal
;;; routines with % prefixes, are excellent candidates for being rewritten
;;; in C.
;;;
;;; It would also be nice to have the ability to mark some of these
;;; routines as candidates for inlining/integration.
;;; 
;;; See the comments preceding the hash function code for notes on tuning
;;; the default bound so that the code never overflows your implementation's
;;; fixnum size into bignum calculation.
;;;
;;; All the %-prefixed routines in this source code are written
;;; to be called internally to this library. They do *not* perform
;;; friendly error checks on the inputs; they assume everything is
;;; proper. They also do not take optional arguments. These two properties
;;; save calling overhead and enable procedure integration -- but they
;;; are not appropriate for exported routines.

;;; Copyright notice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1988-1995 Massachusetts Institute of Technology
;;; 
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy and modify this software, to
;;; redistribute either the original software or a modified version, and
;;; to use this software for any purpose is granted, subject to the
;;; following restrictions and understandings.
;;; 
;;; 1. Any copy made of this software must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this software.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that the operation of
;;; this software will be error-free, and MIT is under no obligation to
;;; provide any services, by way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case.

;eof
