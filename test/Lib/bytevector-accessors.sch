; Test cases for the 16-, 32- and 64-bit bytevector accessors
; (bytevector-u32-ref, bytevector-s64-native-set!, ...).
;
; The compiler open-codes them (bytevector-accessor-rules in
; src/Compiler/common.imp.sch, and on 64-bit Fence targets whole-word
; loads and stores), so each is tested as an inlined call with a
; literal endianness, with an endianness known only at run time, and
; as a native accessor, and also through apply (the out-of-line
; procedure).  Values are checked against byte reads and exact
; arithmetic; out-of-range indices, misaligned native accesses, values
; that do not fit, and arguments of the wrong type must be errors that
; leave the bytevector as it was.

; Each entry: (size readers writers), the readers and writers being
; (name procedure order native?) for unsigned and for signed values.

(define-syntax bva:accessors
  (syntax-rules ()
    ((_ (size uref sref uset sset unref snref unset snset) ...)
     (list
      (list size
            (list (bva:readers uref unref) (bva:readers sref snref))
            (list (bva:writers uset unset) (bva:writers sset snset)))
      ...))))

(define-syntax bva:readers
  (syntax-rules ()
    ((_ ref nref)
     (list (list 'big (lambda (b i) (ref b i 'big)) 'big #f)
           (list 'little (lambda (b i) (ref b i 'little)) 'little #f)
           (list 'run-time-big
                 (let ((f (lambda (b i w) (ref b i w))))
                   (lambda (b i) (f b i (car (list 'big)))))
                 'big #f)
           (list 'run-time-little
                 (let ((f (lambda (b i w) (ref b i w))))
                   (lambda (b i) (f b i (car (list 'little)))))
                 'little #f)
           (list 'run-time-other      ; any other symbol is native
                 (let ((f (lambda (b i w) (ref b i w))))
                   (lambda (b i) (f b i (car (list 'whatever)))))
                 (native-endianness) #f)
           (list 'native (lambda (b i) (nref b i)) (native-endianness) #t)
           (list 'out-of-line-big
                 (lambda (b i) (apply ref (list b i 'big))) 'big #f)
           (list 'out-of-line-little
                 (lambda (b i) (apply ref (list b i 'little))) 'little #f)
           (list 'out-of-line-native
                 (lambda (b i) (apply nref (list b i)))
                 (native-endianness) #t)))))

(define-syntax bva:writers
  (syntax-rules ()
    ((_ set nset)
     (list (list 'big (lambda (b i n) (set b i n 'big)) 'big #f)
           (list 'little (lambda (b i n) (set b i n 'little)) 'little #f)
           (list 'run-time-big
                 (let ((f (lambda (b i n w) (set b i n w))))
                   (lambda (b i n) (f b i n (car (list 'big)))))
                 'big #f)
           (list 'run-time-little
                 (let ((f (lambda (b i n w) (set b i n w))))
                   (lambda (b i n) (f b i n (car (list 'little)))))
                 'little #f)
           (list 'run-time-other
                 (let ((f (lambda (b i n w) (set b i n w))))
                   (lambda (b i n) (f b i n (car (list 'whatever)))))
                 (native-endianness) #f)
           (list 'native (lambda (b i n) (nset b i n)) (native-endianness) #t)
           (list 'out-of-line-big
                 (lambda (b i n) (apply set (list b i n 'big))) 'big #f)
           (list 'out-of-line-little
                 (lambda (b i n) (apply set (list b i n 'little)))
                 'little #f)
           (list 'out-of-line-native
                 (lambda (b i n) (apply nset (list b i n)))
                 (native-endianness) #t)))))


(define (run-bytevector-accessor-tests)
  (display "Bytevector accessors") (newline)
  (bytevector-accessor-tests))

(define (bytevector-accessor-tests)

  (define failures 0)

  (define (check id got want)
    (if (not (test id got want))
        (set! failures (+ failures 1))))

  (define seed 12345)

  (define (rand n)
    (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
    (modulo (quotient seed 16) n))

  (define native (native-endianness))

  (define (ref-u b i size order)
    (do ((j 0 (+ j 1))
         (r 0 (+ (* r 256)
                 (bytevector-u8-ref b (if (eq? order 'big)
                                          (+ i j)
                                          (+ i (- size 1 j)))))))
        ((= j size) r)))

  (define (ref-s b i size order)
    (let ((u (ref-u b i size order)))
      (if (>= u (expt 2 (- (* 8 size) 1)))
          (- u (expt 2 (* 8 size)))
          u)))

  ; The bytes of n in memory order.

  (define (encode n size order)
    (let* ((u (if (< n 0) (+ n (expt 2 (* 8 size))) n))
           (le (do ((j 0 (+ j 1))
                    (u u (quotient u 256))
                    (acc '() (cons (remainder u 256) acc)))
                   ((= j size) (reverse acc)))))
      (if (eq? order 'big) (reverse le) le)))

  (define (fails? thunk)
    (safely (lambda () (thunk) #f) #t))

  (define (filter-in p xs)
    (cond ((null? xs) '())
          ((p (car xs)) (cons (car xs) (filter-in p (cdr xs))))
          (else (filter-in p (cdr xs)))))

  (define table
    (bva:accessors
     (2 bytevector-u16-ref bytevector-s16-ref
        bytevector-u16-set! bytevector-s16-set!
        bytevector-u16-native-ref bytevector-s16-native-ref
        bytevector-u16-native-set! bytevector-s16-native-set!)
     (4 bytevector-u32-ref bytevector-s32-ref
        bytevector-u32-set! bytevector-s32-set!
        bytevector-u32-native-ref bytevector-s32-native-ref
        bytevector-u32-native-set! bytevector-s32-native-set!)
     (8 bytevector-u64-ref bytevector-s64-ref
        bytevector-u64-set! bytevector-s64-set!
        bytevector-u64-native-ref bytevector-s64-native-ref
        bytevector-u64-native-set! bytevector-s64-native-set!)))

  (define len 40)

  ; Bytes that are often 0, 255 or near 128, to reach the edges.

  (define (random-bytevector)
    (let ((b (make-bytevector len)))
      (do ((i 0 (+ i 1)))
          ((= i len) b)
        (bytevector-u8-set! b i
                            (case (rand 4)
                              ((0) 0)
                              ((1) 255)
                              ((2) (+ 120 (rand 16)))
                              (else (rand 256)))))))

  ; Values at the edges of the size's range and of the fixnum range
  ; (of 64-bit systems).

  (define (edge-values size signed?)
    (let* ((bits (* 8 size))
           (lo (if signed? (- (expt 2 (- bits 1))) 0))
           (hi (- (if signed? (expt 2 (- bits 1)) (expt 2 bits)) 1))
           (fx (expt 2 61)))
      (filter-in (lambda (n) (<= lo n hi))
                 (list lo (+ lo 1) -1 0 1 255 256 hi (- hi 1)
                       127 128 -128 -129 32767 32768 -32768 65535
                       (- fx 1) fx (- fx) (- (- fx) 1)
                       (expt 2 56) (* 31 (expt 2 56)) (* 32 (expt 2 56))
                       (- (* 32 (expt 2 56)) 1)
                       (- (expt 2 63)) (- (expt 2 64) 1)))))

  (define (random-value size signed?)
    (let* ((bits (* 8 size))
           (u (do ((j 0 (+ j 1))
                   (r 0 (+ (* r 256) (rand 256))))
                  ((= j size) r))))
      (if (and signed? (>= u (expt 2 (- bits 1))))
          (- u (expt 2 bits))
          u)))

  (define (read-tests size signed? rs)
    (do ((round 0 (+ round 1)))
        ((= round 30))
      (let ((b (random-bytevector)))
        (do ((i 0 (+ i 1)))
            ((> i (- len size)))
          (for-each
           (lambda (r)
             (let ((name (car r))
                   (proc (cadr r))
                   (order (caddr r))
                   (native? (cadddr r)))
               (if (or (not native?) (= 0 (remainder i size)))
                   (check (list 'ref size signed? name i)
                          (proc b i)
                          ((if signed? ref-s ref-u) b i size order)))))
           rs)))))

  (define (write-tests size signed? ws)
    (let ((values
           (append (edge-values size signed?)
                   (do ((k 0 (+ k 1))
                        (acc '() (cons (random-value size signed?) acc)))
                       ((= k 200) acc)))))
      (for-each
       (lambda (w)
         (let ((name (car w))
               (proc (cadr w))
               (order (caddr w))
               (native? (cadddr w)))
           (for-each
            (lambda (n)
              (let ((b (make-bytevector len 7))
                    (want (make-bytevector len 7))
                    (i (if native?
                           (* size (rand (quotient len size)))
                           (rand (- len size -1)))))
                (proc b i n)
                (do ((j i (+ j 1))
                     (bs (encode n size order) (cdr bs)))
                    ((null? bs))
                  (bytevector-u8-set! want j (car bs)))
                (check (list 'set size signed? name i n) b want)))
            values)))
       ws)))

  (define (error-tests size signed? rs ws)
    (let ((b (make-bytevector len 0))
          (bits (* 8 size)))
      (for-each
       (lambda (r)
         (let ((name (car r))
               (proc (cadr r))
               (native? (cadddr r)))
           (for-each
            (lambda (i)
              (check (list 'ref-error size signed? name i)
                     (fails? (lambda () (proc b i)))
                     #t))
            (append (list (- len size -1) (+ len 100) -1 -8
                          (- (expt 2 61) 1) (- (expt 2 61) 2)
                          (expt 2 70) 1.0 'x)
                    (if native? (list 1 (+ size 1) (- len size 1)) '())))
           (check (list 'ref-error-not-bytevector size name)
                  (fails? (lambda () (proc (vector 1 2 3 4 5 6 7 8 9) 0)))
                  #t)))
       rs)
      (for-each
       (lambda (w)
         (let ((name (car w))
               (proc (cadr w))
               (native? (cadddr w)))
           (for-each
            (lambda (n)
              (let ((b (make-bytevector len 7)))
                (check (list 'set-error size signed? name n)
                       (fails? (lambda () (proc b 0 n)))
                       #t)
                (check (list 'set-error-unchanged size signed? name n)
                       b
                       (make-bytevector len 7))))
            (append (if signed?
                        (list (expt 2 (- bits 1))
                              (- (- (expt 2 (- bits 1))) 1))
                        (list -1 (expt 2 bits) (- (expt 2 61))))
                    (list 1.0 'x (expt 2 200))))
           (check (list 'set-error-index size name)
                  (fails? (lambda () (proc b (- len size -1) 0)))
                  #t)
           (if native?
               (check (list 'set-error-alignment size name)
                      (fails? (lambda () (proc b 1 0)))
                      #t))))
       ws)))

  (for-each
   (lambda (entry)
     (let ((size (car entry))
           (rs (cadr entry))
           (ws (caddr entry)))
       (read-tests size #f (car rs))
       (read-tests size #t (cadr rs))
       (write-tests size #f (car ws))
       (write-tests size #t (cadr ws))
       (error-tests size #f (car rs) (car ws))
       (error-tests size #t (cadr rs) (cadr ws))))
   table)

  (allof "multi-byte bytevector accessors"
         (test "failures" failures 0)))
