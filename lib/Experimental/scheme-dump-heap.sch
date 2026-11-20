; Heap dumping in Scheme, for any collector.
;
; (scheme-dump-heap filename proc)
; (scheme-dump-interactive-heap filename)
;
; These do what dump-heap and dump-interactive-heap do, but they work
; under every collector, not only under the stop-and-copy collector
; (the runtime's dumper for the other collectors is a stub; see
; dump_generational_system in src/Rts/Sys/memmgr.c).  Native Larceny
; only: in Petit Larceny, procedures hold addresses of C functions,
; which the runtime's dumper writes relative to an anchor that Scheme
; cannot find.
;
; The heap is written in the split format of the runtime's dumper
; (src/Rts/Sys/heapio.c), which every collector loads: code vectors in
; the text segment, everything else in the data segment.  It holds what
; the roots reach.  Of the roots, only G_STARTUP and G_CALLOUTS survive
; a load (src/Rts/Sys/gc.c): the startup procedure, and the global cell
; of millicode-support, which the arithmetic millicode calls into.
;
; The dumper reads objects with peek-bytes, by address, and never
; holds them as Scheme values, so it needs addresses to stay put while
; it works: the walk allocates nothing, so no collection can happen
; during it.  Everything it uses is allocated first, sized by sro.  If
; a collection happens anyway (gc-counter changes), that is a bug in
; this file, and the dump fails.  The walk runs in passes over the
; objects that the roots reach:
;
;   1. Find them, breadth first.  Each gets an index, in a table from
;      address to index (open addressing, of fixnums only).
;   1b. Find the code vectors that no procedure points to yet, such as
;      those of nested lambdas (see sdh:pass-code and sdh:classify).
;   2. Give each an offset in its segment.
;   3. Copy each into the image, with pointers turned into offsets.
;
; The code here must not allocate: no closures in the walk, no deep
; recursion, fixnum arithmetic only (words are read and written as
; 32-bit halves; a word with the high bit set is no fixnum), no output.

; Layouts (include/layouts.ch, include/globals.ch).

(define sdh:heap-version 10)            ; HEAP_VERSION
(define sdh:heap-split 1)               ; HEAP_SPLIT
(define sdh:root-count 44)              ; LAST_ROOT - FIRST_ROOT + 1
(define sdh:g-startup 36)               ; G_STARTUP
(define sdh:g-callouts 37)              ; G_CALLOUTS
(define sdh:false-word 2)               ; #f
(define sdh:pagesize 4096)              ; PAGESIZE (src/Rts/Sys/gclib.h)
(define sdh:chunk-bytes 65536)          ; Bytes read by one peek-bytes

; The walker's state, a vector allocated before the walk.

(define-syntax sdh:scratch  (syntax-rules () ((_ st) (vector-ref st 0))))
(define-syntax sdh:keys     (syntax-rules () ((_ st) (vector-ref st 1))))
(define-syntax sdh:vals     (syntax-rules () ((_ st) (vector-ref st 2))))
(define-syntax sdh:queue    (syntax-rules () ((_ st) (vector-ref st 3))))
(define-syntax sdh:sizes    (syntax-rules () ((_ st) (vector-ref st 4))))
(define-syntax sdh:offs     (syntax-rules () ((_ st) (vector-ref st 5))))
(define-syntax sdh:flags    (syntax-rules () ((_ st) (vector-ref st 6))))
(define-syntax sdh:image    (syntax-rules () ((_ st) (vector-ref st 7))))
(define-syntax sdh:count    (syntax-rules () ((_ st) (vector-ref st 8))))
(define-syntax sdh:mask     (syntax-rules () ((_ st) (vector-ref st 9))))
(define-syntax sdh:error    (syntax-rules () ((_ st) (vector-ref st 10))))
(define-syntax sdh:errinfo  (syntax-rules () ((_ st) (vector-ref st 11))))
(define-syntax sdh:gccount  (syntax-rules () ((_ st) (vector-ref st 12))))
(define-syntax sdh:dbytes   (syntax-rules () ((_ st) (vector-ref st 13))))
(define-syntax sdh:tbytes   (syntax-rules () ((_ st) (vector-ref st 14))))
(define-syntax sdh:max-code (syntax-rules () ((_ st) (vector-ref st 15))))
(define-syntax sdh:min-data (syntax-rules () ((_ st) (vector-ref st 16))))
(define sdh:state-size 17)

; Flags of an object.

(define sdh:flag-code 1)        ; A code vector, in the text segment
(define sdh:flag-consts 2)      ; A constant vector
(define sdh:flag-done 4)        ; A constant vector looked at by pass 1b
(define sdh:flag-pattern 8)     ; Code by the pattern of pass 1b

(define (sdh:flag? st i bit)
  (not (zero? (fxlogand (bytevector-ref (sdh:flags st) i) bit))))

(define (sdh:flag! st i bit)
  (bytevector-set! (sdh:flags st) i
                   (fxlogior (bytevector-ref (sdh:flags st) i) bit)))

; Error codes.

(define sdh:err-queue 1)        ; More objects than sro counted
(define sdh:err-header 2)       ; Not an object header
(define sdh:err-gc 3)           ; A collection happened
(define sdh:err-image 4)        ; More words than sro counted
(define sdh:err-lost 5)         ; Pointer to an object not found

(define (scheme-dump-heap filename proc)
  (cond ((not (string? filename))
         (error "scheme-dump-heap: invalid file name: " filename))
        ((not (procedure? proc))
         (error "scheme-dump-heap: invalid procedure: " proc)))
  (sdh:check-system)
  (let ((run-init-procedures
         (sdh:global-value dump-heap 'run-init-procedures)))
    (display "; Dumping heap...")
    (newline)
    (sdh:dump filename
              (lambda (argv)
                (command-line-arguments argv)
                (run-init-procedures)
                (proc argv)))
    (display "; Done.")
    (newline)))

(define (scheme-dump-interactive-heap filename)
  (scheme-dump-heap filename
                    (sdh:global-value dump-interactive-heap
                                      'interactive-entry-point)))

(define (sdh:check-system)
  (let ((features (system-features)))
    (define (feature name)
      (let ((probe (assq name features)))
        (and probe (cdr probe))))
    (if (not (and (eq? (feature 'codevector-representation) 'bytevector)
                  (eqv? (feature 'arch-word-size) 64)
                  (eq? (feature 'arch-endianness) 'little)))
        (error "scheme-dump-heap: needs 64-bit little-endian native Larceny"))))

; The value of the global variable name that the compiled procedure
; proc refers to.  Global cells are pairs (value . name) in constant
; vectors, which nest for nested lambdas.  Used for procedures that
; are not in the interaction environment.

(define (sdh:global-value proc name)
  (define (search v)
    (let loop ((i 0))
      (if (= i (vector-length v))
          #f
          (let ((x (vector-ref v i)))
            (cond ((and (pair? x) (eq? (cdr x) name) (procedure? (car x)))
                   (car x))
                  ((and (vector? x) (search x)))
                  (else
                   (loop (+ i 1))))))))
  (or (search (procedure-ref proc 1))
      (error "scheme-dump-heap: cannot find " name)))

(define (sdh:dump filename startup)
  (reset-all-hashtables!)
  (let* ((counts (sdh:count-live))
         (callouts (caddr counts))
         (cap (+ (car counts) 4096))
         (m (let loop ((m 1024)) (if (< m (* 2 cap)) (loop (* 2 m)) m)))
         (st (make-vector sdh:state-size 0)))
    (vector-set! st 0 (make-bytevector sdh:chunk-bytes 0))
    (vector-set! st 1 (make-vector m 0))
    (vector-set! st 2 (make-vector m 0))
    (vector-set! st 3 (make-vector cap 0))
    (vector-set! st 4 (make-vector cap 0))
    (vector-set! st 5 (make-vector cap 0))
    (vector-set! st 6 (make-bytevector cap 0))
    (vector-set! st 7 (make-bytevector
                       (+ (* 8 (cadr counts)) (* 2 sdh:pagesize)) 0))
    (vector-set! st 9 (- m 1))
    (vector-set! st 15 -1)
    (vector-set! st 16 (expt 2 60))
    (call-without-interrupts
     (lambda ()
       (sdh:walk st
                 (+ (* 16 (.internal:machine-address startup)) 7)
                 (+ (* 16 (.internal:machine-address callouts)) 1))))
    (if (not (zero? (sdh:error st)))
        (error "scheme-dump-heap: failed: "
               (case (sdh:error st)
                 ((1) "more objects than counted")
                 ((2) "not an object header at address")
                 ((3) "a collection happened during the walk")
                 ((4) "more words than counted")
                 ((5) "lost an object at address")
                 (else (sdh:error st)))
               (sdh:errinfo st)))
    (sdh:write-heap st filename)
    ;; Keep the roots alive through the walk.
    (and (procedure? startup) (pair? callouts))))

; The number of live objects, the words they take, and the global cell
; of millicode-support (a pair (value . name), its value a vector).

(define (sdh:count-live)
  (let ((objs (sro -1 -1 -1)))
    (define (words n)
      (* 2 (quotient (+ n 2) 2)))       ; Header and n words, rounded up
    (let loop ((i 0) (words-total 0) (callouts #f))
      (if (= i (vector-length objs))
          (if callouts
              (list (vector-length objs) words-total callouts)
              (error "scheme-dump-heap: cannot find millicode-support"))
          (let ((x (vector-ref objs i)))
            (loop (+ i 1)
                  (+ words-total
                     (cond ((pair? x) 2)
                           ((procedure? x) (words (procedure-length x)))
                           ((vector-like? x) (words (vector-like-length x)))
                           ((bytevector-like? x)
                            (words (quotient
                                    (+ (bytevector-like-length x) 7) 8)))
                           (else 0)))
                  (if (and (pair? x)
                           (eq? (cdr x) 'millicode-support)
                           (vector? (car x)))
                      x
                      callouts)))))))

(define (sdh:fail! st code info)
  (if (zero? (sdh:error st))
      (begin (vector-set! st 10 code)
             (vector-set! st 11 info))))

; The roots are objects 0 and 1.

(define (sdh:walk st startup callouts)
  (vector-set! st 12 (gc-counter))
  (sdh:intern! st startup)
  (sdh:intern! st callouts)
  (sdh:pass1 st 0)
  (if (zero? (sdh:error st)) (sdh:pass-code st))
  (if (zero? (sdh:error st)) (sdh:classify st 0))
  (if (zero? (sdh:error st)) (sdh:pass2 st))
  (if (zero? (sdh:error st)) (sdh:pass3 st 0))
  (if (not (= (gc-counter) (sdh:gccount st)))
      (sdh:fail! st sdh:err-gc 0)))

; Reads count bytes at address addr into the bytevector bv.  The
; syscall procedure (and so peek-bytes) takes a rest list; %syscall,
; which it calls, is the trampoline into the runtime, and allocates
; nothing.  syscall:peek-bytes is 40 (src/Lib/Common/syscall-id.sch).

(define sdh:%syscall (sdh:global-value syscall '%syscall))

(define (sdh:peek-bytes addr bv count)
  (sdh:%syscall 40 addr bv count))

; Words as 32-bit halves.

(define (sdh:u32 bv i)
  (fxlogior (fxlogior (bytevector-ref bv i)
                      (fxlsh (bytevector-ref bv (+ i 1)) 8))
            (fxlogior (fxlsh (bytevector-ref bv (+ i 2)) 16)
                      (fxlsh (bytevector-ref bv (+ i 3)) 24))))

(define (sdh:u32-set! bv i v)
  (bytevector-set! bv i (fxlogand v 255))
  (bytevector-set! bv (+ i 1) (fxlogand (fxrshl v 8) 255))
  (bytevector-set! bv (+ i 2) (fxlogand (fxrshl v 16) 255))
  (bytevector-set! bv (+ i 3) (fxlogand (fxrshl v 24) 255)))

; The table from address (a tagged pointer, as a fixnum) to index.

(define (sdh:hash st p)
  (fxlogand (fxlogxor (fxrshl p 4) (fxrshl p 21)) (sdh:mask st)))

(define (sdh:lookup st p)
  (let ((keys (sdh:keys st)))
    (let loop ((h (sdh:hash st p)))
      (let ((k (vector-ref keys h)))
        (cond ((eq? k p) (vector-ref (sdh:vals st) h))
              ((eq? k 0) -1)
              (else (loop (fxlogand (+ h 1) (sdh:mask st)))))))))

; The index of p, which is queued if it is new; -1 if the queue is full.

(define (sdh:intern! st p)
  (let ((keys (sdh:keys st)))
    (let loop ((h (sdh:hash st p)))
      (let ((k (vector-ref keys h)))
        (cond ((eq? k p) (vector-ref (sdh:vals st) h))
              ((eq? k 0)
               (let ((i (sdh:count st)))
                 (if (= i (vector-length (sdh:queue st)))
                     (begin (sdh:fail! st sdh:err-queue 0) -1)
                     (begin (vector-set! keys h p)
                            (vector-set! (sdh:vals st) h i)
                            (vector-set! (sdh:queue st) i p)
                            (vector-set! st 8 (+ i 1))
                            i))))
              (else (loop (fxlogand (+ h 1) (sdh:mask st)))))))))

; The object at address addr with pointer tag tag: the number of words
; after its header, which are read to scratch; #f if the header is
; wrong.  Pairs have no header and two words.

(define (sdh:body-words st addr tag)
  (if (= tag 1)
      2
      (let ((scratch (sdh:scratch st)))
        (sdh:peek-bytes addr scratch 8)
        (let* ((lo (sdh:u32 scratch 0))
               (bytes (+ (fxrshl lo 8) (fxlsh (sdh:u32 scratch 4) 24))))
          (cond ((= tag 5)
                 (and (= (fxlogand lo #xe3) #xc2)    ; BV_HDR
                      (fxrshl (+ bytes 7) 3)))
                ((= tag 3)
                 (and (= (fxlogand lo #xe3) #xa2)    ; VEC_HDR
                      (fxrshl bytes 3)))
                (else
                 (and (= (fxlogand lo #xe3) #xe2)    ; PROC_HDR
                      (fxrshl bytes 3))))))))

; Pass 1.

(define (sdh:pass1 st i)
  (if (and (< i (sdh:count st)) (zero? (sdh:error st)))
      (begin (sdh:scan st i)
             (sdh:pass1 st (+ i 1)))))

(define (sdh:scan st i)
  (let* ((p (vector-ref (sdh:queue st) i))
         (tag (fxlogand p 7))
         (addr (- p tag))
         (n (sdh:body-words st addr tag)))
    (cond ((not (= (gc-counter) (sdh:gccount st)))
           (sdh:fail! st sdh:err-gc 0))
          ((not n)
           (sdh:fail! st sdh:err-header addr))
          (else
           (vector-set! (sdh:sizes st) i
                        (if (= tag 1) 2 (fxlogand (+ n 2) -2)))
           (if (and (not (= tag 5)) (< addr (sdh:min-data st)))
               (vector-set! st 16 addr))
           (cond ((= tag 1)
                  (sdh:scan-chunks st addr 0 2 #f))
                 ((not (= tag 5))
                  (sdh:scan-chunks st addr 1 n (= tag 7))))))))

; Scans the n words from word w of the object at addr, by chunks.  If
; proc? is true, words w and w+1 are a procedure's code and constants.

(define (sdh:scan-chunks st addr w n proc?)
  (if (and (> n 0) (zero? (sdh:error st)))
      (let ((k (min n (quotient sdh:chunk-bytes 8))))
        (sdh:peek-bytes (+ addr (* 8 w)) (sdh:scratch st) (* 8 k))
        (sdh:scan-words st 0 k proc?)
        (sdh:scan-chunks st addr (+ w k) (- n k) #f))))

(define (sdh:scan-words st j k proc?)
  (if (< j k)
      (let* ((scratch (sdh:scratch st))
             (lo (sdh:u32 scratch (* 8 j))))
        (if (= (fxlogand lo 1) 1)
            (let* ((q (+ lo (fxlsh (sdh:u32 scratch (+ (* 8 j) 4)) 32)))
                   (x (sdh:intern! st q)))
              (cond ((or (not proc?) (< x 0)))
                    ((and (= j 0) (= (fxlogand q 7) 5))
                     (sdh:flag! st x sdh:flag-code)
                     (if (> (- q 5) (sdh:max-code st))
                         (vector-set! st 15 (- q 5))))
                    ((and (= j 1) (= (fxlogand q 7) 3))
                     (sdh:flag! st x sdh:flag-consts)))))
        (sdh:scan-words st (+ j 1) k proc?))))

; Pass 1b.  A nested lambda's code vector is in the constant vector of
; the procedure that makes it, followed by the lambda's constant
; vector ($lambda in src/Asm/Fence/pass5p2.sch).  So a bytevector
; (subtag 0) followed by a vector in a constant vector matches the
; pattern of a code vector, and the vector is taken for a constant
; vector; repeated until nothing changes, since constant vectors nest.
; A bytevector literal followed by a vector literal matches too, so the
; pattern decides only where the address cannot (sdh:classify).

(define (sdh:pass-code st)
  (if (sdh:pass-code1 st 0 #f)
      (sdh:pass-code st)))

(define (sdh:pass-code1 st i changed?)
  (cond ((or (= i (sdh:count st)) (not (zero? (sdh:error st))))
         changed?)
        ((and (sdh:flag? st i sdh:flag-consts)
              (not (sdh:flag? st i sdh:flag-done)))
         (sdh:flag! st i sdh:flag-done)
         (let* ((p (vector-ref (sdh:queue st) i))
                (addr (- p 3))
                (n (sdh:body-words st addr 3)))
           (if n
               (sdh:scan-consts st addr 1 n)
               (sdh:fail! st sdh:err-header addr)))
         (sdh:pass-code1 st (+ i 1) #t))
        (else
         (sdh:pass-code1 st (+ i 1) changed?))))

; Looks at words k and k+1 of the vector at addr, for k from k to n-1.

(define (sdh:scan-consts st addr k n)
  (if (< k n)
      (let ((scratch (sdh:scratch st)))
        (sdh:peek-bytes (+ addr (* 8 k)) scratch 16)
        (let ((a (sdh:u32 scratch 0))
              (b (sdh:u32 scratch 8)))
          (if (and (= (fxlogand a 7) 5) (= (fxlogand b 7) 3))
              (let* ((pa (+ a (fxlsh (sdh:u32 scratch 4) 32)))
                     (xa (sdh:lookup st pa))
                     (xb (sdh:lookup st (+ b (fxlsh (sdh:u32 scratch 12) 32)))))
                (sdh:peek-bytes (- pa 5) scratch 8)   ; The header of a
                (if (and (>= xa 0) (>= xb 0)
                         (= (fxlogand (sdh:u32 scratch 0) #xff) #xc2))
                    (begin (sdh:flag! st xa sdh:flag-pattern)
                           (sdh:flag! st xb sdh:flag-consts))))))
        (sdh:scan-consts st addr (+ k 1) n))))

; Which bytevectors are code, by address.  The code space is an arena
; of its own, normally just below the data arena (arena_init in
; src/Rts/Sys/osdep-generic.c).  So if the highest code vector that a
; procedure points to is below the lowest pair, vector or procedure, a
; bytevector at or below the former is code and one at or above the
; latter is data; for those in between, and if the arenas are not laid
; out that way, the pattern of pass 1b decides.  (Only the runtime knows
; exactly: is_code_page.)

(define (sdh:classify st i)
  (if (< i (sdh:count st))
      (let ((p (vector-ref (sdh:queue st) i)))
        (if (and (= (fxlogand p 7) 5)
                 (not (sdh:flag? st i sdh:flag-code))
                 (let ((addr (- p 5))
                       (max-code (sdh:max-code st))
                       (min-data (sdh:min-data st)))
                   (cond ((not (< -1 max-code min-data))
                          (sdh:flag? st i sdh:flag-pattern))
                         ((<= addr max-code) #t)
                         ((>= addr min-data) #f)
                         (else (sdh:flag? st i sdh:flag-pattern)))))
            (sdh:flag! st i sdh:flag-code))
        (sdh:classify st (+ i 1)))))

; Pass 2.  Segments are padded to whole pages, as the runtime does.

(define (sdh:pass2 st)
  (let loop ((i 0) (dbytes 0) (tbytes 0))
    (if (< i (sdh:count st))
        (let ((bytes (* 8 (vector-ref (sdh:sizes st) i))))
          (if (sdh:flag? st i sdh:flag-code)
              (begin (vector-set! (sdh:offs st) i tbytes)
                     (loop (+ i 1) dbytes (+ tbytes bytes)))
              (begin (vector-set! (sdh:offs st) i dbytes)
                     (loop (+ i 1) (+ dbytes bytes) tbytes))))
        (let ((dbytes (sdh:round-page dbytes))
              (tbytes (sdh:round-page tbytes)))
          (vector-set! st 13 dbytes)
          (vector-set! st 14 tbytes)
          (if (> (+ dbytes tbytes) (bytevector-length (sdh:image st)))
              (sdh:fail! st sdh:err-image 0))))))

(define (sdh:round-page n)
  (* sdh:pagesize (quotient (+ n (- sdh:pagesize 1)) sdh:pagesize)))

; Pass 3.  The image holds the data segment, then the text segment.

(define (sdh:pass3 st i)
  (if (and (< i (sdh:count st)) (zero? (sdh:error st)))
      (begin (sdh:emit st i)
             (sdh:pass3 st (+ i 1)))))

(define (sdh:emit st i)
  (let* ((p (vector-ref (sdh:queue st) i))
         (tag (fxlogand p 7))
         (addr (- p tag))
         (n (sdh:body-words st addr tag))
         (base (+ (vector-ref (sdh:offs st) i)
                  (if (sdh:flag? st i sdh:flag-code)
                      (sdh:dbytes st)
                      0))))
    (cond ((not (= (gc-counter) (sdh:gccount st)))
           (sdh:fail! st sdh:err-gc 0))
          ((not n)
           (sdh:fail! st sdh:err-header addr))
          ((= tag 1)
           (sdh:emit-chunks st addr 0 2 base #t))
          (else
           ;; The header has a clear low bit, so it is copied as is.
           (sdh:emit-chunks st addr 0 (+ n 1) base (not (= tag 5)))))))

; Copies the n words from word w of the object at addr to the image at
; byte base (where the object's word 0 goes), turning pointers into
; offsets if reloc? is true.

(define (sdh:emit-chunks st addr w n base reloc?)
  (if (and (> n 0) (zero? (sdh:error st)))
      (let ((k (min n (quotient sdh:chunk-bytes 8))))
        (sdh:peek-bytes (+ addr (* 8 w)) (sdh:scratch st) (* 8 k))
        (sdh:emit-words st 0 k (+ base (* 8 w)) reloc?)
        (sdh:emit-chunks st addr (+ w k) (- n k) base reloc?))))

(define (sdh:emit-words st j k dst reloc?)
  (if (< j k)
      (let* ((scratch (sdh:scratch st))
             (image (sdh:image st))
             (lo (sdh:u32 scratch (* 8 j)))
             (hi (sdh:u32 scratch (+ (* 8 j) 4)))
             (d (+ dst (* 8 j))))
        (if (and reloc? (= (fxlogand lo 1) 1))
            (let ((x (sdh:lookup st (+ lo (fxlsh hi 32)))))
              (if (< x 0)
                  (sdh:fail! st sdh:err-lost (+ lo (fxlsh hi 32)))
                  (let ((off (vector-ref (sdh:offs st) x)))
                    (sdh:u32-set! image d
                                  (fxlogior (fxlogand off #xffffffff)
                                            (fxlogand lo 7)))
                    (sdh:u32-set! image (+ d 4)
                                  (if (sdh:flag? st x sdh:flag-code)
                                      (fxlogior (fxrshl off 32) #x80000000)
                                      (fxrshl off 32))))))
            (begin (sdh:u32-set! image d lo)
                   (sdh:u32-set! image (+ d 4) hi)))
        (sdh:emit-words st (+ j 1) k dst reloc?))))

; Writing the file (this may allocate).  The roots are objects 0 and 1,
; in the data segment.

(define (sdh:write-heap st filename)
  (let ((header (make-bytevector (* 8 (+ 1 sdh:root-count 2)) 0))
        (dbytes (sdh:dbytes st))
        (tbytes (sdh:tbytes st)))
    (define (word! i lo hi)
      (sdh:u32-set! header (* 8 i) lo)
      (sdh:u32-set! header (+ (* 8 i) 4) hi))
    (word! 0 (+ (* sdh:heap-split 65536) sdh:heap-version) 0)
    (do ((i 0 (+ i 1)))
        ((= i sdh:root-count))
      (word! (+ i 1) sdh:false-word 0))
    (word! (+ 1 sdh:g-startup) (+ (vector-ref (sdh:offs st) 0) 7) 0)
    (word! (+ 1 sdh:g-callouts) (+ (vector-ref (sdh:offs st) 1) 1) 0)
    (word! (+ 1 sdh:root-count) (quotient tbytes 8) 0)
    (word! (+ 2 sdh:root-count) (quotient dbytes 8) 0)
    (if (file-exists? filename)
        (delete-file filename))
    (let ((out (open-file-output-port filename))
          (image (sdh:image st)))
      (put-bytevector out header)
      (put-bytevector out image dbytes tbytes)
      (put-bytevector out image 0 dbytes)
      (close-port out))))

; eof
