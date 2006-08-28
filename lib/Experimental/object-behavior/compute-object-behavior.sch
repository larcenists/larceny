; Object behavior computation
; 26 August 1999 / lth
;
;
; COMPUTE-OBJECT-BEHAVIOR takes two arguments:
;   - a thunk (a computation) 
;   - an allocation interval, in bytes (an exact nonnegative integer)
; and returns a list of triples (tick age volume): at the end of
; interval <tick>, objects of age <age> had volume <volume>.  Volume is
; measured in bytes.  If the volume is 0, then the record is omitted.
;
; Load this file along with HASH and HASHTABLE.  All three programs
; _must_ be run compiled.  Compiling unsafe will gain you some in
; performance -- about 10% with the current compiler.
;
;
; Implementation.
;
; The program instruments the allocation procedures by overriding them
; with procedures that record the allocated object in an internal table
; along with the object's age.  At the end of every allocation interval,
; SRO is used to collect all objects in the system that are referenced 
; only once.  The objects in the internal table that are also in the set
; returned by SRO are removed from the table and are recorded as having
; been garbage collected.  The sweep for dead objects is expensive: in
; the worst case, proportional to the product of the number of objects 
; returned by SRO and the number of objects in the internal table.  
; Usually, both quantities are quite large.  This program attempts to
; speed the process by hashing the objects into a hash table before 
; every sweep.  In practice this yields good results, though the worst 
; case is just as bad.
;
; Linked structures present a problem in the following sense.  Every
; object in the linked structure is recorded in the internal table, but
; when the user program drops the last reference to the head of the
; structure, all nodes except the head will still be referenced from
; other nodes in the structure and from the table, so SRO won't be able
; to find them on the first sweep.  Circular structures are even worse:
; they are never found by SRO because every node is referenced from
; both the table and from other nodes in the structure.
;
; This program repeats the sweep for dead objects until none can be 
; collected at the end of each allocation interval.  The number of sweeps 
; is equal to the longest chain of dead objects.
;
; The program reduces the set of objects returned by SRO by performing
; an initial call to SRO to pick up all objects that are referenced only
; once at the beginning of the program, and by using doubly-linked 
; structures for some internal data, setting their reference counts to 
; at least two.  The hash tables are only used temporarily and do not 
; need this treatment.
;
;
; Performance (or lack thereof).
;
; The program has been tuned extensively for speed.  We can divide the
; program into five parts:
;
;   The computation: entering objects into the object table
;   Repeatedly:
;     Running SRO 
;     Building the hash table from the object table
;     Scanning the data returned by SRO and marking dead objects
;   Compacting the object table
;
; Entering an object into the table takes a roughly constant amount
; of time.  SRO takes time proportional to the amount of live storage
; in the system, because it performs a full trace.  Building the hashtable
; takes time proportional to the size of the object table, which, if
; we're running something challenging, will be large and in any case
; proportional to the amount of live data in the system.  Scanning the
; data depends on the data returned by SRO: if there is much garbage
; (SRO returns a lot of data), then the scan takes a lot of time; if there
; is little garbage and much live data, then scanning won't take long.
; Compaction has cost proportional to the amount of live controlled 
; data at the end of the collection, but is very fast, and in practice
; takes almost no time.
;
; In the past, scanning was the bottleneck but the scanner has been
; bummed to the point where, at present, for interesting benchmarks, 
; SRO and table building are clearly slower (numbers to appear).
;
; The program has also been tuned for space, primarily to reduce the 
; per-object overhead in internal data structures.  The per-object
; overhead can be reduced by another word.  In the best case, this will
; reduce the memory requirements of the program by 20%, when all objects
; are the smallest possible, that is, pairs.
;
;
; Caveats.
;
; The program doesn't handle allocation due to:
;   Rest arguments
;   LAMBDA
;   Number primitives (or other library procedures)
;   QUASIQUOTE
;   DELAY/FORCE
;   STRING->SYMBOL, though this is easy to fix
;   EVAL

(define *irrelevant* #f)
(define *debug-print* #t)
(define *old-gc-count* (gc-counter))

(define (report . args)
  (let ((cnt (- (gc-counter) *old-gc-count*)))
    (set! *old-gc-count* (gc-counter))
    (if *debug-print*
        (apply format #t 
               "Round=~a  Live=~a  SRO=~a  Collected=~a  GCs=~a  SRO=~a Build=~a  Scan=~a  Unbuild=~a~%"
               args))))

(define compute-object-behavior
  (let ((make-vector     make-vector)
        (vector          vector)
        (list->vector    list->vector)
        (vector->list    vector->list)
        (cons            cons)
        (list            list)
        (map             map)
        (append          append)
        (reverse         reverse)
        (make-string     make-string)
        (string          string)
        (list->string    list->string)
        (string->list    string->list)
        (string-copy     string-copy)
        (string-append   string-append)
        (substring       substring)
        (number->string  number->string)
        (*size*          2000))         ; Chosen to be large enough to
                                        ; trigger large-object allocation
                                        ; and have little internal
                                        ; fragmentation.
    
    (lambda (thunk interval)
 
      (define age 0)                       ; current age
      (define total-bytes 0)               ; byte counter since last profile
      (define live (vector 0))             ; count of live objects for each age
      (define object-profile #f)           ; summary information
      (define nobjects 0)                  ; count of live objects
      (define distinct 0)

      ; 'Objects' is a vector of vectors.  Curr_v is the index of the
      ; current working element.  Each inner vector has v_size elements.  
      ; Only the current working vector is partially filled; next_i has the 
      ; index of the next free element in that vector.
      ;
      ; A four-element structure (object age bytes hashcode) has been
      ; flattened into the vectors, to save space.  Furthermore, the
      ; bytes field is not actually stored, but is recomputed when needed,
      ; because it's so easy to do.
      ;
      ; Other tricks not yet employed:
      ;   - index the structure on age -- have one structure per age.
      ;
      ; Consider perhaps keeping double references to the vectors to
      ; avoid them being found by SRO.

      (define objects                     ; table of live objects
        (vector (make-vector (* 3 *size*) #f)))
      (define curr_v 0)                   ; working vector in 'objects'
      (define next_i 0)                   ; next free in working vector
      (define v_size (* 3 *size*))        ; size of inner vector
      (define incr 3)                     ; stepper increment

      (define (new-slot)
        (if (= next_i v_size)
            (begin 
              ;; Oveflow in inner vector
              (set! curr_v (+ curr_v 1))
              (if (= curr_v (vector-length objects))
                  (begin
                    ;; Overflow in outer vector 
                    (let* ((size (vector-length objects))
                           (nsize (* size 2))
                           (v (make-vector nsize #f)))
                      (do ((i 0 (+ i 1)))
                          ((= i curr_v))
                        (vector-set! v i (vector-ref objects i)))
                      (set! objects v))))
              (vector-set! objects curr_v (make-vector v_size #f))
              (set! next_i 0)))
        (let ((k (+ (* curr_v v_size) next_i)))
          (set! next_i (+ next_i incr))
          k))

      (define (slot-ref.age slot)
        (let ((v (vector-ref objects (quotient slot v_size)))
              (offs (remainder slot v_size)))
          (vector-ref v (+ offs 1))))

      (define (slot-ref.bytes slot)
        (let ((v (vector-ref objects (quotient slot v_size)))
              (offs (remainder slot v_size)))
          (object-size (vector-ref v offs))))

      (define (slot-set! slot object age hashcode)
        (let ((v (vector-ref objects (quotient slot v_size)))
              (offs (remainder slot v_size)))
          (vector-set! v offs object)
          (vector-set! v (+ offs 1) age)
          (vector-set! v (+ offs 2) hashcode)))

      (define (slot-kill! slot)
        (slot-set! slot #f #f #f))

; Inlined below at its only use.

;      (define (slot-for-each fn)
;        (do ((i 0 (+ i 1)))
;            ((> i curr_v))
;          (let ((v     (vector-ref objects i))
;                (limit (if (< i curr_v) v_size next_i))
;                (k     (* i v_size)))
;            (do ((j 0 (+ j incr)))
;                ((= j limit))
;              (fn (+ k j)
;                  (vector-ref v j)
;                  (vector-ref v (+ j 1))
;                  (vector-ref v (+ j 2)))))))

      (define (compact-object-table)
        (let loop ((free_i 0) (free_j 0) (scan_i 0) (scan_j 0))
          (cond ((and (= scan_i curr_v) (= scan_j next_i))
                 (do ((i (+ free_i 1) (+ i 1)))
                     ((> i scan_i))
                   (vector-set! objects i #f))
                 (set! curr_v free_i)
                 (set! next_i free_j))
                ((= free_j v_size)
                 (loop (+ free_i 1) 0 scan_i scan_j))
                ((= scan_j v_size)
                 (loop free_i free_j (+ scan_i 1) 0))
                ((not (vector-ref (vector-ref objects scan_i) scan_j))
                 (loop free_i free_j scan_i (+ scan_j incr)))
                (else
                 (let ((free_v (vector-ref objects free_i))
                       (scan_v (vector-ref objects scan_i)))
                   (do ((i 0 (+ i 1)))
                       ((= i incr))
                     (vector-set! free_v (+ free_j i)
                                  (vector-ref scan_v (+ scan_j i))))
                   (loop free_i (+ free_j incr) scan_i (+ scan_j incr)))))))

      ; An object has been allocated, so record it.

      (define (object-size obj)
        (cond ((pair? obj) 8)
              ((vector? obj) (* 4 (vector-length obj)))
              ((string? obj) (string-length obj))
              (else ???)))

      (define (allocate object)
        (let ((bytes (object-size object)))
          (slot-set! (new-slot) object age (equal-hash object))
          (set! nobjects (+ 1 nobjects))
          (set! total-bytes (+ total-bytes bytes))
          (vector-set! live age (+ (vector-ref live age) bytes))
          (if (>= total-bytes interval)
              (begin (if (or #t *debug-print*)
                         (begin (display "Computing profile.") (newline)))
                     (compute-object-profile)
                     (increment-age)
                     (set! total-bytes 0)))
          object))

      (define (compute-object-profile)
        
        (define collected (make-vector (+ age 1) 0))
        (define cc 0)
        (define key #f)  ; used by hash fn
        
        (define (try-to-collect x ht)
          (let ((probe (hashtable-get ht x)))
            (if probe
                (let ((age   (slot-ref.age probe))
                      (bytes (slot-ref.bytes probe)))
                  (set! cc (+ cc 1))
                  (vector-set! collected age (+ (vector-ref collected age)
                                                bytes))
                  (slot-kill! probe)
                  (set! nobjects (- nobjects 1))))))
         
        (define (summarize-live i)
          (let ((volume (- (vector-ref live i) (vector-ref collected i))))
            (vector-set! live i volume)
            (if (not (= volume 0))
                (set! object-profile
                      (vector age 
                              (- age i) 
                              (vector-ref live i)
                              object-profile
                              object-profile)))))
        
        (define (hashtable-from-objects ht)
          (do ((i 0 (+ i 1)))
              ((> i curr_v))
            (let ((v     (vector-ref objects i))
                  (limit (if (< i curr_v) v_size next_i))
                  (k     (* i v_size)))
              (do ((j 0 (+ j incr)))
                  ((= j limit))
                (let ((hash (vector-ref v (+ j 2)))
                      (obj  (vector-ref v j))
                      (slot (+ k j)))
                  (if obj
                      (begin 
                        (set! key hash)
                        (hashtable-unsafe-put-unique! ht obj slot)))))))
          (set! key #f)
          ht)

; Slot-for-each was inlined to give us the code above.
;
;        (define (hashtable-from-objects ht)
;          (slot-for-each
;           (lambda (slot object age hash)
;             (set! key hash)
;             (hashtable-unsafe-put-unique! ht object slot)))
;          (set! key #f)
;          ht)
        
        (define (objects-from-hashtable ht)
          (compact-object-table)
          objects)
        
	; This should probably use memstats-user-time from
        ; Lib/Common/memstats.sch rather than the magic number 23
        (define (memstats:user-cpu-time v)
          (vector-ref v 23))

        (let loop ((cc2 cc) (round 1))
          (clear-all-registers)
          (let* ((old-gc-counter (gc-counter))
                 (m0        (memstats))
                 (ref1      (sro -1 -1 1))
                 (l         (vector-length ref1))
                 (m1        (memstats))
                 (ht        (hashtable-from-objects 
                             (make-hashtable (lambda (x)
                                               (or key (equal-hash x)))
                                             optimized-assq
                                             (+ nobjects 
                                                (quotient nobjects 2)))))
                 (m2        (memstats))
                 (objs-live nobjects))
            (do ((i 0 (+ i 1)))
                ((= i l))
              (try-to-collect (vector-ref ref1 i) ht))
            (let ((m3 (memstats)))
              (if (= cc cc2)            ; Compact at end only
                  (set! objects (objects-from-hashtable ht)))
              (let ((m4 (memstats)))
                (hashtable-clear! ht)
                (report round 
                        objs-live 
                        l 
                        cc 
                        (- (gc-counter) old-gc-counter)
                        (- (memstats:user-cpu-time m1)
                           (memstats:user-cpu-time m0))
                        (- (memstats:user-cpu-time m2)
                           (memstats:user-cpu-time m1))
                        (- (memstats:user-cpu-time m3)
                           (memstats:user-cpu-time m2))
                        (- (memstats:user-cpu-time m4)
                           (memstats:user-cpu-time m3)))))
            (if (and (> cc cc2) (> nobjects 0))
                (loop cc (+ round 1)))))
        (do ((i 0 (+ i 1)))
            ((> i age))
          (summarize-live i)))
    
      (define (increment-age)
        (set! age (+ age 1))
        (let ((l (vector-length live)))
          (if (>= age l)
              (let ((v (make-vector (* 2 l) 0)))
                (do ((i 0 (+ i 1)))
                    ((= i l))
                  (vector-set! v i (vector-ref live i)))
                (set! live v)))))
      
      (define (allocate-list l)
        (do ((m l (cdr m)))
            ((null? m) l)
          (allocate m)))
      
      (define (instrumented-make-vector n . rest)
        (allocate (if (null? rest)
                      (make-vector n)
                      (make-vector n (car rest)))))
      
      (define (instrumented-vector . xs)
        (allocate (list->vector xs)))
      
      (define (instrumented-list->vector l)
        (allocate (list->vector l)))
      
      (define (instrumented-vector->list v)
        (allocate-list (vector->list v)))
      
      (define (instrumented-cons x y)
        (allocate (cons x y)))
      
      (define (instrumented-list . xs)
        (allocate-list xs))
      
      (define (instrumented-map p . ls)
        (allocate-list (apply map p ls)))
      
      (define (instrumented-append . ls)
        (allocate-list (apply append ls)))
      
      (define (instrumented-reverse l)
        (allocate-list (reverse l)))

      (define (instrumented-make-string n . rest)
        (allocate (if (null? rest)
                      (make-string n)
                      (make-string n (car rest)))))
      
      (define (instrumented-string . xs)
        (allocate (list->string xs)))
      
      (define (instrumented-list->string l)
        (allocate (list->string l)))
      
      (define (instrumented-string->list s)
        (allocate-list (string->list s)))
      
      (define (instrumented-string-copy s)
        (allocate (string-copy s)))
      
      (define (instrumented-string-append . ss)
        (allocate (apply string-append ss)))
      
      (define (instrumented-substring s x y)
        (allocate (substring s x y)))
      
      (define (instrumented-number->string n . rest)
        (let ((s (if (null? rest)
                     (number->string n)
                     (number->string n (car rest)))))
          (allocate s)))
      
      (define (begin-computation)
        (install-procedures instrumented-make-vector instrumented-vector
                            instrumented-list->vector instrumented-vector->list
                            instrumented-cons instrumented-list 
                            instrumented-map instrumented-append
                            instrumented-reverse instrumented-make-string
                            instrumented-string instrumented-list->string
                            instrumented-string->list instrumented-string-copy
                            instrumented-string-append instrumented-substring
                            instrumented-number->string))
      
      (define (end-computation)
        (install-procedures make-vector vector list->vector vector->list
                            cons list map append reverse make-string
                            string list->string string->list string-copy
                            string-append substring number->string))
      
      ; Hang onto everything that is referenced once when we start so that 
      ; we won't have to look at it every time.
      (set! *irrelevant* (sro -1 -1 1))
      (dynamic-wind
       begin-computation
       (lambda () (thunk) #f)
       end-computation)
; This appears to be little but a waste of time.
;      (if (> nobjects 0) 
;          (begin (display "Computing final object profile.")
;                 (newline)
;                 (compute-object-profile)))
      (set! *irrelevant* #f)
      (let loop ((op object-profile) (p '()))
        (if (not op) 
            p
            (loop (vector-ref op 3)
                  (cons (list (vector-ref op 0)
                              (vector-ref op 1)
                              (vector-ref op 2))
                        p)))))))

; This is optimized only in the sense that if this file is compiled
; unsafe, then this function is compiled unsafe also!

(define (optimized-assq x l)

  (define (assq x l)
    (cond ((null? l) #f)
          ((eq? (caar l) x) (car l))
          (else (assq x (cdr l)))))

  (assq x l))

(define (clear-all-registers)
  (clear-all-registers-aux 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define (clear-all-registers-aux . ignored)
  #t)

(define (install-procedures _make-vector _vector _list->vector _vector->list
                            _cons _list _map _append _reverse _make-string
                            _string _list->string _string->list _string-copy
                            _string-append _substring _number->string)
    (set! make-vector  _make-vector)
    (set! vector       _vector)
    (set! list->vector _list->vector)
    (set! vector->list _vector->list)
    (set! cons         _cons)
    (set! list         _list)
    (set! map          _map)
    (set! append       _append)
    (set! reverse      _reverse)
    (set! make-string  _make-string)
    (set! string       _string)
    (set! list->stirng _list->string)
    (set! string->list _string->list)
    (set! string-copy  _string-copy)
    (set! string-append _string-append)
    (set! substring     _substring)
    (set! number->string _number->string))


; eof
