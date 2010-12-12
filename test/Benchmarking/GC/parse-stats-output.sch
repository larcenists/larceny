(require "Experimental/temp-files")

(define (stats-read)
  (let ((f (make-temporary-file "larcenystats~a")))
    (stats-dump-on f)
    (collect) ;; to ensure that (at least) one gc stats result ends up in file
    (stats-dump-off)
    (let ((v (call-with-input-file f read)))
      (delete-file f)
      v)))

;; An Entry is one of:
;; - (vector Symbol X_1 .. X_n)
;; - (list Symbol X_1 .. X_n)

;; An Dataset is one of:
;; - Vectorof[X]
;; - Listof[X]
;; where the X <: [Union Entry Any]

;; potential idea: generalize extract-entry so that if we do not find
;; the entry, *and* there is deeper structure to explore, then do so,
;; returning rather than just an entry, list of prefixed entries that
;; includes the data about what path led to that particular entry.
;; 
;; (This probably turns into XPath and/or Demeter in the limit, so
;;  I should investigate those options before spending much time on
;;  such hacks, though...)
;; 
;; The other problem with such hacks is that they might not compose
;; properly, depending on how the resulting structure is formatted...
;; (this is what I discovered when I tried an "obvious" generalization
;; to allow queries using Listof[Symbol] as the key).

;; extract-entry : Dataset Symbol -> Maybe[Entry]
(define (extract-entry s k)
  (let ((s (cond ((vector? s) (vector->list s))
                 ((list? s)   s))))
    (cond ((memf (lambda (e)
                   (or (and (vector? e)
                            (> (vector-length e) 0)
                            (eq? k (vector-ref e 0)))
                       (and (pair? e)
                            (eq? k (car e)))))
                 s)
           => car)
          (else #f))))

;; extract-path : Dataset (cons Symbol [Listof Symbol]) -> Maybe[Entry]
(define (extract-path s p)
  (define (extract s k)
    (cond ((pair? s)
           (if (eq? k (car s))
               s
               (let ((a (extract (car s) k))
                     (d (extract (cdr s) k)))
                 (cond ((and (not a) (not d)) #f)
                       ((not d) a)
                       ((not a) d)
                       (else (cons a d))))))
          ((vector? s)
           (let ((r (extract (vector->list s) k)))
             (if (list? r)
                 (list->vector r)
                 r)))
          (else
           #f)))
  (let ((entry (extract s (car p))))
    (cond ((not entry) 
           (if (list? s) 
               (let ((results (map (lambda (x) (extract-path x p)) s)))
                 (if (null? (cdr results))
                     (car results)
                     results))
               #f))
          ((null? (cdr p)) entry)
          (else 
           (extract-path (cadr entry) (cdr p))))))

;; print-header-line : -> void
;; print-bench-line : Dataset -> void
;; process-and-print-log : Filename -> void
(define headers/paths
  '((benchmark (last-stashed-stats name:))

    (max-pause (last-stashed-stats gc-max-pause:   elapsed))
    (tot-ms    (last-stashed-stats elapsed-time:))
    (tot-maxw  (stats-dump mem_allocated_max))

    (cheney-ms (last-stashed-stats gc-total-time:  elapsed))
    (mark-ms   (last-stashed-stats mark-time:      elapsed))
    (sumz-ms   (last-stashed-stats summarize-time: elapsed)) 

    (heap-peak (stats-dump heap_allocated_peak))
    (rem-peak (stats-dump remset_allocated_peak))
    (sumz-peak (stats-dump summ_allocated_peak))
    (mark-peak (stats-dump smircy_allocated_peak))
    (rts-peak  (stats-dump rts_allocated_peak))
    (frag-peak (stats-dump heap_fragmentation_peak))

    (heap-maxw (stats-dump heap_allocated_max))
    (rem-maxw  (stats-dump remset_allocated_max))
    (sumz-maxw (stats-dump summ_allocated_max))
    (mark-maxw (stats-dump smircy_allocated_max))
    (rts-maxw  (stats-dump rts_allocated_max))
    (frag-maxw (stats-dump heap_fragmentation_max))
    ))

(define (header-line-list)
  (define (header-convert-char c)
    (if (char=? c #\-) #\_ (char-upcase c)))
  (define (header-symbol->string x)
    (list->string (map header-convert-char 
                       (string->list (symbol->string x)))))
  (map header-symbol->string (map car headers/paths)))

(define (bench-line-list s)
  (define (second x)
    (cond ((vector?   x) (vector-ref x 1))
          ((list?     x) (list-ref x 1))))
  (define (extracted-second path)
    (second (extract-path s path)))
  (map extracted-second (map cadr headers/paths)))

(define (print-header-line)
  (define (pr x) (write x) (display ",\t"))
  (for-each pr (header-line-list)))

(define (print-bench-line s)
  (define (pr x) (write x) (display ",\t"))
  (for-each pr (bench-line-list s)))

(define (process-log f)
  (call-with-input-file f
    (lambda (in)
      (let* ((line-0 (read in))
             (line-1 (read in)))
        `((,line-1 ,line-0)
          ,(header-line-list)
          ,@(do ((x (read in) (read in))
                 (l '() (cons (bench-line-list x) l)))
                ((eof-object? x) (reverse l))))))))
;; A DataElem is one of:
;; -- String
;; -- Number
;; -- #f

;; A DataMatrix is a [Listof [Listof DataElem]]

;; build-list : Nat (Nat -> X) -> [Listof X]
(define (build-list n f)
  (do ((i 0 (+ i 1))
       (l '() (cons (f i) l)))
      ((= i n) (reverse l))))

;; elem->string : DataElem -> String
;; converts elem to a string fit for passing to display 
(define (elem->string x)
  (cond ((string? x) (call-with-output-string (lambda (o) (write x o))))
        ((number? x) (call-with-output-string 
                      (lambda (o) 
                        (write (if (not (integer? x)) (exact->inexact x) x) 
                               o))))
        ((not x)     "")))

;; elem-written-width : DataElem -> Nat
(define (elem-written-width x)
  (string-length (elem->string x)))

;; elem-print : DataElem -> void
(define (elem-print x . args)
  (apply display (elem->string x) args))

;; print-elem-padded-to : DataElem Nat -> void
(define (print-elem-padded-to x w . args)
  (apply display (make-string (max 0 (- w (elem-written-width x))) #\space) args)
  (apply elem-print x args))

;; print-matrix-csv/normalized : DataMatrix -> void
;; prints dm in comma separated format with spacing to make columns line up.
(define (print-matrix-csv/normalized dm)
  (define (matrix-row dm i) (list-ref dm i))
  (define (matrix-col dm j) 
    (define (jth-of-row row) (if (< j (length row)) (list-ref row j) #f))
    (map jth-of-row dm))
  (define (matrix-row-count dm) (length dm))
  (define (matrix-col-count dm) (apply max (map length dm)))
  (let* ((num-cols (matrix-col-count dm))
         (col-widths 
          (build-list num-cols
                      (lambda (j) 
                        (let* ((col (matrix-col dm j))
                               (elem-widths (map elem-written-width col)))
                          (apply max elem-widths))))))
    (for-each 
     (lambda (row) 
       (for-each (lambda (col-elem elem-width) 
                   (print-elem-padded-to col-elem elem-width)
                   (display ", "))
                 row col-widths)
       (newline))
     dm)))

(define (process-and-print-log f)
  (let ((processed (process-log f)))
    (print-matrix-csv/normalized (list (car processed)))
    (print-matrix-csv/normalized (cdr processed))))

;; extract-sublist : Listof[Any] Symbol -> Maybe[Listof[Any]]
;; extracts all non-symbol values immediately following k in l, or #f if none
(define extract-sublist 
  (let ()
    (define (take-nonsymbols l*)
      (let loop ((l* l*))
        (cond ((null? l*) '())
              (else (cond ((symbol? (car l*)) '())
                          (else (cons (car l*) 
                                      (loop (cdr l*)))))))))
    (lambda (l k)
      (let loop ((l l))
        (cond 
         ((null? l) #f)
         (else (cond ((eq? (car l) k)
                      (take-nonsymbols (cdr l)))
                     (else 
                      (loop (cdr l))))))))))

;; An Extractor is the *intersection* of:
;; - (        -> Maybe[Dataset])
;; - (String  -> Maybe[Dataset])
;; - (Dataset -> Maybe[Dataset])
;;
;; Extractors are for easy key lookup (in the stats) at Larceny REPL.
;; With no args, queries host; o/w stats source inferred (see below).

;; key->extractor : Symbol -> Extractor
;; Builds an extractor for key.
(define (key->extractor key)
  (lambda args
    (let ((stats (cond ((null? args)         (stats-read))
                       ((string? (car args)) (call-with-input-file (car args) read))
                       (else                 (car args)))))
      (extract-entry stats key))))

(define extract-histograms
  (key->extractor 'histograms))

(define extract-mmu 
  (key->extractor 'gc_mmu_log_t))

(define extract-gc-memstats
  (key->extractor 'gc_memstat_t))

(define extract-gclib-memstats
  (key->extractor 'gclib_memstat_t))

(define extract-gc-event-memstats
  (key->extractor 'gc_event_memstat_t))

(define extract-histograms
  (key->extractor 'histograms))

;; extract-gc-event-memstats : Extractor
;; pulls out global stats data (ie, data not per-generation/per-region)
(define extract-gc-general-memstats
  (lambda args
    (let ((gcm (apply extract-gc-memstats args))
          (gcl (apply extract-gclib-memstats args))
          (gce (apply extract-gc-event-memstats args)))
      (list->vector (apply append (map vector->list (list gcm gcl gce)))))))

;; render-mmu : Sexp -> Listof[(list Nat Real0..1)]
;; render-mmu :      -> Listof[(list Nat Real0..1)]
;; Produces min. mutator data as several (list window-size percentage) entries.
(define (render-mmu . args)
  (let* ((mmu-entry
          (or (if (null? args) (extract-mmu) (car args))
              (error 'render-mmu 
                     "must pass mmu or run larceny with mmu enabled.")))
         (extract-size    (key->extractor 'size))
         (extract-mutator (key->extractor 'mutator))
         (extract-min     (key->extractor 'min))
         (extract-real    (key->extractor 'real))
         (windows (cdr (vector->list mmu-entry)))
         (mutator-windows
          (map (lambda (window) (list (extract-size window)
                                      (extract-mutator window)))
               windows))
         (min-mutator-windows 
          (map (lambda (mut-window) 
                 (list (extract-size mut-window)
                       (extract-min (cdr (extract-mutator mut-window)))))
               mutator-windows))
         (elapsed-min-mutator-windows ;; Listof[(list Number Number)]
          (map (lambda (mmu-window)
                 (list (cadr (extract-size mmu-window))
                       (cadr (extract-real (cdr (extract-min mmu-window))))))
               min-mutator-windows))
         (percentage-mmus
          (map (lambda (window-size-and-mmu)
                 (let ((size (car window-size-and-mmu))
                       (mmu  (cadr window-size-and-mmu)))
                   (list size (/ (inexact mmu) (inexact size)))))
               elapsed-min-mutator-windows))
         )
    percentage-mmus))

;; render-max-mem : Sexp -> Listof[(list Symbol Nat)]
;; render-max-mem :      -> Listof[(list Symbol Nat)]
;; Produces max mem usage data as (list memory-type word-count) entries.
(define (render-max-mem . args)
  (let* ((memstat-v (if (null? args) (extract-gc-general-memstats) (car args)))
         (memstats (vector->list memstat-v)))
    (map (lambda (stats-name-and-memory-type)
           (let* ((stats-name (car stats-name-and-memory-type))
                  (memory-type (cadr stats-name-and-memory-type))
                  (elems (extract-sublist memstats stats-name)))
             (list memory-type
                   (cond
                    ((not elems) #f)
                    ((= 1 (length elems)) (car elems))
                    ((and (= 2 (length elems))
                          (zero? (car elems))) (cadr elems))
                    (else (error 'render-max-mem 
                                 "no support for nonzero hi word yet."))))))
          '((mem_allocated_max      total)
            (heap_allocated_max     heap)
            (remset_allocated_max   remsets)
            (summ_allocated_max     summaries)
            (smircy_allocated_max   markstate)
            (rts_allocated_max      runtime)
            (heap_fragmentation_max waste)))))
