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
