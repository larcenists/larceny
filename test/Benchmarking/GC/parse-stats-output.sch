(require "Experimental/temp-files")

;; entrypoints of note:
;; gather-statsfile : Listof[FilenameString] -> Sexp
;; 
;; stats-read : -> Sexp

'(define stats-data 
  (let* ((pat "logs.Argus/*thesis10-log.*Feb1{6,7}*.log")
         (files (failsafe-list-directory pat))
         (files (sort files string<=?)))
    (gather-statsfiles files)))
'(define filename+command-description
   (map (lambda (sd) (list (extract-path sd '(filename)) 
                           (extract-path sd '(command-description))))
        stats-data))

;; alternative construction once you know what filenames and "keys"
;; (e.g. GC configuration names) you want in the data set.
'(define stats-data
   (apply gather-statsfiles (unzip filename+keys-feb17-10)))

;; A RtcfgKey is a Symbol (e.g. for a GC configuration key)
;; A BmarkKey is a Symbol (e.g. for a benchmark name
(define (plot-stats-data/stacked-bars dataset rt-keys bmark-keys rt+bmark->dataline)
  ...)

(define filename+keys-feb17-00
  '(("logs.Argus/bench-thesis10-log.2010Feb17-at-00-26-48.log" dflt)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-00-35-55.log" dflt-nurs1meg)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-00-45-02.log" scpy)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-00-55-14.log" rrof-nurs1meg-rgn4meg-sumz221-pop8-infm1-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-01-13-27.log" rrof-nurs1meg-rgn4meg-sumz1~2-pop6-infm1-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-01-34-35.log" rrof-nurs1meg-rgn4meg-sumz232-pop4-infm1-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-01-58-03.log" rrof-nurs1meg-rgn4meg-sumz221-pop8-infm0-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-02-22-18.log" rrof-nurs1meg-rgn4meg-sumz1~2-pop6-infm0-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-02-52-18.log" rrof-nurs1meg-rgn4meg-sumz232-pop4-infm0-refn1.0)))

(define filename+keys-feb17-10
  '(("logs.Argus/bench-thesis10-log.2010Feb17-at-10-29-59.log" dflt)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-10-39-05.log" dflt-nurs1meg)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-10-48-12.log" scpy)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-10-58-24.log" rrof-nurs1meg-rgn4meg-sumz221-pop8-infm1-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-11-16-45.log" rrof-nurs1meg-rgn4meg-sumz1~2-pop6-infm1-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-11-37-56.log" rrof-nurs1meg-rgn4meg-sumz232-pop4-infm1-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-12-01-16.log" rrof-nurs1meg-rgn4meg-sumz221-pop8-infm0-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-12-25-51.log" rrof-nurs1meg-rgn4meg-sumz1~2-pop6-infm0-refn1.0)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-12-56-07.log" rrof-nurs1meg-rgn4meg-sumz232-pop4-infm0-refn1.0)))

(define filename+keys-feb17-13
  '(("logs.Argus/bench-thesis10-log.2010Feb17-at-14-16-39.log" dflt-mmu)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-14-25-47.log" dflt-nurs1meg-mmu)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-14-34-55.log" scpy-mmu)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-14-45-06.log" rrof-nurs1meg-rgn4meg-sumz221-pop8-infm1-refn1.0-mmu)
    ("logs.Argus/bench-thesis10-log.2010Feb17-at-21-52-36.log" rrof-nurs1meg-rgn4meg-sumz1~2-pop6-infm1-refn1.0-mmu)
    ("logs.Argus/bench-thesis10-log.2010Feb18-at-07-37-54.log" rrof-nurs1meg-rgn4meg-sumz232-pop4-infm1-refn1.0-mmu)))

;; A Nelof[X] is one of:
;; -- (cons X '())
;; -- (cons X Nelof[X])

;; unzip : Neiof[(list X Y ...)] -> (list Nelof[X] Nelof[Y] ...)
;; e.g. (unzip '((1 2 3) (4 5 6) (7 8 9))) ==> ((1 4 7) (2 5 8) (3 6 9))
(define (unzip l)
  (let loop ((res (map list (car l)))
             (l (cdr l)))
    (if (null? l)
        (map reverse res)
        (loop (map cons (car l) res) (cdr l)))))

(define (failsafe-list-directory dir-pat)
  (let ((tmp-file (make-temporary-file)))
    (system (string-append "ls " dir-pat " > " tmp-file))
    (let ((listing
           (call-with-input-file tmp-file
             (lambda (in)
               (do ((x (read-line in) (read-line in))
                    (l '() (cons x l)))
                   ((eof-object? x) (reverse l)))))))
      (delete-file tmp-file)
      listing)))

;; gather-statsfiles : Listof[FilenameString] -> Sexp
;; gather-statsfiles : Listof[FilenameString] Listof[Symbol] -> Sexp
(define (gather-statsfiles filenames . opt-args)
  (define (p filename key) 
    (statsfile-fold 
     filename (lambda (cmd-desc date-and-time sexps)
                (list key 
                      `(filename            ,filename)
                      `(command-description ,cmd-desc)
                      `(date-and-time  ,date-and-time)
                      `(benchmarks             ,sexps)))))
  (let ((keys (if (not (null? opt-args)) 
                  (car opt-args)
                  (map string->symbol filenames))))
    (map p filenames keys)))


(define (stats-read)
  (let ((f (make-temporary-file "larcenystats~a")))
    (stats-dump-on f)
    (collect) ;; to ensure that (at least) one gc stats result ends up in file
    (stats-dump-off)
    (let ((v (call-with-input-file f read)))
      (delete-file f)
      v)))

;; statsfile-fold : FilenameString (String String Listof[Sexp] -> X) -> X
(define (statsfile-fold filename p)
  (call-with-input-file filename
    (lambda (in)
      (let* ((line-0 (read in))
             (line-1 (read in))
             (sexps  (do ((x (read in) (read in))
                          (l '() (cons x l)))
                         ((eof-object? x) (reverse l)))))
        (p line-0 line-1 sexps)))))

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
;; e.g.
'(extract-path '(z (a (b 3) (c 4)) (d (b 5))) '(d))   ; ==> (d (b 5))
'(extract-path '(z (a (b 3) (c 4)) (d (b 5))) '(b))   ; ==> ((b 3) (b 5))
'(extract-path '(z (a (b 3) (c 4)) (d (b 5))) '(a b)) ; ==> (b 3)
'(extract-path '(z (a (b 3) (c 4)) (d (b 5))) '(a c)) ; ==> (c 4)

  (define (extract-path.combine a d)
    (cond ((and (not a) (not d)) #f)
          ((not d) a)
          ((not a) d)
          ((and (pair? d) (symbol? (car d)))
           (cons a (cons d '())))
          (else (cons a d))))

  (define (extract-path.extract s k)
    (let extract ((s s) (k k))
      (cond ((pair? s)
             (if (eq? k (car s))
                 s
                 (let ((a (extract (car s) k))
                       (d (extract (cdr s) k)))
                   (extract-path.combine a d))))
          ((vector? s)
           (let ((r (extract (vector->list s) k)))
             (if (list? r)
                 (list->vector r)
                 r)))
          (else
           #f))))

(define (extract-path s p)

  (let* ((extract extract-path.extract)
         (entry (extract s (car p))))
    (cond ((not entry) 
           (if (list? s) 
               (let* ((results (map (lambda (x) (extract-path x p)) s))
                      (results (filter values results)))
                 (cond ((null? results) #f)
                       ((null? (cdr results)) (car results))
                       (else results)))
               #f))
          ((null? (cdr p)) entry)
          (else 
           (extract-path.combine (extract-path (car entry) (cdr p))
                                 (extract-path (cdr entry) (cdr p))
                                 )))))

;; first-satisfying : (Any -> Boolean : X) Sexp -> Maybe[X]
(define (first-satisfying pred? sexp)
  (let first ((sexp sexp))
    (cond
     ((pred? sexp) sexp)
     ((pair? sexp) (or (first (car sexp)) (first (cdr sexp))))
     ((vector? sexp) (first (vector->list sexp)))
     (else #f))))

;; first-number : Sexp -> Maybe[Number]
(define (first-number sexp) 
  (first-satisfying number? sexp))

;; first-string : Sexp -> Maybe[String]
(define (first-string sexp)
  (first-satisfying string? sexp))

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
