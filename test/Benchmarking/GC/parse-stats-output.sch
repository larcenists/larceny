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

;; extract-entry : Dataset Symbol -> Maybe[Entry]
(define (extract-entry s k)
  (let ((s (cond ((vector? s) (vector->list s))
                 ((list? s)   s))))
    (cond ((memf (lambda (e)
                   (or (and (vector? e)
                            (eq? k (vector-ref e 0)))
                       (and (pair? e)
                            (eq? k (car e)))))
                 s)
           => car)
          (else #f))))

;; An Extractor is the *intersection* of:
;; - (        -> Maybe[Dataset])
;; - (String  -> Maybe[Dataset])
;; - (Dataset -> Maybe[Dataset])

;; key->extractor : Symbol -> Extractor
(define (key->extractor key)
  (lambda args
    (let ((stats (cond ((null? args)         (stats-read))
                       ((string? (car args)) (call-with-file (car args) read))
                       (else                 (car args)))))
      (extract-entry stats key))))

(define extract-histograms
  (key->extractor 'histograms))

(define extract-mmu 
  (key->extractor 'gc_mmu_log_t))

(define (render-mmu . args)
  (let* ((mmu-entry (if (null? args) (extract-mmu) (car args)))
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

