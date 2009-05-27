;; make-temporary-file :                     -> PathString
;; make-temporary-file : FmtString           -> PathString
;; make-temporary-file : FmtString DirString -> PathString
(define (make-temporary-file . args)
  (let* ((args-ref (lambda (i default)
                     (if (< i (length args))
                         (list-ref args i)
                         (default))))
         (base-name (args-ref 0 (lambda () "larcenytmp~a")))
         (dir (args-ref 1 (lambda () 
                            (if (member '(os-name . "Win32") (system-features))
                                "C:/Temp/"
                                "/tmp/"))))
         (fmt (lambda (b n)
                (call-with-string-output-port
                 (lambda (p) (format p b n))))))
    (let loop ((n 0) (base-name base-name))
      (let* ((target-name (fmt base-name n))
             (target-path (string-append dir "/" target-name)))
        (cond ((file-exists? target-path)
               (loop (+ n 1)
                     (if (string=? (fmt base-name 0) (fmt base-name 1))
                         ;; last minute fixup to base-name to ensure
                         ;; distinct names
                         (string-append base-name "~a")
                         base-name)))
              (else
               (system (string-append "touch " target-path))
               target-path))))))

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
;; - Vectorof[Entry]
;; - Listof[Entry]

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
;; - (       -> Maybe[Dataset])
;; - (String -> Maybe[Dataset])
;; - (Stats  -> Maybe[Dataset])

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
