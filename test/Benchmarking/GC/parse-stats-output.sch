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
    (collect)
    (stats-dump-off)
    (let ((v (call-with-input-file f read)))
      (delete-file f)
      v)))
