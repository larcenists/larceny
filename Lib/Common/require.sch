;; Implementation of REQUIRE, for loading libraries.

;; require/resolve-ext: String x (List-of String) -> String+{#f}
;; Given a base name and a list of extentions, return the first
;; name that exists, or #f if none do.
(define (require/resolve-ext base exts)
  (if (null? exts) #f
    (let ((src (string-append base "." (car exts))))
      (if (file-exists? src) src
        (require/resolve-ext base (cdr exts))))))

;; require/resolve-dir: String x String x Boolean -> String+{#f}
;; Given the name of a module and a directory name, resolves that into
;; either an absolute path to an existing file, or returns #f.  If
;; noisy, it complains when loading source because the fasl is stale.
(define (require/resolve-dir name dir noisy)
  (define base (if (absolute-path-string? dir)
                 (string-append dir "/" name)
                 (string-append (current-larceny-root) "/" dir "/" name)))
  (define (warn src)
    (if noisy
      (format #t "Warning: loading source in favor of stale fasl file: ~a~%"
              src))
    src)
  (if (file-exists? base) base
    (let ((src (require/resolve-ext base '("sch" "scm" "ss")))
          (fasl (require/resolve-ext base '("fasl"))))
      (if (and fasl src)
        (if (file-newer? fasl src) fasl (warn src))
        (or fasl src)))))

;; require/resolve: String+Symbol x [Boolean] -> String+{#f}
;; Given the name of a library, return a filename if found, or #f otherwise.
(define (require/resolve name . quiet)
  (let ((name (cond
                ((string? name) name)
                ((symbol? name) (symbol->string name))
                (else (error "Bad argument to resolver: " name))))
        (noisy (or (null? quiet) (not (car quiet)))))
    (let loop ((path (current-require-path)))
      (if (null? path) #f
        (or (require/resolve-dir name (car path) noisy)
            (loop (cdr path)))))))

(define *require-loaded-files* '())

;; require: String+Symbol -> Boolean
(define (require name)
  (cond
    (((current-library-resolver) name)
        => (lambda (file)
             (if (member file *require-loaded-files*)
               #f
               (begin
                 (set! *require-loaded-files*
                   (cons file *require-loaded-files*))
                 (load file)
                 #t))))
    (else
      (error "Could not locate library: " name))))

(define current-library-resolver
  (make-parameter "current-library-resolver" require/resolve))

(define current-larceny-root
  (make-env-parameter "LARCENY_ROOT"))

(define current-require-path
  (make-parameter "current-require-path" '()))
                 
