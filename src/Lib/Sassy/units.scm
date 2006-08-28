
(define sassy-linked-units (make-hash-table equal?))
(define sassy-linking-files (make-hash-table equal?))
(define sassy-loaded-sources (make-hash-table equal?))
(define sassy-code-sources (make-hash-table equal?))
(define sassy-dynamic-sources (make-hash-table equal?))
(define sassy-dependencies (make-hash-table equal?))
(define sassy-options (make-hash-table equal?))

; I move these to extras.scm -Jon

; (define (file->sexp-list file)
;   (call-with-input-file file
;     (lambda (p)
;       (let lp ((res '()))
;         (let ((sexp (read p)))
;           (if (eof-object? sexp)
;             (reverse res)
;             (lp (cons sexp res))))))))

; (define (string-split str ch)
;   (let ((len (string-length str)))
;     (let lp ((from 0) (to 0) (res '()))
;       (define (collect)
;         (if (= from to) res (cons (substring str from to) res)))
;       (cond
;         ((>= to len)
;          (reverse (collect)))
;         ((eqv? ch (string-ref str to))
;          (lp (+ to 1) (+ to 1) (collect)))
;         (else
;          (lp from (+ to 1) res))))))

(define sassy-include-path
  (string-split (or (getenv "SASSY_INCLUDE_PATH")
                    ".:/usr/local/share/sassy")
                #\:))

(define (sassy-find-file file)
  (let lp ((ls sassy-include-path))
    (if (null? ls)
      (error "sassy: couldn't find file in path: " file sassy-include-path)
      (let ((path (string-append (car ls) "/" file)))
        (if (file-exists? path)
          path
          (lp (cdr ls)))))))

