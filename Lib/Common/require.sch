;; Implementation of REQUIRE, for loading libraries.

(define current-larceny-root
  (make-env-parameter "LARCENY_ROOT"))

(define current-require-path
  (make-parameter "current-require-path"
                  (list "Lib/SRFI")))

;;; (define (string-read s)
;;;   (let* ((port (open-input-string s))
;;;          (res  (read port)))
;;;     (close-input-port port)
;;;     res))
;;; 
;;; (define (string-write obj)
;;;   (format #t "~s" obj))
;;; 
;;; (define (make-parsed-env-parameter name)
;;;   (let ((param (make-env-parameter name)))
;;;     (lambda value
;;;       ;(let ((result 
;;;               (apply param (map string-write value))))
;;;         ;(if (string? result)
;;;           ;(string-read result)
;;;           ;result)))))
;;; )
;;; 
;;; 
;;; ;(define current-scheme-extensions
;;;   ;(make-parameter (list "sch" "scm" "ss")))
;;; 
;;; ;(define current-library-resolver
;;;   ;(make-parameter #f))
;;; 
;;; (define current-library-path
;;;   (make-parsed-env-parameter "LARCENY_PATH"))
;;; 
;;; (define (resolve spec . maybe-path)
;;;   (let ((path (cond
;;;                 ((null? maybe-path)       (current-library-path))
;;;                 ((null? (cdr maybe-path)) (car maybe-path))
;;;                 (error "Not a singleton list: " maybe-path))))
;;;     path))
