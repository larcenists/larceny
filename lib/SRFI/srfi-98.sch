;;; SRFI 98: An interface to access environment variables.
;;;
;;; $Id$
;;;
;;; FIXME: may work only for Unix
;;; FIXME: requires write permission in /tmp

(define (get-environment-variable name)
  (getenv name))

(define (get-environment-variables)

  (define (tempname root)
    (define (loop i)
      (let ((filename (string-append root (number->string i 16))))
        (cond ((> i 500)
               #f)
              ((file-exists? filename)
               (loop (+ i 1)))
              (else
               filename))))
    (loop 0))

  (define (parse-parameter-names filename)
    (define (loop p params)
      (let ((s (get-line p)))
        (cond ((string? s)
               (let ((param (parse-line s)))
                 (loop p
                       (if param (cons param params) params))))
              (else
               (reverse params)))))
    (call-with-input-file
     filename
     (lambda (p) (loop p '()))))

  (define (parse-line s)
    (let ((x (memv #\= (string->list s))))
      (if x
          (substring s 0 (- (string-length s) (length x)))
          #f)))

  (let ((osname (cdr (assq 'os-name (system-features)))))
    (cond ((and #f (string=? osname "Win32"))                           ; FIXME
           '())
          (else
           (let* ((filename (tempname "/tmp/temporary."))
                  (r (system (string-append "printenv > " filename)))
                  (params (parse-parameter-names filename)))
             (delete-file filename)
             (if (= r 0)
                 (map (lambda (param) (cons param (getenv param)))
                      params)
                 '()))))))

; eof
