; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File I/O.

($$trace "fileio")

; List of open files: assoc list on (file-data . port)

(define *file-io/files* '())

(define (file-io/initialize)
  (call-without-interrupts               ; *file-io/files*
   (lambda ()
     (set! *file-io/files* '()))))

(define (file-io/remember-file data p)
  (call-without-interrupts               ; *file-io/files*
   (lambda ()
     (set! *file-io/files* 
           (cons (cons data p) *file-io/files*)))))

(define (file-io/forget-file data)
  (call-without-interrupts               ; *file-io/files*
   (lambda ()
     (set! *file-io/files* 
           (remq! (assq data *file-io/files*) *file-io/files*)))))

(define (file-io/ioproc op)
  (case op
    ((read)
     (lambda (data buffer)
       (file-io/read-bytes (file-io/fd data) buffer)))
    ((write)
     (lambda (data buffer count)
       (file-io/write-bytes (file-io/fd data) buffer count 0)))
    ((close)
     (lambda (data)
       (file-io/close-file data)))
    ((ready?)
     (lambda (data) #t))
    ((name) 
     (lambda (data)
       (file-io/name data)))
    (else 
     (error "file-io/ioproc: illegal operation: " op))))

(define (file-io/data fd name)
  (cons fd name))

(define (file-io/fd datum)
  (car datum))

(define (file-io/name datum)
  (cdr datum))

(define (file-io/read-bytes fd buffer)
  (let ((r (osdep/read-file fd buffer (bytevector-like-length buffer))))
    (cond ((not (fixnum? r)) 'error)
          ((< r 0) 'error)
          ((= r 0) 'eof)
          (else r))))

(define (file-io/write-bytes fd buffer n offset)
  (let ((k (osdep/write-file4 fd buffer n offset)))
    (cond ((not (fixnum? k)) 'error)
          ((<= k 0) 'error)
          ((= k n)  'ok)
          (else (file-io/write-bytes fd buffer (- n k) (+ offset k))))))

(define (file-io/open-file filename . modes)
  (let* ((io-mode (if (memq 'input modes) 'input 'output))
         (tx-mode (if (memq 'binary modes) 'binary 'text))
         (fd      (osdep/open-file filename io-mode tx-mode))) 
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p    (io/make-port file-io/ioproc data io-mode tx-mode)))
          (file-io/remember-file data p)
          p)
        (begin (error "Unable to open file " filename " for " io-mode)
               #t))))

; FIXME: ignores file options and buffer mode.
; The R6RS says it's supposed to ignore the file options anyway,
; and the buffer mode doesn't appear to have any real semantics
; for files.

(define (file-io/open-file-input-port filename options bufmode transcoder)
  (let* ((fd      (osdep/open-file filename 'input 'binary)))
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p    (io/make-port file-io/ioproc data 'input 'binary))
               (p    (if (and transcoder (not (zero? transcoder)))
                         (io/transcoded-port p transcoder)
                         p)))
          (file-io/remember-file data p)
          p)
        (begin (error 'open-file-input-port "unable to open file" filename)
               #t))))

(define (file-io/open-file-output-port filename options bufmode transcoder)
  (let* ((opts (file-options->list options))
         (dont-create (memq 'no-create opts))
         (dont-fail (memq 'no-fail opts))
         (dont-truncate (memq 'no-truncate opts))
         (bufmode (case bufmode
                   ((none) 'none)
                   ((line) 'line)
                   ((datum flush) 'datum)
                   (else 'block)))
         (exists? (file-io/file-exists? filename)))
    (cond ((and exists? (not dont-create) (not dont-fail))
           (let* ((exec-mode (assq 'execution-mode (system-features)))
                  (exec-mode (if exec-mode (cdr exec-mode) 'r5rs)))
             (case exec-mode
              ((r5rs) #t)
              ((err5rs)
               (if (issue-warnings)
                   (begin (display "WARNING: output file already exists: ")
                          (display filename)
                          (newline))))
              (else
               (raise-r6rs-exception
                (make-i/o-file-already-exists-error filename)
                'open-file-output-port
                (errmsg 'msg:fileexists)
                (list filename opts))))))
          ((and (not exists?) dont-create)
           (raise-r6rs-exception
            (make-i/o-file-does-not-exist-error filename)
            'open-file-output-port
            (errmsg 'msg:nosuchfile)
            (list filename opts))))
    (let ((fd (apply osdep/open-file filename 'output 'binary opts)))
      (if (>= fd 0)
          (let* ((data (file-io/data fd filename))
                 (p    (io/make-port file-io/ioproc data
                                     'output 'binary bufmode))
                 (p    (if (and transcoder (not (zero? transcoder)))
                           (io/transcoded-port p transcoder)
                           p)))
            (file-io/remember-file data p)
            p)
          (begin (error 'open-file-output-port "unable to open file" filename)
                 #t)))))

; FIXME:  This should be implemented better.

(define (file-io/open-file-input/output-port filename options bufmode t)
  (let* ((opts (file-options->list options))
         (dont-create (memq 'no-create opts))
         (dont-fail (memq 'no-fail opts))
         (dont-truncate (memq 'no-truncate opts))
         (bufmode (case bufmode
                   ((none) 'none)
                   ((line) 'line)
                   ((datum flush) 'datum)
                   (else 'block)))
         (exists? (file-io/file-exists? filename)))
    (cond ((and exists? (not dont-create) (not dont-fail))
           (let* ((exec-mode (assq 'execution-mode (system-features)))
                  (exec-mode (if exec-mode (cdr exec-mode) 'r5rs)))
             (case exec-mode
              ((r5rs) #t)
              ((err5rs)
               (if (issue-warnings)
                   (begin (display "WARNING: output file already exists: ")
                          (display filename)
                          (newline))))
              (else
               (raise-r6rs-exception
                (make-i/o-file-already-exists-error filename)
                'open-file-input/output-port
                (errmsg 'msg:fileexists)
                (list filename opts))))))
          ((not exists?)
           (raise-r6rs-exception
            (make-i/o-file-does-not-exist-error filename)
            'open-file-input/output-port
            (errmsg 'msg:nosuchfile)
            (list filename opts))))
    (let ((dir (current-directory)))
      (cond ((not t)
             (let* ((initial-contents
                     (call-with-port
                      (open-file-input-port filename)
                      get-bytevector-all))
                    (initial-contents
                     (if (bytevector? initial-contents)
                         initial-contents
                         '#vu8()))
                    (bvport (open-input/output-bytevector initial-contents))
                    (show
                     (lambda ()
                       '(display " ")
                       '(write (vector-like-ref bvport 7))
                       '(newline)))
                    (read-method
                     (lambda (bv start count)
                       '(write (list 'reading start count))
                       (show)
                       (let ((r (get-bytevector-n! bvport bv start count)))
                         (if (eof-object? r) 0 r))))
                    (write-method
                     (lambda (bv start count)
                       '(write (list 'writing start count))
                       (show)
                       (put-bytevector bvport bv start count)
                       count))
                    (get-position-method
                     (lambda () (port-position bvport)))
                    (set-position-method
                     (lambda (posn) (set-port-position! bvport posn)))
                    (close-method
                     (lambda ()
                       (let* ((final-contents (get-output-bytevector bvport))
                              (current-dir (current-directory)))
                         (dynamic-wind
                          (lambda () (current-directory dir))
                          (lambda ()
                            (call-with-port
                             (open-file-output-port filename options bufmode)
                             (lambda (out)
                               (put-bytevector out final-contents))))
                          (lambda () (current-directory current-dir)))))))
               (make-custom-binary-input/output-port
                filename
                read-method write-method
                get-position-method set-position-method close-method)))
            ((eq? (transcoder-codec t) 'latin-1)
             (transcoded-port
              (file-io/open-file-input/output-port filename options bufmode #f)
              t))
            (else
             (assertion-violation
              'open-file-input/output-port
              "illegal codec" t))))))

(define (file-io/close-file data)
  (let ((r (osdep/close-file (file-io/fd data))))
    (file-io/forget-file data)
    (if (< r 0) 
        'error
        'ok)))

(define (file-io/close-open-files)
  (let ((files (call-without-interrupts    ; *file-io/files*
                (lambda ()
                  (let ((x *file-io/files*))
                    (set! *file-io/files* '())
                    x)))))
    (do ((l files (cdr l)))
        ((null? l) (unspecified))
      (io/close-port (cdar l)))))

(define (file-io/file-modification-time filename)
  (osdep/file-modification-time filename))

(define (file-io/file-exists? filename)
  (osdep/file-exists? filename))

(define (file-io/relative-path-string? filename)
  (osdep/relative-path-string? filename))

(define (file-io/absolute-path-string? filename)
  (osdep/absolute-path-string? filename))

(define (file-io/rename-file from to)
  (osdep/rename-file from to))

(define (file-io/delete-file filename)
  (osdep/delete-file filename))

; eof
