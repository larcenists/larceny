; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File I/O.

($$trace "fileio")

(define *files-open* #f)

(define (file-io/initialize)
  (set! *files-open* #f)
  #t)

(define (file-io/finalize)
  (if *files-open*
      (file-io/close-open-files))
  #t)

(define (file-io/remember p)
  (if (output-port? p)
      (set! *files-open* #t)))

; Actually closes all open ports, because custom output ports use
; the same mechanism to ensure they're flushed and closed on exit.
;
; FIXME:  If we're going to close all open ports, it would be
; better just to keep a list of them and avoid using sro.

(define (file-io/close-open-files)
  (let ((ports (sro sys$tag.vector-tag sys$tag.port-typetag -1)))
    (set! *files-open* #f)
    (vector-for-each io/close-port ports)))

(define (file-io/data fd name)
  (cons fd name))

(define (file-io/fd datum)
  (car datum))

(define (file-io/name datum)
  (cdr datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-io/ioproc op)
  (case op
    ((read)
     file-io/read)
    ((write)
     file-io/write)
    ((close)
     file-io/close)
    ((ready?)
     file-io/ready?)
    ((name)
     file-io/name)
    ((set-position!)
     file-io/set-position!)
    (else 
     (error "file-io/ioproc: illegal operation: " op))))

(define (file-io/read data buffer)
  (file-io/read-bytes (file-io/fd data) buffer))

(define (file-io/write data buffer count)
  (file-io/write-bytes (file-io/fd data) buffer count 0))

(define (file-io/close data)
  (file-io/close-file data))

(define (file-io/ready? data) #t)

; Parameters for osdep/lseek-file; the magic numbers are portable
; because they're interpreted by Rts/Sys/osdep-*.c

(define whence:seek-set          0)     ; offset is absolute
(define whence:seek-cur          1)     ; offset is relative to current
(define whence:seek-end          2)     ; offset is relative to end

(define (file-io/port-position-as-binary data)
  (let ((r (osdep/lseek-file (file-io/fd data) 0 whence:seek-cur)))
    (if (>= r 0) r 'error)))

(define (file-io/set-position! data offset)
  (let ((r (osdep/lseek-file (file-io/fd data) offset whence:seek-set)))
    (if (>= r 0) 'ok 'error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: using the alist for this, but not for set-position!, is screwy.

(define (file-io/install-port-position-as-binary! p data)
  (let ((get-position
         (lambda ()
           (file-io/port-position-as-binary data))))
    (io/port-alist-set! p
                        (cons (cons 'port-position-in-bytes get-position)
                              (io/port-alist p)))))

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
               (p    (if (eq? 'binary tx-mode)
                         (io/make-port file-io/ioproc data io-mode tx-mode
                                       'set-position!)
                         (io/make-port file-io/ioproc data io-mode tx-mode))))
          (file-io/install-port-position-as-binary! p data)
          (file-io/remember p)
          p)
        (begin (error "Unable to open file " filename " for " io-mode)
               #t))))

; The R6RS says it's supposed to ignore the file options,
; and the buffer mode doesn't appear to have any real semantics
; for files.

(define (file-io/open-file-input-port filename options bufmode transcoder)
  (let* ((fd      (osdep/open-file filename 'input 'binary)))
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p    (io/make-port file-io/ioproc data 'input
                                   'binary 'set-position!))
               (p    (if (and transcoder (not (zero? transcoder)))
                         (io/transcoded-port p transcoder)
                         p)))
          (file-io/install-port-position-as-binary! p data)
          (file-io/remember p)
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
               (if (issue-deprecated-warnings?)
                   (let ((out (current-error-port)))
                     (display "WARNING: output file already exists: " out)
                     (display filename out)
                     (newline out))))
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
                 (p    (io/make-port file-io/ioproc data 'output
                                     'binary 'set-position! bufmode))
                 (p    (if (and transcoder (not (zero? transcoder)))
                           (io/transcoded-port p transcoder)
                           p)))
            (file-io/install-port-position-as-binary! p data)
            (file-io/remember p)
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
               (if (issue-deprecated-warnings?)
                   (let ((out (current-error-port)))
                     (display "WARNING: output file already exists: " out)
                     (display filename out)
                     (newline out))))
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
                         (make-bytevector 0)))
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
    (if (< r 0) 
        'error
        'ok)))

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
