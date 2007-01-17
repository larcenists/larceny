; Copyright 2006 Felix S Klock
;
; $Id: twobit-heap.sch 2916 2006-04-27 21:25:00Z tov $
;
; Load script for building a heap image with all compiler names exposed.
;
; Before you use this script, you must compile the development environment
; and debugger.  The easiest way to do that is to run 'build-twobit' in
; the standard development environment.

(load "setup.sch")
(setup 'sassy)
(load-compiler)

(compat:load (param-filename 'debugger "debug.fasl"))
(compat:load (param-filename 'debugger "inspect-cont.fasl"))
(compat:load (param-filename 'debugger "trace.fasl"))
(install-debugger)

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

(let ()
  (define (file->char-list port)
    (do ((c (read-char port) (read-char port))
         (l '() (cons c l)))
        ((eof-object? c) (reverse l))))
  (define (trim-leading-spaces char-list)
    (let loop ((l char-list))
      (if (char-whitespace? (car l))
          (loop (cdr l))
          l)))
  (define (trim-trailing-spaces char-list)
    (reverse (trim-leading-spaces (reverse char-list))))
  (define (trim-spaces char-list)
    (trim-trailing-spaces
     (trim-leading-spaces char-list)))
  (define date-cmd
    (if (equal? (cdr (assq 'os-name (system-features)))
                "Win32")
        "date /t "
        "date    "))
  (let ((herald-string
         (begin
           ;; A "temporary" file (we know it is about to get overwritten).
           (system (string-append date-cmd "> twobit.heap"))
           (call-with-input-file "twobit.heap"
             (lambda (port)
               (string-append 
                "twobit.heap, built on " 
                (list->string (trim-spaces (file->char-list port)))))))))
    (herald herald-string)))

(dump-interactive-heap "twobit.heap")
(cond
 ((equal? (cdr (assq 'os-name (system-features)))
	  "Win32")
  (system "larceny.bin -reorganize-and-dump -heap twobit.heap")
  (system "move twobit.heap.split twobit.heap"))
 (else
  (system "./larceny.bin -reorganize-and-dump -heap twobit.heap")
  (system "/bin/mv twobit.heap.split twobit.heap")))

; eof
