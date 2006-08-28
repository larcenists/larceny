;;; This is an embarressing hack to work around the lack of a
;;; YouWantItWhen style module system in Larceny.  
;;;
;;; You use compile-file/with-syntax to pull syntax
;;; in from other files before compiling the source code.
;;; This is useful for compiling the SRFI's and Sassy.

;; compile-file/with-syntax : Filename [Listof Filename] . Filename -> void
;; Pulls all of the syntax definitions out of the Filename list before
;; compiling the source file.  The usual-syntactic-environment 
;; is unchanged at the end of the process.
(define (compile-file/with-syntax file syntax-files . rest)
  (define (expand-usual! e)
    (twobit-expand e usual-syntactic-environment))
  
  (define (expand-usual-all! p)
    (let loop ()
      (let ((x (read p)))
	(cond ((eof-object? x) (unspecified))
	      (else (expand-usual! x)
		    (loop))))))
    
  ;; load-for-syntax/hack : String -> void
  ;; modifies: usual-syntactic-environment
  (define (load-for-syntax/hack file)
    (expand-usual-all! (open-input-file file)))

  (let ((copy  (the-usual-syntactic-environment 'copy))
	(saved usual-syntactic-environment))
    (dynamic-wind 
      (lambda () (set! usual-syntactic-environment copy))
      (lambda () 
	(for-each load-for-syntax/hack syntax-files)
	(apply compile-file file rest))
      (lambda () (set! usual-syntactic-environment saved)))))
