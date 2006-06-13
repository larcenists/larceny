;;; This is an embarressing hack to work around the lack of a
;;; YouWantItWhen style module system in Larceny.  
;;;
;;; You use compile-file/requiring to pull syntax
;;; in from other files before compiling the source code.
;;; This is useful for compiling the SRFI's and Sassy.

;; compile-file/requiring : Filename [Listof RequireSpec] . Filename -> void
;; Pulls all of the syntax definitions out of the require specs before
;; compiling the source file.  The usual-syntactic-environment 
;; is unchanged at the end of the process.
(define compile-file/requiring)

(let ()
  ;; require-for-syntax/hack : [Oneof String Symbol] -> void
  ;; modifies: usual-syntactic-environment
  (define (require-for-syntax/hack name)
    (define (expand-usual! e)
      (twobit-expand e usual-syntactic-environment))
    
    (define (expand-usual-all! p)
      (let loop ()
        (let ((x (read p)))
          (cond ((eof-object? x) (unspecified))
                (else (expand-usual! x)
                      (loop))))))
    
    (let ((file ((current-library-resolver) name)))
      (expand-usual-all! (open-input-file file))))
  
  (set! compile-file/requiring 
        (lambda (file requires . rest)
          (let ((usual-syntactic-environment/orig
                 (the-usual-syntactic-environment 'copy))
                (use (the-usual-syntactic-environment)))
            (dynamic-wind 
                (lambda () (set! usual-syntactic-environment use))
                (lambda () 
                  (for-each require-for-syntax/hack requires)
                  (apply compile-file file rest))
                (lambda () (set! usual-syntactic-environment
                                 usual-syntactic-environment/orig))))))
)
