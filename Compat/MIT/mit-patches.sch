; 17 May 1999 / lth
;
; $Id$
;
; Load this file into a running MIT Scheme to allow it to run
; Util/config.sch and Util/expander.sch.  (Note: MIT Scheme cannot
; be used to run Twobit, as it does not distinguish #f and the empty
; list.)

(load-option 'format)

(define delete-file
  (let ((delete-file delete-file))
    (lambda (x)
      (if (file-exists? x) (delete-file x)))))

(define gensym
  (lambda (key)
    (generate-uninterned-symbol key)))

(define eval 
  (let ((eval eval))
    (lambda (expr . rest)
      (if (null? rest)
	  (eval expr user-initial-environment)
	  (eval expr (car rest))))))

; eof

