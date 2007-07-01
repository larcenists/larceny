(load "html-entities.sch")
(load "html-parser.sch")
(load "datastore.sch")
(load "html-format.sch")

; Example: parse a site and print out paths for all IMG tags seen
; along the way.  In a more sophisticated setting (!) the handler
; would start downloads of these images by opening content streams
; and threads to read them and decode their contents; the element
; can be side-effected to store references to those threads or
; streams.
;
; FUTUREs may be the thing for some of this.

(define *verbose* #f)

(define (get-linked-content elt)
  (if *verbose*
      (let ((probe (assq 'src (cadr elt))))
	(if probe
	    (format #t "~a: ~a~%" (car elt) (cdr probe))))))

(putprop 'img 'html-reduce-handler get-linked-content)
(putprop 'style 'html-reduce-handler get-linked-content)
(putprop 'script 'html-reduce-handler get-linked-content)

(define (browse host object)
  (for-each html-format
	    (html-parser (open-content-stream host 80 object #t #f))))
