; Test suite for SRFI-29
; 2004-01-01 / lth
;
; $Id$

(import (rnrs base)
        (rnrs io simple)
        (srfi :29 localization))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define-syntax parameterize
  (syntax-rules ()
   ((_ ((parameter value)) form ...)
    (let ((current-value (parameter)))
      (dynamic-wind
       (lambda () (parameter value))
       (lambda () form ...)
       (lambda () (parameter current-value)))))))

(or (symbol? (current-language))
    (fail 'current-language:1))

(let ((language (if (eq? (current-language) 'en) 'fr 'en)))
  (parameterize ((current-language language))
    (or (eq? language (current-language))
        (fail 'current-language:2))))

(or (symbol? (current-country))
    (fail 'current-country:1))

(let ((country (if (eq? (current-country) 'us) 'ca 'us)))
  (parameterize ((current-country country))
    (or (eq? country (current-country))
        (fail 'current-country:2))))

(or (list? (current-locale-details))
    (fail 'current-locale-details:1))

(let ((translations
       '(((en) . ((time . "It's ~a, ~a.")
		  (goodbye . "Goodbye, ~a.")))
         ((fr) . ((time . "~1@*~a, c'est ~a.")
		  (goodbye . "Au revoir, ~a."))))))
  (for-each (lambda (translation)
              (let ((bundle-name (cons 'hello-program (car translation))))
                (if (not (load-bundle! bundle-name))
                    (begin
		      (declare-bundle! bundle-name (cdr translation))
		      (store-bundle! bundle-name)))))
	    translations))

(current-language 'en)
(or (not (localized-template 'hello-program 'foobar))
    (fail 'localized-template:1))

(define localized-message
  (lambda (message-name . args)
    (apply format (cons (localized-template 'hello-program message-name)
                        args))))

(let ((myname "Fred"))
  (or (equal? '("It's 12:00, Fred." "Goodbye, Fred.")
	      (parameterize ((current-language 'en))
		(list (localized-message 'time "12:00" myname)
		      (localized-message 'goodbye myname))))
      (fail 'english:1))

  (or (equal? '("Fred, c'est 12:00." "Au revoir, Fred.")
	      (parameterize ((current-language 'fr))
		(list (localized-message 'time "12:00" myname)
		      (localized-message 'goodbye myname))))
      (fail 'french:1)))

(writeln "Done.")
