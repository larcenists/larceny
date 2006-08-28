; Test suite for SRFI-13
; 2004-01-02 / lth
;
; There are two third-party test suites in this file: the first was
; written by Martin Grabmueller for Guile and is covered by the GPL.
; The second was probably written by Shiro Kawai for Gauche and is
; covered by a BSD-style licence.
;
; The two suites are commented separately with their respective
; licenses.
;
; Following those are test written by me (lth@acm.org) to complete
; the suite.
;
; $Id$

(cond-expand (srfi-13))

(define total-errors 0)
(define total-tests 0)
(define test-prefix #f)

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (set! total-errors (+ total-errors 1))
  (if test-prefix
      (writeln "Error: test failed in " test-prefix ": ")
      (writeln "Error: test failed: "))
  (apply writeln "   " token "  " more)
  #f)

;;;; Test suite for SRFI-13 as implemented by Guile
;;;;
;;;; Martin Grabmueller, 2001-05-07
;;;;
;;;; Copyright (C) 2001 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA

;;;; From http://www.glug.org/snap/guile-core/test-suite/tests/srfi-13.test
;;;; Test dated 27 April 2002, downloaded 02 January 2004

(require 'common-syntax)		; FLUID-LET

(define-syntax with-test-prefix 
  (syntax-rules ()
    ((with-test-prefix pfix form ...)
     (fluid-let ((test-prefix 'pfix))
       form ...))))

(define-syntax pass-if
  (syntax-rules ()
    ((pass-if ?desc ?expr)
     (begin
       ;(writeln '?expr)
       (set! total-tests (+ total-tests 1))
       (let ((desc ?desc)
	     (expr ?expr))
	 (if (not expr)
	     (fail desc '?expr expr)))))
    ((pass-if ?desc ?expr ?test)
     (begin
       ;(writeln '?expr)
       (set! total-tests (+ total-tests 1))
       (let ((desc ?desc))
	 ?expr
	 (if (not ?test)
	     (fail desc '?expr)))))))

(with-test-prefix "string-any"

  (pass-if "no match"
    (not (string-any char-upper-case? "abcde")))

  (pass-if "one match"
    (string-any char-upper-case? "abCde"))

  (pass-if "more than one match"
    (string-any char-upper-case? "abCDE"))

  (pass-if "no match, start index"
    (not (string-any char-upper-case? "Abcde" 1)))

  (pass-if "one match, start index"
    (string-any char-upper-case? "abCde" 1))

  (pass-if "more than one match, start index"
    (string-any char-upper-case? "abCDE" 1))

  (pass-if "no match, start and end index"
    (not (string-any char-upper-case? "AbcdE" 1 4)))

  (pass-if "one match, start and end index"
    (string-any char-upper-case? "abCde" 1 4))

  (pass-if "more than one match, start and end index"
    (string-any char-upper-case? "abCDE" 1 4)))

(with-test-prefix "string-every"

  (pass-if "no match at all"
    (not (string-every char-upper-case? "abcde")))

  (pass-if "not all match"
    (not (string-every char-upper-case? "abCDE")))

  (pass-if "all match"
    (string-every char-upper-case? "ABCDE"))

  (pass-if "no match at all, start index"
    (not (string-every char-upper-case? "Abcde" 1)))

  (pass-if "not all match, start index"
    (not (string-every char-upper-case? "ABcde" 1)))

  (pass-if "all match, start index"
    (string-every char-upper-case? "aBCDE" 1))

  (pass-if "no match at all, start and end index"
    (not (string-every char-upper-case? "AbcdE" 1 4)))

  (pass-if "not all match, start and end index"
    (not (string-every char-upper-case? "ABcde" 1 4)))

  (pass-if "all match, start and end index"
    (string-every char-upper-case? "aBCDe" 1 4)))

(with-test-prefix "string-tabulate"

  (pass-if "static fill-char"
    (string=? (string-tabulate (lambda (idx) #\!) 10) "!!!!!!!!!!"))

  (pass-if "variable fill-char"
    (string=? (string-tabulate
	       (lambda (idx) (integer->char (+ idx 32))) 10) " !\"#$%&'()")))

(with-test-prefix "string->list"

  (pass-if "empty"
     (zero? (length (string->list ""))))

  (pass-if "nonempty"
     (= (length (string->list "foo")) 3))

  (pass-if "empty, start index"
     (zero? (length (string->list "foo" 3 3))))

   (pass-if "nonempty, start index"
     (= (length (string->list "foo" 1 3)) 2))
  )

(with-test-prefix "reverse-list->string"

  (pass-if "empty"
     (string-null? (reverse-list->string '())))

  (pass-if "nonempty"
     (string=? "foo" (reverse-list->string '(#\o #\o #\f)))))


(with-test-prefix "string-join"

  (pass-if "empty list, no delimiter, implicit infix, empty 1"
     (string=? "" (string-join '())))

  (pass-if "empty string, no delimiter, implicit infix, empty 2"
     (string=? "" (string-join '(""))))

  (pass-if "non-empty, no delimiter, implicit infix"
     (string=? "bla" (string-join '("bla"))))

  (pass-if "empty list, implicit infix, empty 1"
     (string=? "" (string-join '() "|delim|")))

  (pass-if "empty string, implicit infix, empty 2"
     (string=? "" (string-join '("") "|delim|")))

  (pass-if "non-empty, implicit infix"
     (string=? "bla" (string-join '("bla") "|delim|")))

  (pass-if "non-empty, implicit infix"
     (string=? "bla" (string-join '("bla") "|delim|")))

  (pass-if "two strings, implicit infix"
     (string=? "bla|delim|fasel" (string-join '("bla" "fasel") "|delim|")))

  (pass-if "empty, explicit infix"
     (string=? "" (string-join '("") "|delim|" 'infix)))

  (pass-if "empty list, explicit infix"
     (string=? "" (string-join '() "|delim|" 'infix)))

  (pass-if "non-empty, explicit infix"
     (string=? "bla" (string-join '("bla") "|delim|" 'infix)))

  (pass-if "two strings, explicit infix"
     (string=? "bla|delim|fasel" (string-join '("bla" "fasel") "|delim|"
					      'infix)))

;   (pass-if-exception "empty list, strict infix"
;      exception:strict-infix-grammar
;      (string-join '() "|delim|" 'strict-infix))

  (pass-if "empty, strict infix"
     (string=? "" (string-join '("") "|delim|" 'strict-infix)))

  (pass-if "non-empty, strict infix"
     (string=? "foo" (string-join '("foo") "|delim|" 'strict-infix)))

  (pass-if "two strings, strict infix"
     (string=? "foo|delim|bar" (string-join '("foo" "bar") "|delim|"
					    'strict-infix)))

  (pass-if "empty list, prefix"
     (string=? "" (string-join '() "|delim|" 'prefix)))

  (pass-if "empty, prefix"
     (string=? "|delim|" (string-join '("") "|delim|" 'prefix)))

  (pass-if "non-empty, prefix"
     (string=? "|delim|foo" (string-join '("foo") "|delim|" 'prefix)))

  (pass-if "two strings, prefix"
     (string=? "|delim|foo|delim|bar" (string-join '("foo" "bar") "|delim|"
						   'prefix)))

  (pass-if "empty list, suffix"
     (string=? "" (string-join '() "|delim|" 'suffix)))

  (pass-if "empty, suffix"
     (string=? "|delim|" (string-join '("") "|delim|" 'suffix)))

  (pass-if "non-empty, suffix"
     (string=? "foo|delim|" (string-join '("foo") "|delim|" 'suffix)))

  (pass-if "two strings, suffix"
     (string=? "foo|delim|bar|delim|" (string-join '("foo" "bar") "|delim|"
						   'suffix))))

(with-test-prefix "string-copy"

  (pass-if "empty string"
    (string=? "" (string-copy "")))

  (pass-if "full string"
    (string=? "foo-bar" (string-copy "foo-bar")))

  (pass-if "start index"
    (string=? "o-bar" (string-copy "foo-bar" 2)))

  (pass-if "start and end index"
    (string=? "o-ba" (string-copy "foo-bar" 2 6)))
)

(with-test-prefix "substring/shared"

  (pass-if "empty string"
    (string=? "" (substring/shared "" 0)))

  (pass-if "non-empty string"
    (string=? "foo" (substring/shared "foo-bar" 0 3)))

  (pass-if "non-empty string, not eq?"
    (string=? "foo-bar" (substring/shared "foo-bar" 0 7))))

(with-test-prefix "string-copy!"

  (pass-if "non-empty string"
    (string=? "welld, oh yeah!"
	      (let* ((s "hello")
		     (t "world, oh yeah!"))
		(string-copy! t 1 s 1 3)
		t))))

(with-test-prefix "string-take"

  (pass-if "empty string"
    (string=? "" (string-take "foo bar braz" 0)))

  (pass-if "non-empty string"
    (string=? "foo " (string-take "foo bar braz" 4)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-take "foo bar braz" 12))))

(with-test-prefix "string-take-right"

  (pass-if "empty string"
    (string=? "" (string-take-right "foo bar braz" 0)))

  (pass-if "non-empty string"
    (string=? "braz" (string-take-right "foo bar braz" 4)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-take-right "foo bar braz" 12))))

(with-test-prefix "string-drop"

  (pass-if "empty string"
    (string=? "" (string-drop "foo bar braz" 12)))

  (pass-if "non-empty string"
    (string=? "braz" (string-drop "foo bar braz" 8)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-drop "foo bar braz" 0))))

(with-test-prefix "string-drop-right"

  (pass-if "empty string"
    (string=? "" (string-drop-right "foo bar braz" 12)))

  (pass-if "non-empty string"
    (string=? "foo " (string-drop-right "foo bar braz" 8)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-drop-right "foo bar braz" 0))))

(with-test-prefix "string-pad"

  (pass-if "empty string, zero pad"
    (string=? "" (string-pad "" 0)))

  (pass-if "empty string, zero pad, pad char"
    (string=? "" (string-pad "" 0)))

  (pass-if "empty pad string, 2 pad "
    (string=? "  " (string-pad "" 2)))

  (pass-if "empty pad string, 2 pad, pad char"
    (string=? "!!" (string-pad "" 2 #\!)))

  (pass-if "empty pad string, 2 pad, pad char, start index"
    (string=? "!c" (string-pad "abc" 2 #\! 2)))

  (pass-if "empty pad string, 2 pad, pad char, start and end index"
    (string=? "!c" (string-pad "abcd" 2 #\! 2 3)))

  (pass-if "freestyle 1"
    (string=? "32" (string-pad (number->string 532) 2 #\!)))

  (pass-if "freestyle 2"
    (string=? "!532" (string-pad (number->string 532) 4 #\!))))

(with-test-prefix "string-pad-right"

  (pass-if "empty string, zero pad"
    (string=? "" (string-pad-right "" 0)))

  (pass-if "empty string, zero pad, pad char"
    (string=? "" (string-pad-right "" 0)))

  (pass-if "empty pad string, 2 pad "
    (string=? "  " (string-pad-right "" 2)))

  (pass-if "empty pad string, 2 pad, pad char"
    (string=? "!!" (string-pad-right "" 2 #\!)))

  (pass-if "empty pad string, 2 pad, pad char, start index"
    (string=? "c!" (string-pad-right "abc" 2 #\! 2)))

  (pass-if "empty pad string, 2 pad, pad char, start and end index"
    (string=? "c!" (string-pad-right "abcd" 2 #\! 2 3)))

  (pass-if "freestyle 1"
    (string=? "53" (string-pad-right (number->string 532) 2 #\!)))

  (pass-if "freestyle 2"
    (string=? "532!" (string-pad-right (number->string 532) 4 #\!))))

(with-test-prefix "string-trim"

  (pass-if "empty string"
    (string=? "" (string-trim "")))

  (pass-if "no char/pred"
    (string=? "foo " (string-trim " \tfoo ")))

  (pass-if "start index, pred"
    (string=? "foo " (string-trim " \tfoo " char-whitespace? 1)))

  (pass-if "start and end index, pred"
    (string=? "f" (string-trim " \tfoo " char-whitespace? 1 3)))

  (pass-if "start index, char"
    (string=? "\tfoo " (string-trim " \tfoo " #\space 1)))

  (pass-if "start and end index, char"
    (string=? "\tf" (string-trim " \tfoo " #\space 1 3)))

  (pass-if "start index, charset"
    (string=? "foo " (string-trim " \tfoo " char-set:whitespace 1)))

  (pass-if "start and end index, charset"
    (string=? "f" (string-trim " \tfoo " char-set:whitespace 1 3))))

(with-test-prefix "string-trim-right"

  (pass-if "empty string"
    (string=? "" (string-trim-right "")))

  (pass-if "no char/pred"
    (string=? " \tfoo" (string-trim-right " \tfoo ")))

  (pass-if "start index, pred"
    (string=? " \tfoo" (string-trim-right " \tfoo " char-whitespace? 1)))

  (pass-if "start and end index, pred"
    (string=? " \tf" (string-trim-right " \tfoo " char-whitespace? 1 3)))

  (pass-if "start index, char"
    (string=? " \tfoo" (string-trim-right " \tfoo " #\space 1)))

  (pass-if "start and end index, char"
    (string=? " \tf" (string-trim-right " \tfoo " #\space 1 3)))

  (pass-if "start index, charset"
    (string=? " \tfoo" (string-trim-right " \tfoo " char-set:whitespace 1)))

  (pass-if "start and end index, charset"
    (string=? " \tf" (string-trim-right " \tfoo " char-set:whitespace 1 3))))

(with-test-prefix "string-trim-both"

  (pass-if "empty string"
    (string=? "" (string-trim-both "")))

  (pass-if "no char/pred"
    (string=? "foo" (string-trim-both " \tfoo ")))

  (pass-if "start index, pred"
    (string=? "foo" (string-trim-both " \tfoo " char-whitespace? 1)))

  (pass-if "start and end index, pred"
    (string=? "f" (string-trim-both " \tfoo " char-whitespace? 1 3)))

  (pass-if "start index, char"
    (string=? "\tfoo" (string-trim-both " \tfoo " #\space 1)))

  (pass-if "start and end index, char"
    (string=? "\tf" (string-trim-both " \tfoo " #\space 1 3)))

  (pass-if "start index, charset"
    (string=? "foo" (string-trim-both " \tfoo " char-set:whitespace 1)))

  (pass-if "start and end index, charset"
    (string=? "f" (string-trim-both " \tfoo " char-set:whitespace 1 3))))

(define s0 (make-string 200 #\!))
(define s1 (make-string 0 #\!))

(with-test-prefix "string-fill!"

  (pass-if "empty string, no indices"
    (string-fill! s1 #\*)
    (= (string-length s1) 0))

  (pass-if "empty string, start index"
    (string-fill! s1 #\* 0)
    (= (string-length s1) 0))

  (pass-if "empty string, start and end index"
    (string-fill! s1 #\* 0 0)
    (= (string-length s1) 0))

  (pass-if "no indices"
    (string-fill! s0 #\*)
    (char=? (string-ref s0 0) #\*))

  (pass-if "start index"
    (string-fill! s0 #\+ 10)
    (char=? (string-ref s0 11) #\+))

  (pass-if "start and end index"
    (string-fill! s0 #\| 12 20)
    (char=? (string-ref s0 13) #\|)))

(with-test-prefix "string-prefix-length"

  (pass-if "empty prefix"
    (= 0 (string-prefix-length "" "foo bar")))

  (pass-if "non-empty prefix - match"
    (= 3 (string-prefix-length "foo" "foo bar")))

  (pass-if "non-empty prefix - no match"
    (= 0 (string-prefix-length "bar" "foo bar"))))

(with-test-prefix "string-prefix-length-ci"

  (pass-if "empty prefix"
    (= 0 (string-prefix-length-ci "" "foo bar")))

  (pass-if "non-empty prefix - match"
    (= 3 (string-prefix-length-ci "fOo" "foo bar")))

  (pass-if "non-empty prefix - no match"
    (= 0 (string-prefix-length-ci "bAr" "foo bar"))))

(with-test-prefix "string-suffix-length"

  (pass-if "empty suffix"
    (= 0 (string-suffix-length "" "foo bar")))

  (pass-if "non-empty suffix - match"
    (= 3 (string-suffix-length "bar" "foo bar")))

  (pass-if "non-empty suffix - no match"
    (= 0 (string-suffix-length "foo" "foo bar"))))

(with-test-prefix "string-suffix-length-ci"

  (pass-if "empty suffix"
    (= 0 (string-suffix-length-ci "" "foo bar")))

  (pass-if "non-empty suffix - match"
    (= 3 (string-suffix-length-ci "bAr" "foo bar")))

  (pass-if "non-empty suffix - no match"
    (= 0 (string-suffix-length-ci "fOo" "foo bar"))))

(with-test-prefix "string-prefix?"

  (pass-if "empty prefix"
    (string-prefix? "" "foo bar"))

  (pass-if "non-empty prefix - match"
    (string-prefix? "foo" "foo bar"))

  (pass-if "non-empty prefix - no match"
    (not (string-prefix? "bar" "foo bar"))))

(with-test-prefix "string-prefix-ci?"

  (pass-if "empty prefix"
    (string-prefix-ci? "" "foo bar"))

  (pass-if "non-empty prefix - match"
    (string-prefix-ci? "fOo" "foo bar"))

  (pass-if "non-empty prefix - no match"
    (not (string-prefix-ci? "bAr" "foo bar"))))

(with-test-prefix "string-suffix?"

  (pass-if "empty suffix"
    (string-suffix? "" "foo bar"))

  (pass-if "non-empty suffix - match"
    (string-suffix? "bar" "foo bar"))

  (pass-if "non-empty suffix - no match"
    (not (string-suffix? "foo" "foo bar"))))

(with-test-prefix "string-suffix-ci?"

  (pass-if "empty suffix"
    (string-suffix-ci? "" "foo bar"))

  (pass-if "non-empty suffix - match"
    (string-suffix-ci? "bAr" "foo bar"))

  (pass-if "non-empty suffix - no match"
    (not (string-suffix-ci? "fOo" "foo bar"))))

(with-test-prefix "string-index"

  (pass-if "empty string - char"
    (not (string-index "" #\a)))

  (pass-if "non-empty - char - match"
    (= 5 (string-index "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (not (string-index "frobnicate" #\x)))

  (pass-if "empty string - char - start index"
    (not (string-index "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 5 (string-index "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (not (string-index "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-index "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 5 (string-index "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (not (string-index "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-index "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 0 (string-index "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (not (string-index "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-index "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 1 (string-index "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (not (string-index "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-index "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 1 (string-index "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (not (string-index "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-index "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 0 (string-index "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (not (string-index "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-index "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 1 (string-index "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (not (string-index "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-index "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 1 (string-index "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (not (string-index "frobnicate" char-numeric? 2 5))))

(with-test-prefix "string-index-right"

  (pass-if "empty string - char"
    (not (string-index-right "" #\a)))

  (pass-if "non-empty - char - match"
    (= 5 (string-index-right "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (not (string-index-right "frobnicate" #\x)))

  (pass-if "empty string - char - start index-right"
    (not (string-index-right "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 5 (string-index-right "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (not (string-index-right "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-index-right "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 5 (string-index-right "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (not (string-index-right "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-index-right "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 6 (string-index-right "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (not (string-index-right "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-index-right "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 6 (string-index-right "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (not (string-index-right "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-index-right "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 5 (string-index-right "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (not (string-index-right "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-index-right "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 6 (string-index-right "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (not (string-index-right "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-index-right "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 6 (string-index-right "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (not (string-index-right "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-index-right "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 5 (string-index-right "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (not (string-index-right "frobnicate" char-numeric? 2 5))))

(with-test-prefix "string-skip"

  (pass-if "empty string - char"
    (not (string-skip "" #\a)))

  (pass-if "non-empty - char - match"
    (= 0 (string-skip "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (= 0 (string-skip "frobnicate" #\x)))

  (pass-if "empty string - char - start index"
    (not (string-skip "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 1 (string-skip "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (= 2 (string-skip "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-skip "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 1 (string-skip "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (= 2 (string-skip "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-skip "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 3 (string-skip "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (= 0 (string-skip "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-skip "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 3 (string-skip "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (= 2 (string-skip "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-skip "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 3 (string-skip "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (= 2 (string-skip "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-skip "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 3 (string-skip "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (= 0 (string-skip "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-skip "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 3 (string-skip "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (= 2 (string-skip "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-skip "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 3 (string-skip "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (= 2 (string-skip "frobnicate" char-numeric? 2 5))))

(with-test-prefix "string-skip-right"

  (pass-if "empty string - char"
    (not (string-skip-right "" #\a)))

  (pass-if "non-empty - char - match"
    (= 6 (string-skip-right "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (= 9 (string-skip-right "frobnicate" #\x)))

  (pass-if "empty string - char - start index-right"
    (not (string-skip-right "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 6 (string-skip-right "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (= 9 (string-skip-right "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-skip-right "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 4 (string-skip-right "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (= 4 (string-skip-right "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-skip-right "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 3 (string-skip-right "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (= 9 (string-skip-right "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-skip-right "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 3 (string-skip-right "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (= 9 (string-skip-right "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-skip-right "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 3 (string-skip-right "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (= 4 (string-skip-right "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-skip-right "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 3 (string-skip-right "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (= 9 (string-skip-right "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-skip-right "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 3 (string-skip-right "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (= 9 (string-skip-right "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-skip-right "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 3 (string-skip-right "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (= 4 (string-skip-right "frobnicate" char-numeric? 2 5))))

(with-test-prefix "string-replace"

;   (pass-if "empty string(s), no indices"
;     (string=? "" (string-replace "" "")))

;   (pass-if "empty string(s), 1 index"
;     (string=? "" (string-replace "" "" 0)))

  (pass-if "empty string(s), 2 indices"
    (string=? "" (string-replace "" "" 0 0)))

  (pass-if "empty string(s), 3 indices"
    (string=? "" (string-replace "" "" 0 0 0)))

  (pass-if "empty string(s), 4 indices"
    (string=? "" (string-replace "" "" 0 0 0 0)))

;   (pass-if "no indices"
;     (string=? "uu" (string-replace "foo bar" "uu")))

;   (pass-if "one index"
;     (string=? "fuu" (string-replace "foo bar" "uu" 1)))

  (pass-if "two indices"
    (string=? "fuuar" (string-replace "foo bar" "uu" 1 5)))

  (pass-if "three indices"
    (string=? "fuar" (string-replace "foo bar" "uu" 1 5 1)))

  (pass-if "four indices"
    (string=? "fuar" (string-replace "foo bar" "uu" 1 5 1 2))))

(with-test-prefix "string-tokenize"

  (pass-if "empty string, no char/pred"
    (zero? (length (string-tokenize ""))))

  (pass-if "empty string, charset"
    (zero? (length (string-tokenize "" char-set:punctuation))))

  (pass-if "no char/pred"
    (equal? '("foo" "bar" "!a") (string-tokenize "foo\tbar !a")))

  (pass-if "charset"
    (equal? '("foo" "bar" "!a") (string-tokenize "foo\tbar !a"
						char-set:graphic)))

  (pass-if "charset, start index"
    (equal? '("oo" "bar" "!a") (string-tokenize "foo\tbar !a"
						char-set:graphic 1)))

  (pass-if "charset, start and end index"
    (equal? '("oo" "bar" "!") (string-tokenize "foo\tbar !a"
					       char-set:graphic 1 9))))

(with-test-prefix "string-filter"

  (pass-if "empty string, char"
    (string=? "" (string-filter "" #\.)))

  (pass-if "empty string, charset"
    (string=? "" (string-filter "" char-set:punctuation)))

  (pass-if "empty string, pred"
    (string=? "" (string-filter "" char-alphabetic?)))

  (pass-if "char"
    (string=? "..." (string-filter ".foo.bar." #\.)))

  (pass-if "charset"
    (string=? "..." (string-filter ".foo.bar." char-set:punctuation)))

  (pass-if "pred"
    (string=? "foobar" (string-filter ".foo.bar." char-alphabetic?)))

  (pass-if "char, start index"
    (string=? ".." (string-filter ".foo.bar." #\. 2)))

  (pass-if "charset, start index"
    (string=? ".." (string-filter ".foo.bar." char-set:punctuation 2)))

  (pass-if "pred, start index"
    (string=? "oobar" (string-filter ".foo.bar." char-alphabetic? 2)))

  (pass-if "char, start and end index"
    (string=? "" (string-filter ".foo.bar." #\. 2 4)))

  (pass-if "charset, start and end index"
    (string=? "" (string-filter ".foo.bar." char-set:punctuation 2 4)))

  (pass-if "pred, start and end index"
    (string=? "oo" (string-filter ".foo.bar." char-alphabetic? 2 4))))

(with-test-prefix "string-delete"

  (pass-if "empty string, char"
    (string=? "" (string-delete "" #\.)))

  (pass-if "empty string, charset"
    (string=? "" (string-delete "" char-set:punctuation)))

  (pass-if "empty string, pred"
    (string=? "" (string-delete "" char-alphabetic?)))

  (pass-if "char"
    (string=? "foobar" (string-delete ".foo.bar." #\.)))

  (pass-if "charset"
    (string=? "foobar" (string-delete ".foo.bar." char-set:punctuation)))

  (pass-if "pred"
    (string=? "..." (string-delete ".foo.bar." char-alphabetic?)))

  (pass-if "char, start index"
    (string=? "oobar" (string-delete ".foo.bar." #\. 2)))

  (pass-if "charset, start index"
    (string=? "oobar" (string-delete ".foo.bar." char-set:punctuation 2)))

  (pass-if "pred, start index"
    (string=? ".." (string-delete ".foo.bar." char-alphabetic? 2)))

  (pass-if "char, start and end index"
    (string=? "oo" (string-delete ".foo.bar." #\. 2 4)))

  (pass-if "charset, start and end index"
    (string=? "oo" (string-delete ".foo.bar." char-set:punctuation 2 4)))

  (pass-if "pred, start and end index"
    (string=? "" (string-delete ".foo.bar." char-alphabetic? 2 4))))

(with-test-prefix "string-map"

  (pass-if "constant"
    (string=? "xxx" (string-map (lambda (c) #\x) "foo")))

  (pass-if "identity"
    (string=? "foo" (string-map (lambda (x) x) "foo")))

  (pass-if "upcase"
    (string=? "FOO" (string-map char-upcase "foo"))))

(with-test-prefix "string-for-each"

  (pass-if "copy"
     (let* ((foo "foo")
            (bar (make-string (string-length foo)))
            (i 0))
       (string-for-each
        (lambda (c) (string-set! bar i c) (set! i (+ i 1))) foo)
       (string=? foo bar)))

  (pass-if "index"
     (let* ((foo "foo")
            (bar (make-string (string-length foo))))
       (string-for-each-index
        (lambda (i) (string-set! bar i (string-ref foo i))) foo)
       (string=? foo bar))))

;; Tests for SRFI-13 as implemented by the Gauche scheme system.
;;
;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; See http://sourceforge.net/projects/gauche/

(define-syntax test*
  (syntax-rules ()
    ((test* key expected form)
     (let ((v form))
       (set! total-tests (+ total-tests 1))
       (if (not (equal? v expected))
	   (fail 'key "  " expected "  " v))))
    ((test* key expected form same?)
     (let ((v form))
       (set! total-tests (+ total-tests 1))
       (if (not (same? v expected))
	   (fail 'key "  " expected "  " v))))))

(test* "string-null?" #f (string-null? "abc"))
(test* "string-null?" #t (string-null? ""))
(test* "string-every" #t (string-every #\a ""))
(test* "string-every" #t (string-every #\a "aaaa"))
(test* "string-every" #f (string-every #\a "aaba"))
(test* "string-every" #t (string-every char-set:lower-case "aaba"))
(test* "string-every" #f (string-every char-set:lower-case "aAba"))
(test* "string-every" #t (string-every char-set:lower-case ""))
(test* "string-every" #t (string-every (lambda (x) (char-ci=? x #\a)) "aAaA"))
(test* "string-every" #f (string-every (lambda (x) (char-ci=? x #\a)) "aAbA"))
(test* "string-every" (char->integer #\A)
       (string-every (lambda (x) (char->integer x)) "aAbA"))
(test* "string-every" #t
       (string-every (lambda (x) (error "hoge")) ""))
(test* "string-any" #t (string-any #\a "aaaa"))
(test* "string-any" #f (string-any #\a "Abcd"))
(test* "string-any" #f (string-any #\a ""))
(test* "string-any" #t (string-any char-set:lower-case "ABcD"))
(test* "string-any" #f (string-any char-set:lower-case "ABCD"))
(test* "string-any" #f (string-any char-set:lower-case ""))
(test* "string-any" #t (string-any (lambda (x) (char-ci=? x #\a)) "CAaA"))
(test* "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) "ZBRC"))
(test* "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) ""))
(test* "string-any" (char->integer #\a)
       (string-any (lambda (x) (char->integer x)) "aAbA"))
(test* "string-tabulate" "0123456789"
       (string-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        10))
(test* "string-tabulate" ""
       (string-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        0))
(test* "reverse-list->string" "cBa"
       (reverse-list->string '(#\a #\B #\c)))
(test* "reverse-list->string" ""
       (reverse-list->string '()))
; string-join : Gauche builtin.
(test* "substring/shared" "cde" (substring/shared "abcde" 2))
(test* "substring/shared" "cd"  (substring/shared "abcde" 2 4))
(test* "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "CDE")
         x))
(test* "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "ZABCDE" 3)
         x))
(test* "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "ZABCDEFG" 3 6)
         x))
(test* "string-take" "Pete S"  (string-take "Pete Szilagyi" 6))
(test* "string-take" ""        (string-take "Pete Szilagyi" 0))
(test* "string-take" "Pete Szilagyi" (string-take "Pete Szilagyi" 13))
(test* "string-drop" "zilagyi" (string-drop "Pete Szilagyi" 6))
(test* "string-drop" "Pete Szilagyi" (string-drop "Pete Szilagyi" 0))
(test* "string-drop" ""        (string-drop "Pete Szilagyi" 13))

(test* "string-take-right" "rules" (string-take-right "Beta rules" 5))
(test* "string-take-right" ""      (string-take-right "Beta rules" 0))
(test* "string-take-right" "Beta rules" (string-take-right "Beta rules" 10))
(test* "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
(test* "string-drop-right" "Beta rules" (string-drop-right "Beta rules" 0))
(test* "string-drop-right" ""      (string-drop-right "Beta rules" 10))

(test* "string-pad" "  325" (string-pad "325" 5))
(test* "string-pad" "71325" (string-pad "71325" 5))
(test* "string-pad" "71325" (string-pad "8871325" 5))
(test* "string-pad" "~~325" (string-pad "325" 5 #\~))
(test* "string-pad" "~~~25" (string-pad "325" 5 #\~ 1))
(test* "string-pad" "~~~~2" (string-pad "325" 5 #\~ 1 2))
(test* "string-pad-right" "325  " (string-pad-right "325" 5))
(test* "string-pad-right" "71325" (string-pad-right "71325" 5))
(test* "string-pad-right" "88713" (string-pad-right "8871325" 5))
(test* "string-pad-right" "325~~" (string-pad-right "325" 5 #\~))
(test* "string-pad-right" "25~~~" (string-pad-right "325" 5 #\~ 1))
(test* "string-pad-right" "2~~~~" (string-pad-right "325" 5 #\~ 1 2))

(test* "string-trim"  "a b c d  \n"
       (string-trim "  \t  a b c d  \n"))
(test* "string-trim"  "\t  a b c d  \n"
       (string-trim "  \t  a b c d  \n" #\space))
(test* "string-trim"  "a b c d  \n"
       (string-trim "4358948a b c d  \n" char-set:digit))

(test* "string-trim-right"  "  \t  a b c d"
       (string-trim-right "  \t  a b c d  \n"))
(test* "string-trim-right"  "  \t  a b c d  "
       (string-trim-right "  \t  a b c d  \n" (char-set #\newline)))
(test* "string-trim-right"  "349853a b c d"
       (string-trim-right "349853a b c d03490" char-set:digit))

(test* "string-trim-both"  "a b c d"
       (string-trim-both "  \t  a b c d  \n"))
(test* "string-trim-both"  "  \t  a b c d  "
       (string-trim-both "  \t  a b c d  \n" (char-set #\newline)))
(test* "string-trim-both"  "a b c d"
       (string-trim-both "349853a b c d03490" char-set:digit))

;; string-fill - in string.scm

(test* "string-compare" 5
       (string-compare "The cat in the hat" "abcdefgh"
                       values values values
                       4 6 2 4))
(test* "string-compare-ci" 5
       (string-compare-ci "The cat in the hat" "ABCDEFGH"
                          values values values
                          4 6 2 4))

;; TODO: bunch of string= families

(test* "string-prefix-length" 5
       (string-prefix-length "cancaNCAM" "cancancan"))
(test* "string-prefix-length-ci" 8
       (string-prefix-length-ci "cancaNCAM" "cancancan"))
(test* "string-suffix-length" 2
       (string-suffix-length "CanCan" "cankancan"))
(test* "string-suffix-length-ci" 5
       (string-suffix-length-ci "CanCan" "cankancan"))

(test* "string-prefix?" #t    (string-prefix? "abcd" "abcdefg"))
(test* "string-prefix?" #f    (string-prefix? "abcf" "abcdefg"))
(test* "string-prefix-ci?" #t (string-prefix-ci? "abcd" "aBCDEfg"))
(test* "string-prefix-ci?" #f (string-prefix-ci? "abcf" "aBCDEfg"))
(test* "string-suffix?" #t    (string-suffix? "defg" "abcdefg"))
(test* "string-suffix?" #f    (string-suffix? "aefg" "abcdefg"))
(test* "string-suffix-ci?" #t (string-suffix-ci? "defg" "aBCDEfg"))
(test* "string-suffix-ci?" #f (string-suffix-ci? "aefg" "aBCDEfg"))

(test* "string-index #1" 4
       (string-index "abcd:efgh:ijkl" #\:))
(test* "string-index #2" 4
       (string-index "abcd:efgh;ijkl" (char-set-complement char-set:letter)))
(test* "string-index #3" #f
       (string-index "abcd:efgh;ijkl" char-set:digit))
(test* "string-index #4" 9
       (string-index "abcd:efgh:ijkl" #\: 5))
(test* "string-index-right #1" 4
       (string-index-right "abcd:efgh;ijkl" #\:))
(test* "string-index-right #2" 9
       (string-index-right "abcd:efgh;ijkl" (char-set-complement char-set:letter)))
(test* "string-index-right #3" #f
       (string-index-right "abcd:efgh;ijkl" char-set:digit))
(test* "string-index-right #4" 4
       (string-index-right "abcd:efgh;ijkl" (char-set-complement char-set:letter) 2 5))

(test* "string-count #1" 2
       (string-count "abc def\tghi jkl" #\space))
(test* "string-count #2" 3
       (string-count "abc def\tghi jkl" char-set:whitespace))
(test* "string-count #3" 2
       (string-count "abc def\tghi jkl" char-set:whitespace 4))
(test* "string-count #4" 1
       (string-count "abc def\tghi jkl" char-set:whitespace 4 9))
(test* "string-contains" 3
       (string-contains "Ma mere l'oye" "mer"))
(test* "string-contains" #f
       (string-contains "Ma mere l'oye" "Mer"))
(test* "string-contains-ci" 3
       (string-contains-ci "Ma mere l'oye" "Mer"))
(test* "string-contains-ci" #f
       (string-contains-ci "Ma mere l'oye" "Meer"))

(test* "string-titlecase" "--Capitalize This Sentence."
       (string-titlecase "--capitalize tHIS sentence."))
(test* "string-titlecase" "3Com Makes Routers."
       (string-titlecase "3com makes routers."))
(test* "string-titlecase!" "alSo Whatever"
       (let ((s (string-copy "also whatever")))
         (string-titlecase! s 2 9)
         s))

(test* "string-upcase" "SPEAK LOUDLY"
       (string-upcase "speak loudly"))
(test* "string-upcase" "PEAK"
       (string-upcase "speak loudly" 1 5))
(test* "string-upcase!" "sPEAK loudly"
       (let ((s (string-copy "speak loudly")))
         (string-upcase! s 1 5)
         s))

(test* "string-downcase" "speak softly"
       (string-downcase "SPEAK SOFTLY"))
(test* "string-downcase" "peak"
       (string-downcase "SPEAK SOFTLY" 1 5))
(test* "string-downcase!" "Speak SOFTLY"
       (let ((s (string-copy "SPEAK SOFTLY")))
         (string-downcase! s 1 5)
         s))

(test* "string-reverse" "nomel on nolem on"
       (string-reverse "no melon no lemon"))
(test* "string-reverse" "nomel on"
       (string-reverse "no melon no lemon" 9))
(test* "string-reverse" "on"
       (string-reverse "no melon no lemon" 9 11))
(test* "string-reverse!" "nomel on nolem on"
       (let ((s (string-copy "no melon no lemon")))
         (string-reverse! s) s))
(test* "string-reverse!" "no melon nomel on"
       (let ((s (string-copy "no melon no lemon")))
         (string-reverse! s 9) s))
(test* "string-reverse!" "no melon on lemon"
       (let ((s (string-copy "no melon no lemon")))
         (string-reverse! s 9 11) s))

(test* "string-append" #f
       (let ((s "test")) (eq? s (string-append s))))
(test* "string-concatenate" #f
       (let ((s "test")) (eq? s (string-concatenate (list s)))))
(test* "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       (string-concatenate
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test* "string-concatenate/shared" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       (string-concatenate/shared
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test* "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
       (string-concatenate-reverse
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test* "string-concatenate-reverse" #f
       (let ((s "test"))
         (eq? s (string-concatenate-reverse (list s)))))
(test* "string-concatenate-reverse/shared" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
       (string-concatenate-reverse/shared
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

(test* "string-map" "svool"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello"))
(test* "string-map" "vool"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello" 1))
(test* "string-map" "vo"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello" 1 3))
(test* "string-map!" "svool"
       (let ((s (string-copy "hello")))
         (string-map! (lambda (c)
                        (integer->char (- 219 (char->integer c))))
                      s)
         s))
(test* "string-map!" "hvool"
       (let ((s (string-copy "hello")))
         (string-map! (lambda (c)
                        (integer->char (- 219 (char->integer c))))
                      s 1)
         s))
(test* "string-map!" "hvolo"
       (let ((s (string-copy "hello")))
         (string-map! (lambda (c)
                        (integer->char (- 219 (char->integer c))))
                      s 1 3)
         s))

(test* "string-fold" '(#\o #\l #\l #\e #\h . #t)
       (string-fold cons #t "hello"))
(test* "string-fold" '(#\l #\e . #t)
       (string-fold cons #t "hello" 1 3))
(test* "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
       (string-fold-right cons #t "hello"))
(test* "string-fold-right" '(#\e #\l . #t)
       (string-fold-right cons #t "hello" 1 3))

(test* "string-unfold" "hello"
       (string-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
(test* "string-unfold" "hi hello"
       (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
(test* "string-unfold" "hi hello ho"
       (string-unfold null? car cdr
                      '(#\h #\e #\l #\l #\o) "hi "
                      (lambda (x) " ho")))

(test* "string-unfold-right" "olleh"
       (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
(test* "string-unfold-right" "olleh hi"
       (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
(test* "string-unfold-right" "ho olleh hi"
       (string-unfold-right null? car cdr
                            '(#\h #\e #\l #\l #\o) " hi"
                            (lambda (x) "ho ")))

(test* "string-for-each" "CLtL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language")

         (get-output-string out)))
(test* "string-for-each" "oLtL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language" 1)
         (get-output-string out)))
(test* "string-for-each" "oL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language" 1 10)
         (get-output-string out)))
(test* "string-for-each-index" '(4 3 2 1 0)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello")
         r))
(test* "string-for-each-index" '(4 3 2 1)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1)
         r))
(test* "string-for-each-index" '(2 1)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1 3)
         r))

(test* "xsubstring" "cdefab"
       (xsubstring "abcdef" 2))
(test* "xsubstring" "efabcd"
       (xsubstring "abcdef" -2))
(test* "xsubstring" "abcabca"
       (xsubstring "abc" 0 7))
(test* "xsubstring" "abcabca"
       (xsubstring "abc"
                   30000000000000000000000000000000
                   30000000000000000000000000000007))
(test* "xsubstring" "defdefd"
       (xsubstring "abcdefg" 0 7 3 6))
(test* "xsubstring" ""
       (xsubstring "abcdefg" 9 9 3 6))

(test* "string-xcopy!" "ZZcdefabZZ"
       (let ((s (make-string 10 #\Z)))
         (string-xcopy! s 2 "abcdef" 2)
         s))
(test* "string-xcopy!" "ZZdefdefZZ"
       (let ((s (make-string 10 #\Z)))
         (string-xcopy! s 2 "abcdef" 0 6 3)
         s))

(test* "string-replace" "abcdXYZghi"
       (string-replace "abcdefghi" "XYZ" 4 6))
(test* "string-replace" "abcdZghi"
       (string-replace "abcdefghi" "XYZ" 4 6 2))
(test* "string-replace" "abcdZefghi"
       (string-replace "abcdefghi" "XYZ" 4 4 2))
(test* "string-replace" "abcdefghi"
       (string-replace "abcdefghi" "XYZ" 4 4 1 1))
(test* "string-replace" "abcdhi"
       (string-replace "abcdefghi" "" 4 7))

(test* "string-tokenize" '("Help" "make" "programs" "run," "run," "RUN!")
       (string-tokenize "Help make programs run, run, RUN!"))
(test* "string-tokenize" '("Help" "make" "programs" "run" "run" "RUN")
       (string-tokenize "Help make programs run, run, RUN!"
                        char-set:letter))
(test* "string-tokenize" '("programs" "run" "run" "RUN")
       (string-tokenize "Help make programs run, run, RUN!"
                        char-set:letter 10))
(test* "string-tokenize" '("elp" "make" "programs" "run" "run")
       (string-tokenize "Help make programs run, run, RUN!"
                        char-set:lower-case))

(test* "string-filter" "rrrr"
       (string-filter "Help make programs run, run, RUN!" #\r ))
(test* "string-filter" "HelpmakeprogramsrunrunRUN"
       (string-filter "Help make programs run, run, RUN!"
                      char-set:letter))
(test* "string-filter" "programsrunrun"
       (string-filter "Help make programs run, run, RUN!"
                      (lambda (c) (char-lower-case? c)) 10))
(test* "string-filter" ""
       (string-filter "" (lambda (c) (char-lower-case? c))))
(test* "string-delete" "Help make pogams un, un, RUN!"
       (string-delete "Help make programs run, run, RUN!" #\r))
(test* "string-delete" "   , , !"
       (string-delete "Help make programs run, run, RUN!"
                      char-set:letter))
(test* "string-delete" " , , RUN!"
       (string-delete "Help make programs run, run, RUN!"
                      (lambda (c) (char-lower-case? c)) 10))
(test* "string-delete" ""
       (string-delete "" (lambda (c) (char-lower-case? c))))

;;; Additional tests so that the suite at least touches all
;;; the functions.

(or (<= 0 (string-hash "abracadabra" 20) 19)
    (fail 'string-hash:1))
(or (= (string-hash "abracadabra" 20) (string-hash "abracadabra" 20))
    (fail 'string-hash:2))
(or (= (string-hash "abracadabra" 20 2 7) (string-hash (substring "abracadabra" 2 7) 20))
    (fail 'string.hash:3))

(or (= (string-hash-ci "aBrAcAdAbRa" 20) (string-hash-ci "AbRaCaDaBrA" 20))
    (fail 'string-hash-ci:1))
(or (= (string-hash-ci "aBrAcAdAbRa" 20 2 7) 
       (string-hash-ci (substring "AbRaCaDaBrA" 2 7) 20))
    (fail 'string-hash-ci:2))

(or (string= "foo" "foo") (fail 'string=:1))
(or (string= "foobar" "foo" 0 3) (fail 'string=:2))
(or (string= "foobar" "barfoo" 0 3 3) (fail 'string=:3))
(or (not (string= "foobar" "barfoo" 0 3 2 5)) (fail 'string=:4))

(or (string<> "flo" "foo") (fail 'string<>:1))
(or (string<> "flobar" "foo" 0 3) (fail 'string<>:2))
(or (string<> "flobar" "barfoo" 0 3 3) (fail 'string<>:3))
(or (not (string<> "foobar" "foobar" 0 3 0 3)) (fail 'string<>:4))

(or (string<= "fol" "foo") (fail 'string<=:1))
(or (string<= "folbar" "foo" 0 3) (fail 'string<=:2))
(or (string<= "foobar" "barfoo" 0 3 3) (fail 'string<=:3))
(or (not (string<= "foobar" "barfoo" 0 3 1 4)) (fail 'string<=:4))

(or (string< "fol" "foo") (fail 'string<:1))
(or (string< "folbar" "foo" 0 3) (fail 'string<:2))
(or (string< "folbar" "barfoo" 0 3 3) (fail 'string<:3))
(or (not (string< "foobar" "barfoo" 0 3 1 4)) (fail 'string<:4))

(or (string>= "foo" "fol") (fail 'string>=:1))
(or (string>= "foo" "folbar" 0 3 0 3) (fail 'string>=:2))
(or (string>= "barfoo" "foo" 3 6 0) (fail 'string>=:3))
(or (not (string>= "barfoo" "foobar" 1 4 0 3)) (fail 'string>=:4))

(or (string> "foo" "fol") (fail 'string>:1))
(or (string> "foo" "folbar" 0 3 0 3) (fail 'string>:2))
(or (string> "barfoo" "fol" 3 6 0) (fail 'string>:3))
(or (not (string> "barfoo" "foobar" 1 4 0 3)) (fail 'string>:4))

(or (string-ci= "Foo" "foO") (fail 'string-ci=:1))
(or (string-ci= "Foobar" "fOo" 0 3) (fail 'string-ci=:2))
(or (string-ci= "Foobar" "bArfOo" 0 3 3) (fail 'string-ci=:3))
(or (not (string-ci= "foobar" "BARFOO" 0 3 2 5)) (fail 'string-ci=:4))

(or (string-ci<> "flo" "FOO") (fail 'string-ci<>:1))
(or (string-ci<> "FLOBAR" "foo" 0 3) (fail 'string-ci<>:2))
(or (string-ci<> "flobar" "BARFOO" 0 3 3) (fail 'string-ci<>:3))
(or (not (string-ci<> "foobar" "FOOBAR" 0 3 0 3)) (fail 'string-ci<>:4))

(or (string-ci<= "FOL" "foo") (fail 'string-ci<=:1))
(or (string-ci<= "folBAR" "fOO" 0 3) (fail 'string-ci<=:2))
(or (string-ci<= "fOOBAR" "BARFOO" 0 3 3) (fail 'string-ci<=:3))
(or (not (string-ci<= "foobar" "BARFOO" 0 3 1 4)) (fail 'string-ci<=:4))

(or (string-ci< "fol" "FOO") (fail 'string-ci<:1))
(or (string-ci< "folbar" "FOO" 0 3) (fail 'string-ci<:2))
(or (string-ci< "folbar" "BARFOO" 0 3 3) (fail 'string-ci<:3))
(or (not (string-ci< "foobar" "BARFOO" 0 3 1 4)) (fail 'string-ci<:4))

(or (string-ci>= "FOO" "fol") (fail 'string-ci>=:1))
(or (string-ci>= "foo" "FOLBAR" 0 3 0 3) (fail 'string-ci>=:2))
(or (string-ci>= "BARFOO" "foo" 3 6 0) (fail 'string-ci>=:3))
(or (not (string-ci>= "barfoo" "FOOBAR" 1 4 0 3)) (fail 'string-ci>=:4))

(or (string-ci> "FOO" "fol") (fail 'string-ci>:1))
(or (string-ci> "foo" "FOLBAR" 0 3 0 3) (fail 'string-ci>:2))
(or (string-ci> "barfoo" "FOL" 3 6 0) (fail 'string-ci>:3))
(or (not (string-ci> "barfoo" "FOOBAR" 1 4 0 3)) (fail 'string-ci>:4))

(or (string=? "abcd" (string-append/shared "a" "b" "c" "d"))
    (fail 'string-append/shared))

(or (let-values (((rest start end) (string-parse-start+end #t "foo" '(1 3 fnord))))
      (and (= start 1)
	   (= end 3)
	   (equal? rest '(fnord))))
    (fail 'string-parse-start+end:1))
(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
			 (k #t))))
	 (string-parse-start+end #t "foo" '(1 4))
	 #f)))
    (fail 'string-parse-start+end:2))

(or (let-values (((start end) (string-parse-final-start+end #t "foo" '(1 3))))
      (and (= start 1)
	   (= end 3)))
    (fail 'string-parse-final-start+end:1))

(or (let-string-start+end (start end rest) #t "foo" '(1 3 fnord)
      (and (= start 1)
	   (= end 3)
	   (equal? rest '(fnord))))
    (fail 'let-string-start+end:1))

(or (check-substring-spec #t "foo" 1 3)
    (fail 'check-substring-spec:1))
(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
			 (k #t))))
         (check-substring-spec #t "foo" 1 4)
	 #f)))
    (fail 'check-substring-spec:2))

(or (substring-spec-ok? "foo" 1 3)
    (fail 'substring-spec-ok?:1))
(or (not (substring-spec-ok? "foo" 1 4))
    (fail 'substring-spec-ok?:2))

(or (equal? (make-kmp-restart-vector "") '#())
    (fail 'make-kmp-restart-vector:1))
(or (equal? (make-kmp-restart-vector "a") '#(-1))
    (fail 'make-kmp-restart-vector:2))
; This seems right to me, but is it?
(or (equal? (make-kmp-restart-vector "ab") '#(-1 0))
    (fail 'make-kmp-restart-vector:3))
; The following is from an example in the code, but I expect it is not right.
(or (equal? (make-kmp-restart-vector "abdabx") '#(-1 0 0 -1 1 2))
    (fail 'make-kmp-restart-vector:4))

; FIXME!  Implement tests for these:
;   string-kmp-partial-search
;   kmp-step


;;; Regression tests: check that reported bugs have been fixed

; From: Matthias Radestock <matthias@sorted.org>
; Date: Wed, 10 Dec 2003 21:05:22 +0100
;
; Chris Double has found the following bug in the reference implementation:
;
;  (string-contains "xabc" "ab") => 1    ;good
;  (string-contains "aabc" "ab") => #f   ;bad
;
; Matthias.

(or (equal? 1 (string-contains "aabc" "ab"))
    (fail 'string-contains:regression:1))

(or (equal? 5 (string-contains "ababdabdabxxas" "abdabx"))
    (fail 'string-contains:regression:2))

(or (equal? 1 (string-contains-ci "aabc" "ab"))
    (fail 'string-contains-ci:regression:1))

; (message continues)
;
; PS: There is also an off-by-one error in the bounds check of the 
; unoptimized version of string-contains that is included as commented out 
; code in the reference implementation. This breaks things like
; (string-contains "xab" "ab") and (string-contains "ab" "ab").

; This off-by-one bug has been fixed in the comments of the version
; of SRFI-13 shipped with Larceny.  In a version of the code without
; the fix the following test will catch the bug:

(or (equal? 0 (string-contains "ab" "ab"))
    (fail 'string-contains:regression:2))

; From: dvanhorn@emba.uvm.edu
; Date: Wed, 26 Mar 2003 08:46:41 +0100
;
; The SRFI document gives,
;
;   string-filter s char/char-set/pred [start end] -> string 
;   string-delete s char/char-set/pred [start end] -> string 
;
; Yet the reference implementation switches the order giving,
;
;   ;;; string-delete char/char-set/pred string [start end]
;   ;;; string-filter char/char-set/pred string [start end]
;   ...
;   (define (string-delete criterion s . maybe-start+end)
;   ...
;   (define (string-filter criterion s . maybe-start+end)
;
; I reviewed the SRFI-13 mailing list and c.l.scheme, but found no mention of 
; this issue.  Apologies if I've missed something.

(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
			 (k #f))))
         (string=? "ADR" (string-filter "abrAcaDabRa" char-set:upper-case)))))
    (fail 'string-filter:regression:1))

(or (call-with-current-continuation
     (lambda (k)
       (parameterize ((error-handler
		       (lambda args
			 (k #f))))
         (string=? "abrcaaba" (string-delete "abrAcaDabRa" char-set:upper-case)))))
    (fail 'string-delete:regression:1))

;;;

(writeln "Done.")

