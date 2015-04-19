;;; SRFI 13 string library reference implementation		-*- Scheme -*-
;;; Olin Shivers 7/2000
;;;
;;; $Id$
;;;
;;; Conflicts with:
;;;     (rnrs base): string->list, string-copy, string-for-each
;;;     (rnrs unicode): string-upcase, string-downcase, string-titlecase
;;;     (rnrs mutable-strings): string-fill!
;;;
;;; Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;; Copyright (c) 1998, 1999, 2000 Olin Shivers. All rights reserved.
;;;   The details of the copyrights appear at the end of the file. Short
;;;   summary: BSD-style open source.

;;; FIXME:
;;;     make-kmp-restart-vector appears to be incorrect
;;;     this file is likely to have incompatibilities with Unicode

(define-library (srfi 13 strings)

  (export
   string-map string-map!
   string-fold       string-unfold
   string-fold-right string-unfold-right 
   string-tabulate string-for-each string-for-each-index
   string-every string-any
   string-hash string-hash-ci
   string-compare string-compare-ci
   string=    string<    string>    string<=    string>=    string<>
   string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
   string-downcase  string-upcase  string-titlecase  
   string-downcase! string-upcase! string-titlecase! 
   string-take string-take-right
   string-drop string-drop-right
   string-pad string-pad-right
   string-trim string-trim-right string-trim-both
   string-filter string-delete
   string-index string-index-right 
   string-skip  string-skip-right
   string-count
   string-prefix-length string-prefix-length-ci
   string-suffix-length string-suffix-length-ci
   string-prefix? string-prefix-ci?
   string-suffix? string-suffix-ci?
   string-contains string-contains-ci
   string-copy! substring/shared
   string-reverse string-reverse! reverse-list->string
   string-concatenate string-concatenate/shared
   string-concatenate-reverse string-concatenate-reverse/shared
   string-append/shared
   xsubstring string-xcopy!
   string-null?
   string-join
   string-tokenize
   string-replace
   
;  R5RS extended:
   string->list string-copy string-fill! 

;  R5RS re-exports:
   string? make-string string-length string-ref string-set! 

;  R5RS re-exports (also defined here but commented-out):
   string string-append list->string

;  Low-level routines:
   make-kmp-restart-vector string-kmp-partial-search kmp-step
   string-parse-start+end
   string-parse-final-start+end
   let-string-start+end
   check-substring-spec
   substring-spec-ok?)

  (import (srfi :13 strings)))


(define-library (srfi 13)

  (export
   string-map string-map!
   string-fold       string-unfold
   string-fold-right string-unfold-right 
   string-tabulate string-for-each string-for-each-index
   string-every string-any
   string-hash string-hash-ci
   string-compare string-compare-ci
   string=    string<    string>    string<=    string>=    string<>
   string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
   string-downcase  string-upcase  string-titlecase  
   string-downcase! string-upcase! string-titlecase! 
   string-take string-take-right
   string-drop string-drop-right
   string-pad string-pad-right
   string-trim string-trim-right string-trim-both
   string-filter string-delete
   string-index string-index-right 
   string-skip  string-skip-right
   string-count
   string-prefix-length string-prefix-length-ci
   string-suffix-length string-suffix-length-ci
   string-prefix? string-prefix-ci?
   string-suffix? string-suffix-ci?
   string-contains string-contains-ci
   string-copy! substring/shared
   string-reverse string-reverse! reverse-list->string
   string-concatenate string-concatenate/shared
   string-concatenate-reverse string-concatenate-reverse/shared
   string-append/shared
   xsubstring string-xcopy!
   string-null?
   string-join
   string-tokenize
   string-replace
   
;  R5RS extended:
   string->list string-copy string-fill! 

;  R5RS re-exports:
   string? make-string string-length string-ref string-set! 

;  R5RS re-exports (also defined here but commented-out):
   string string-append list->string

;  Low-level routines:
   make-kmp-restart-vector string-kmp-partial-search kmp-step
   string-parse-start+end
   string-parse-final-start+end
   let-string-start+end
   check-substring-spec
   substring-spec-ok?)

  (import (srfi 13 strings)))

; eof
