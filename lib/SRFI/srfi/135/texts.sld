;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; Self-describing alternative name for the (srfi 135) library.

(define-library (srfi 135 texts)

  (export

   ;; Predicates

   text?                 textual?
   textual-null? 
   textual-every         textual-any

   ;; Constructors

   make-text             text
   text-tabulate
   text-unfold           text-unfold-right

   ;; Conversion

   textual->text
   textual->string       textual->vector      textual->list
   string->text          vector->text         list->text    reverse-list->text
   textual->utf8         textual->utf16be
   textual->utf16        textual->utf16le
   utf8->text            utf16be->text
   utf16->text           utf16le->text

   ;; Selection

   text-length           textual-length
   text-ref              textual-ref
   subtext               subtextual
   textual-copy
   textual-take          textual-take-right
   textual-drop          textual-drop-right
   textual-pad           textual-pad-right 
   textual-trim          textual-trim-right   textual-trim-both

   ;; Replacement

   textual-replace

   ;; Comparison

   textual=?             textual-ci=?
   textual<?             textual-ci<?
   textual>?             textual-ci>?
   textual<=?            textual-ci<=?
   textual>=?            textual-ci>=?

   ;; Prefixes & suffixes

   textual-prefix-length textual-suffix-length
   textual-prefix?       textual-suffix?    

   ;; Searching

   textual-index         textual-index-right
   textual-skip          textual-skip-right
   textual-contains      textual-contains-right

   ;; Case conversion

   textual-upcase        textual-downcase
   textual-foldcase      textual-titlecase

   ;; Concatenation

   textual-append        textual-concatenate  textual-concatenate-reverse
   textual-join

   ;; Fold & map & friends

   textual-fold          textual-fold-right
   textual-map           textual-for-each
   textual-map-index     textual-for-each-index
   textual-count
   textual-filter        textual-remove
;  textual-reverse

   ;; Replication & splitting

   textual-replicate     textual-split
   )

  (import (srfi 135)))

;;; eof
