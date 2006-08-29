; api.scm - access Sassy's output
; Copyright (C) 2005 Jonathan Kraut

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Contact:
; Jonathan Kraut
; 4130 43 ST #C2
; Sunnyside, NY 11104
; jak76@columbia.edu

; see file COPYING in the top of Sassy's distribution directory


; module api
; import srfi-9 push-stacks
; export all

(define-record-type sassy-output
  (make-sassy-output a b c d e f g h i j k)
  sassy-output?
  (a sassy-symbol-table sassy-symbol-table-set!)
  (b sassy-reloc-list  sassy-reloc-list-set!)
  (c sassy-entry-point sassy-entry-point-set!)
  (d sassy-data-stack sassy-data-stack-set!)
  (e sassy-text-stack sassy-text-stack-set!)
  (f sassy-heap-align sassy-heap-align-set!)
  (g sassy-data-align sassy-data-align-set!)
  (h sassy-text-align sassy-text-align-set!)
  (i sassy-heap-size  sassy-heap-size-set!)
  (j sassy-text-org   sassy-text-org-set!)
  (k sassy-bits       sassy-bits-set!))

(define-record-type sassy-symbol
  (make-sassy-symbol a b c d e f g)
  sassy-symbol?
  (a sassy-symbol-name    sassy-symbol-name-set!)
  (b sassy-symbol-scope   sassy-symbol-scope-set!)
  (c sassy-symbol-section sassy-symbol-section-set!)
  (d sassy-symbol-offset  sassy-symbol-offset-set!)
  (e sassy-symbol-size    sassy-symbol-size-set!)
  (f sassy-symbol-unres   sassy-symbol-unres-set!)
  (g sassy-symbol-exp     sassy-symbol-exp-set!))

(define-record-type sassy-reloc
  (make-sassy-reloc a b c d e f g h)
  sassy-reloc?
  (a sassy-reloc-name    sassy-reloc-name-set!)
  (b sassy-reloc-section sassy-reloc-section-set!)
  (c sassy-reloc-offset  sassy-reloc-offset-set!)
  (d sassy-reloc-type    sassy-reloc-type-set!)
  (e sassy-reloc-patcher sassy-reloc-patcher-set!)
  (f sassy-reloc-value   sassy-reloc-value-set!)
  (g sassy-reloc-width   sassy-reloc-width-set!)
  (h sassy-reloc-target-section sassy-reloc-target-section-set!))

(define (sassy-data-list sassy-output)
  (push-stack-items (sassy-data-stack sassy-output)))
(define (sassy-text-list sassy-output)
  (push-stack-items (sassy-text-stack sassy-output)))
(define (sassy-text-bytevector sassy-output)
  (push-stack-items/bytes (sassy-text-stack sassy-output)))

(define (sassy-text-size sassy-output)
  (push-stack-size (sassy-text-stack sassy-output)))
(define (sassy-data-size sassy-output)
  (push-stack-size (sassy-data-stack sassy-output)))

(define (sassy-symbol-exists? sassy-output name)
  (let iter ((rst (sassy-symbol-table sassy-output)))
    (cond ((hash-table? rst)
	   (hash-table-ref rst name (lambda () #f)))
	  ((hash-table? (car rst))
	   (hash-table-ref (car rst) name (lambda () #f)))
	  ((eq? name (sassy-symbol-name (car rst))) (car rst))
	  (else (iter (cdr rst))))))

(define (sassy-symbol-current name)
  (sassy-symbol-exists? sassy-current name))

(define (sassy-hexdump list-of-bytes)
  (let ((print-count (lambda (count)
		       (let ((n (number->string count 16)))
			 (display (make-string (- 8 (string-length n)) #\0))
			 (display n)
			 (display #\space))))
	(byte->azkey (lambda (byte) 
		       (if (and (>= byte 32) (<= byte 126))
			   (integer->char byte)
			   #\.)))
	(print-hex   (lambda (byte)
		       (let ((tmp (number->string byte 16)))
			 (if (= 1 (string-length tmp))
			     (display "0"))
			 (display tmp)
			 (display #\space))))
	(print-string (lambda (string)
			(display "|")
			(display (list->string (reverse string)))
			(display "|")
			(newline))))
    (define string '())
    (define col 1)
    (newline)
    (do ((rest list-of-bytes (cdr rest))
	 (count 0 (+ count 1)))
	((null? rest)
	 (if (not (zero? (modulo count 16)))
	     (begin
	       (display (make-string (- 61 col) #\space))
	       (print-string string))))
      (if (zero? (modulo count 16))
	  (begin (print-count count) (set! col 10)))
      (if (zero? (modulo count 8))
	  (begin (display #\space) (set! col (+ col 1))))
      (print-hex (car rest))
      (set! col (+ col 3))
      (set! string (cons (byte->azkey (car rest)) string))
      (if (= 15 (modulo count 16))
	  (begin (display #\space)
		 (print-string string)
		 (set! string '()))))))


(define sassy-print-relocs #f)

(define sassy-print-symbols #f)

(let ((print-field (lambda (t v record)
		     (display t)
		     (display ": ")
		     (let ((t (v record)))
		       (display (or t "#<undefined>")))
		     (newline)))
      (make-num (lambda (x)
		  (if x
		      (string-append "#x" (number->string x 16))
		      "#<undefined>"))))
  (set! sassy-print-relocs
	(lambda (sassy-output)
	  (for-each
	   (lambda (reloc)
	     (newline)
	     (print-field "name   " sassy-reloc-name reloc)
	     (print-field "section" sassy-reloc-section reloc)
	     (print-field "offset " (lambda (x)
				      (make-num (sassy-reloc-offset x))) reloc)
	     (print-field "type   " sassy-reloc-type reloc)
	     (print-field "value  " (lambda (x)
				      (make-num (sassy-reloc-value x))) reloc)
	     (print-field "width  " (lambda (x)
				      (make-num (sassy-reloc-width x))) reloc)
	     (print-field "targ-sect" sassy-reloc-target-section reloc))
	   (sassy-reloc-list sassy-output))))
  (set! sassy-print-symbols
	(lambda (sassy-output)
	  (for-each
	   (lambda (sym)
	     (newline)
	     (print-field "name   " sassy-symbol-name sym)
	     (print-field "scope  " sassy-symbol-scope sym)
	     (print-field "section" sassy-symbol-section sym)
	     (print-field "offset " (lambda (x)
				      (make-num (sassy-symbol-offset x))) sym)
	     (print-field "size   " (lambda (x)
				      (make-num (sassy-symbol-size x))) sym))
	   (hash-table-values (sassy-symbol-table sassy-output))))))
