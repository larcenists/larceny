; extras.scm - utility procedures for Sassy
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

; Alex Shinn contributed a new read-file and string-split

;==============;
; 	       ;
; Extras       ;
; 	       ;
;==============;
(define-syntax when
  (syntax-rules ()
    ((_ test conseq ...) (if test (begin conseq ...)))))

; Changed to Alex's TCO version
(define (read-file file)
  (call-with-input-file file
    (lambda (p)
      (let lp ((res '()))
        (let ((sexp (read p)))
          (if (eof-object? sexp)
            (reverse res)
            (lp (cons sexp res))))))))

; Alias this for Alex's stuff
(define file->sexp-list read-file)


(define (string-split str ch)
  (let ((len (string-length str)))
    (let lp ((from 0) (to 0) (res '()))
      (define (collect)
        (if (= from to) res (cons (substring str from to) res)))
      (cond
        ((>= to len)
         (reverse (collect)))
        ((eqv? ch (string-ref str to))
         (lp (+ to 1) (+ to 1) (collect)))
        (else
         (lp from (+ to 1) res))))))

; Something useful to wrap functions in to hurry things along.
; Of course only use this when not using mustation on the data.
(define-syntax memoize
  (syntax-rules ()
    ((_ proc)
     (let ((the-proc proc)
	   (last-in  (if #f #f))
	   (last-out #f))
	 (lambda (arg2)
	   (if (eqv? arg2 last-in)
	       last-out
	       (begin (set! last-in arg2)
		      (let ((result (the-proc arg2)))
			(set! last-out result)
			last-out))))))))


