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


;==============;
; 	       ;
; Extras       ;
; 	       ;
;==============;
(define-syntax when
  (syntax-rules ()
    ((_ test conseq ...) (if test (begin conseq ...)))))

(define (read-file file)
  (with-input-from-file file
    (lambda ()
      (let iter ((next (read)))
	(if (eof-object? next)
	    '()
	    (cons next (iter (read))))))))

