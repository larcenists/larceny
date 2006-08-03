; main.scm - Sassy's main
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


; module parse
; import macros api parse
; export sassy

;=======================;
; 		        ;
; Sassy Main            ;
; 		        ;
;=======================;

(define sassy-current #f)

(define (sassy input . options)

  ; the only option so far...
  (define expand? #t)

  (do ((o options (cdr o)))
      ((null? o))
    (case (car o)
      ((dont-expand) (set! expand? #f))
      (else (error "sassy: unrecognized option" (car o)))))

  (let ((output (make-sassy-output
		 (list (make-hash-table)) ; empty symbol table
		 '()                      ; empty reloc list
		 #f                       ; no entry point
		 (make-pushdown-stack)    ; empty data stack
		 (make-pushdown-stack)    ; empty text stack
		 4                        ; default heap align
		 4                        ; default data align
		 16                       ; default text align
		 0                        ; initial heap size
		 0                        ; default text org
		 32)))                    ; default bits size

    (set! sassy-current output)

    (when expand? (sassy-expand (make-hash-table))) ; install fresh macro table
    
    (cond ((string? input)
	   (parse-directives (read-file input) output 0 expand?))
	  ((pair?   input)
	   (parse-directives input output 0 expand?))
	  (else (error "sassy: bad input" input)))

    (sassy-symbol-table-set! output (car (sassy-symbol-table output)))
    
    output))

; TODO: also want option called "traditional" (only labels and opcodes
; allowed in the text section), for faster handling of text
; section. and "ignore-rel-relocs", so that no relative relocation
; info will be recorded.

