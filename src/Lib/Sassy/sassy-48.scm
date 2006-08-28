; sassy-48.scm - module declarations for scheme48
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

; NOTE: everything is pretty muched lumped together, so opening sassy
; also exposes the whole API (but not Sassy's internals), and all the
; output modules too, all at once. 

;===================;
; 		    ;
; Interfaces	    ;
; 		    ;
;===================;

(define-interface sassy-output-interface
  (export make-sassy-output
	  sassy-output?
	  sassy-symbol-table
	  sassy-symbol-table-set!
	  sassy-reloc-list
	  sassy-reloc-list-set!
	  sassy-entry-point
	  sassy-entry-point-set!
	  sassy-data-stack
	  sassy-data-stack-set!
	  sassy-text-stack
	  sassy-text-stack-set!
	  sassy-heap-size
	  sassy-heap-size-set!
	  sassy-text-org
	  sassy-text-org-set!
	  sassy-heap-align
	  sassy-heap-align-set!
	  sassy-data-align
	  sassy-data-align-set!
	  sassy-text-align
	  sassy-text-align-set!))

(define-interface sassy-symbol-interface
  (export make-sassy-symbol
	  sassy-symbol?
	  sassy-symbol-name
	  sassy-symbol-name-set!
	  sassy-symbol-scope
	  sassy-symbol-scope-set!
	  sassy-symbol-section
	  sassy-symbol-section-set!
	  sassy-symbol-offset
	  sassy-symbol-offset-set!
	  sassy-symbol-size
	  sassy-symbol-size-set!
	  sassy-symbol-unres
	  sassy-symbol-unres-set!))

(define-interface sassy-reloc-interface
  (export make-sassy-reloc
	  sassy-reloc?
	  sassy-reloc-name
	  sassy-reloc-name-set!
	  sassy-reloc-section
	  sassy-reloc-section-set!
	  sassy-reloc-offset
	  sassy-reloc-offset-set!
	  sassy-reloc-type
	  sassy-reloc-type-set!
	  sassy-reloc-patcher
	  sassy-reloc-patcher-set!
	  sassy-reloc-value
	  sassy-reloc-value-set!
	  sassy-reloc-width
	  sassy-reloc-width-set!
	  sassy-reloc-target-section
	  sassy-reloc-target-section-set!))
	  

(define-interface sassy-output-extra-interface
  (export sassy-data-list
	  sassy-text-list
	  sassy-text-size
	  sassy-data-size
	  sassy-symbol-exists?))

(define-interface sassy-push-stack-interface
  (export make-pushdown-stack
	  make-pushup-stack
	  push-stack-empty?
	  push-stack-push
	  push-stack-pointer
	  push-stack-items
	  push-stack-patch
	  push-stack-push->patcher
	  push-stack-save
	  push-stack-direction
	  push-stack-size
	  push-stack-append!
	  push-stack-align))

(define-interface sassy-utilities-interface
  (export sassy-hexdump
	  sassy-print-symbols
	  sassy-print-relocs))

(define-interface sassy-api-interface
  (compound-interface
   sassy-output-interface
   sassy-symbol-interface
   sassy-reloc-interface
   sassy-output-extra-interface
   sassy-push-stack-interface
   sassy-utilities-interface))

(define-interface sassy-units-interface
  (export sassy-linked-units
	  sassy-linking-files
	  sassy-loaded-sources
	  sassy-code-sources
	  sassy-dynamic-sources
	  sassy-dependencies
	  sassy-options
	  sassy-include-path
	  sassy-find-file))


(define-interface sassy-48-init-interface
  (export write-byte
	  read-byte
	  make-hash-table
	  hash-table-set!
	  hash-table?
	  hash-table-ref
	  alist->hash-table
	  hash-table-values
	  hash-table-keys
	  hash-table-delete!
	  hash-table-exists?
	  hash-table-walk
	  logior
	  logand
	  lognot
	  logxor
	  ash
	  bit-field
	  file-exists?
	  delete-file
	  read-file
	  getenv
	  file->sexp-list
	  string-split
	  (memoize :syntax)
	  (when :syntax)))

(define-interface sassy-interface
  (compound-interface sassy-48-init-interface
		      sassy-api-interface
		      sassy-units-interface
		      (export char->integer
			      integer->char
			      sassy
			      sassy-current
			      sassy-symbol-current
			      sassy-expand
			      sassy-make-bin
			      sassy-make-elf)))


;===================;
; 		    ;
; Structures	    ;
; 		    ;
;===================;

(define-structure sassy-48-init sassy-48-init-interface
  (open (modify scheme (hide integer->char char->integer))
	srfi-23 tables bitwise posix-files posix-process-data
	(modify ascii (rename (ascii->char integer->char)
			      (char->ascii char->integer))))
  (files "inits/scheme48-1.3.scm" "extras.scm"))

(define-structure sassy sassy-interface
  (open (modify scheme (hide integer->char char->integer))
	sassy-48-init srfi-1 srfi-8 srfi-9 srfi-23
	(modify ascii (rename (ascii->char integer->char)
			      (char->ascii char->integer))))
  (files "units.scm"
	 "push-stacks.scm"
	 "api.scm"
	 "intern.scm"
	 "macros.scm"
	 "numbers.scm"
	 "other/srfi-56-pieces.scm"
	 "operands.scm"
	 "text-block.scm"
	 "opcodes.scm"
	 "text.scm"
	 "parse.scm"
	 "main.scm"
	 "flat-bin.scm"
	 "elf.scm"))
  

(define-structure sassy-tests (export sassy-run-tests %%%include-test
				      char->integer integer->char)
  (open (modify scheme (hide integer->char char->integer))
	sassy-48-init sassy srfi-1 srfi-23
	(modify ascii (rename (ascii->char integer->char)
			      (char->ascii char->integer))))
  (files "tests/run-tests.scm"))

  
