; elf.scm - create ELF files from Sassy's output
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


; module elf
; import api push-stacks numbers
; export all

(define (sassy-make-elf output-file sassy-output . user-section)

; the elf constants used
  (define sht-progbits  1)
  (define sht-symtab    2)
  (define sht-strtab    3)
  (define sht-nobits    8)
  (define sht-rel       9)
  (define sht-louser    #x80000000)
  (define shf-write     1)
  (define shf-alloc     2)
  (define shf-execinstr 4)
  (define stn-undef     0)
  (define stb-local     0)
  (define stb-global    1)
  (define stt-notype    0)
  (define stt-object    1)
  (define stt-func      2)
  (define stt-section   3)
  (define shn-abs  #xfff1)
  (define r-386-32      1)
  (define r-386-pc32    2)
  (define r-386-got32   3)
  (define r-386-plt32   4)
  (define r-386-gotpc  10)
  (define r-386-gotoff  9)
  (define shn-undef     0)

; wrappers for symbols that begin with a period, since r5rs doesn't
; allow you to actually write such symbols literally
  (define dot-text     (string->symbol ".text"))
  (define dot-data     (string->symbol ".data"))
  (define dot-bss      (string->symbol ".bss"))
  (define dot-symtab   (string->symbol ".symtab"))
  (define dot-shstrtab (string->symbol ".shstrtab"))
  (define dot-rel-data (string->symbol ".rel.data"))
  (define dot-rel-text (string->symbol ".rel.text"))
  (define dot-strtab   (string->symbol ".strtab"))

  (define empty-symbol (string->symbol ""))

  (define (symbol<=? x y)
    (string<=? (symbol->string x) (symbol->string y)))

; Sort some of the things to ensure identical orders of entries regardless of the particular scheme implementation (hash-table-values)
  (define (quicksort-records lst acc pred)
    (if (or (null? lst) (null? (cdr lst)))
	lst
	(let ((p (car lst)))
	  (let iter ((r (cdr lst)) (l '()) (g '()))
	    (cond ((null? r) (append (quicksort-records l acc pred)
				     (cons p (quicksort-records g acc pred))))
		  ((pred (acc (car r)) (acc p))
		   (iter (cdr r) (cons (car r) l) g))
		  (else (iter (cdr r) l (cons (car r) g))))))))
  
; Return a new empty elf string table. Elf string tables consists of
; \nul, followed by \nul-terminated strings.
  (define (make-string-table)
    (let ((table (make-pushdown-stack)))
      (push-stack-push table 0)
      table))

; Enter a symbol in a string-table and return its index
  (define (string-table-set! table symbol)
    (let ((return-ndx (push-stack-size table)))
      (push-stack-push table (map char->integer
				  (string->list (symbol->string symbol))))
      (push-stack-push table 0)
      return-ndx))
 
; Entries-tables map symbols or section-names to their row in the
; appropriate table (sym-table or sh-table) in the image
  (define (make-entries-table) (cons 0 (make-hash-table)))

  (define (entry-set! entries-table symbol)
    (hash-table-set! (cdr entries-table) symbol (car entries-table))
    (set-car! entries-table (+ (car entries-table) 1)))

  (define (entry-ref entries-table symbol)
    (hash-table-ref (cdr entries-table) symbol))

  (define number-of-entries car)
  
  (let ((image             (make-pushdown-stack))
	(strtab            (make-string-table))
	(sh-strtab         (make-string-table))
	(e-shoff-patcher    #f)
	(e-shnum-patcher    #f)
	(e-shstrndx-patcher #f)
	(sh-table          (make-pushdown-stack))
	(sh-table-entries  (make-entries-table))
	(sym-table         (make-pushdown-stack))
	(sym-table-entries (make-entries-table)))
    

; Add a section header. There should 10 fields: sh-name sh-type
; sh-flags sh-addr sh-offset sh-size sh-link sh-info sh-addralign and
; sh-entsize
    (define (section-header-set! name . fields)
      (for-each (lambda (field)
		  (push-stack-push sh-table (number->byte-list field 4)))
		fields)
      (entry-set! sh-table-entries name))

; Add a symbol 
    (define (sym-table-set! name
			    strtab-ndx value size info other ndx)
      (for-each (lambda (field)
		  (push-stack-push sym-table
				   (number->byte-list field 4)))
		(list strtab-ndx value size))
      (for-each (lambda (field)
		  (push-stack-push sym-table field))
		(list info other))
      (push-stack-push sym-table (number->byte-list ndx 2))
      (entry-set! sym-table-entries name))

; Add a list of symbols 
    (define (make-sym-entries sym-lst scope)
      (for-each
       (lambda (symbol)
	 (sym-table-set! (sassy-symbol-name symbol)
			 (string-table-set! strtab (sassy-symbol-name symbol))
			 (or (sassy-symbol-offset symbol) stn-undef)
			 (or (sassy-symbol-size symbol) 0)
			 (+ (ash scope 4)
			    (case (sassy-symbol-section symbol)
			      ((data) stt-object)
			      ((text) stt-func)
			      (else stt-notype)))
			 0
			 (case (sassy-symbol-section symbol)
			   ((heap) (entry-ref sh-table-entries dot-bss))
			   ((data) (entry-ref sh-table-entries dot-data))
			   ((text) (entry-ref sh-table-entries dot-text))
			   (else shn-undef))))
       sym-lst))
 
; Filter a list of records into two separate lists according to
; (getter field-val). Return 2 values (lists)
    (define (split-list-of-records list-of-records getter field-val)
      (let iter ((rest list-of-records)
		 (win '())
		 (lose '()))
	(cond ((null? rest) (values win lose))
	      ((eq? field-val (getter (car rest)))
	       (iter (cdr rest)
		     (cons (car rest) win)
		     lose))
	      (else (iter (cdr rest)
			  win
			  (cons (car rest) lose))))))

; Dispatch on the reloc type and name to push an elf-reloc on to the image 
    (define (push-reloc reloc)
      (let ((get-reloc-name
	     (lambda (name)
	       (case (sassy-symbol-section
		      (hash-table-ref (sassy-symbol-table sassy-output)
				      name))
		 ((text) dot-text)
		 ((data) dot-data)
		 ((heap) dot-bss)
		 (else name))))
	    (make-reloc-info
	     (lambda (name type)
	       (+ (if name
		      (ash (entry-ref sym-table-entries name) 8)
		      0)
		  (case type
		    ((abs)    r-386-32)
		    ((rel)    r-386-pc32)
		    ((gotpc)  r-386-gotpc)
		    ((gotoff) r-386-gotoff)
		    ((got32)  r-386-got32)
		    ((plt32)  r-386-plt32))))))
	(push-stack-push image (number->byte-list (sassy-reloc-offset reloc) 4))
	(push-stack-push
	 image
	 (number->byte-list
	  (case (sassy-reloc-type reloc)
	    ((abs)
	     (make-reloc-info (if (not (sassy-reloc-name reloc))
				  (case (sassy-reloc-target-section reloc)
				    ((data) dot-data)
				    ((text) dot-text)
				    ((heap) dot-bss))
				  (get-reloc-name (sassy-reloc-name reloc)))
			      'abs))
	    ((rel)
	     ((sassy-reloc-patcher reloc) -4)
	     (make-reloc-info (sassy-reloc-name reloc) 'rel))
	    ((gotoff) (make-reloc-info (get-reloc-name (sassy-reloc-name reloc))
				       'gotoff))
	    ((gotpc) (make-reloc-info (sassy-reloc-name reloc) 'gotpc))
	    ((got32) (make-reloc-info (sassy-reloc-name reloc) 'got32))
	    ((sym32) (make-reloc-info (sassy-reloc-name reloc) 'abs))
	    ((plt32)
	     ((sassy-reloc-patcher reloc) -4)
	     (make-reloc-info (sassy-reloc-name reloc) 'plt32)))
	  4))))

; All setup - now to start building:

; Create some null entries

    (section-header-set! empty-symbol 0 0 0 0 0 0 0 0 0 0)
    (sym-table-set! empty-symbol 0 0 0 0 0 0)
    (sym-table-set! 'sh-null 0 0 0 stt-section stb-local shn-abs)

; Build elf-header and patchers for later, and pad it.

    (push-stack-push image (list 127 69 76 70 1 1 1 0 0 0 0 0 0 0 0
				 0 1 0 3 0 1 0 0 0 0 0 0 0 0 0 0 0))
    
    (set! e-shoff-patcher (push-stack-push->patcher image (list 0 0 0 0)))
    
    (push-stack-push image (list 0 0 0 0 52 0 0 0 0 0 40 0))

    (set! e-shnum-patcher
	  (push-stack-push->patcher image (number->byte-list 0 2)))
    
    (set! e-shstrndx-patcher
	  (push-stack-push->patcher image (number->byte-list 0 2)))
    
    (push-stack-align image 16 0)

; Handle the heap section

    (if (not (zero? (sassy-heap-size sassy-output)))
	(begin
	  (section-header-set! dot-bss
			       (string-table-set! sh-strtab dot-bss)
			       sht-nobits
			       (+ shf-write shf-alloc)
			       0
			       (push-stack-size image)
			       (sassy-heap-size sassy-output)
			       0
			       0
			       (sassy-heap-align sassy-output)
			       0)
	  (sym-table-set! dot-bss 0 0 0 stt-section stb-local
			  (entry-ref sh-table-entries dot-bss))))

; Handle the data section
    
    (if (not (zero? (sassy-data-size sassy-output)))
	(begin
	  (section-header-set! dot-data
			       (string-table-set! sh-strtab dot-data)
			       sht-progbits
			       (+ shf-write shf-alloc)
			       0
			       (push-stack-size image)
			       (sassy-data-size sassy-output)
			       0
			       0
			       (sassy-data-align sassy-output)
			       0)
	  (sym-table-set! dot-data 0 0 0 stt-section stb-local
			  (entry-ref sh-table-entries dot-data))
	  (push-stack-append! image (sassy-data-stack sassy-output))
	  (push-stack-align image 16 0)))

; Handle the text section

    (if (not (zero? (sassy-text-size sassy-output)))
	(begin
	  (section-header-set! dot-text
			       (string-table-set! sh-strtab dot-text)
			       sht-progbits
			       (+ shf-execinstr shf-alloc)
			       0
			       (push-stack-size image)
			       (sassy-text-size sassy-output)
			       0
			       0
			       (sassy-text-align sassy-output)
			       0)
	  (sym-table-set! dot-text 0 0 0 stt-section stb-local
			  (entry-ref sh-table-entries dot-text))
	  (push-stack-append! image (sassy-text-stack sassy-output))
	  (push-stack-align image 16 0)))

; Handle the symbol table

    (call-with-values
	(lambda () (split-list-of-records
		    (hash-table-values (sassy-symbol-table sassy-output))
		    sassy-symbol-scope
		    'local))
      (lambda (locals globals)
	(define last-local (number-of-entries sym-table-entries))
	(make-sym-entries (quicksort-records locals sassy-symbol-name symbol<=?)
			  stb-local)
	(set! last-local (number-of-entries sym-table-entries))
	(make-sym-entries (quicksort-records globals
					     sassy-symbol-name
					     symbol<=?)
			  stb-global)
	(section-header-set! dot-symtab
			     (string-table-set! sh-strtab dot-symtab)
			     sht-symtab
			     0
			     0
			     (push-stack-size image)
			     (push-stack-size sym-table)
					;strtab up next
			     (+ 1 (number-of-entries sh-table-entries)) 
			     last-local
			     4
			     16)
	(push-stack-append! image sym-table)
	(push-stack-align image 16 0)))

; Handle strtab

    (section-header-set! dot-strtab
			 (string-table-set! sh-strtab dot-strtab)
			 sht-strtab
			 0
			 0
			 (push-stack-size image)
			 (push-stack-size strtab)
			 0
			 0
			 1
			 0)
    (push-stack-append! image strtab)
    (push-stack-align image 16 0)

; Handle the relocations table
    
    (call-with-values
	(lambda () (split-list-of-records (sassy-reloc-list sassy-output)
					  sassy-reloc-section
					  'data))
      (lambda (datas texts)
	(if (not (null? datas))
	    (let ((current-offset (push-stack-size image)))
	      (for-each push-reloc (quicksort-records datas sassy-reloc-offset
						      <=))
	      (section-header-set! dot-rel-data
				   (string-table-set! sh-strtab dot-rel-data)
				   sht-rel
				   0
				   0
				   current-offset
				   (- (push-stack-size image) current-offset)
				   (entry-ref sh-table-entries dot-symtab)
				   (entry-ref sh-table-entries dot-data)
				   4
				   8)
	      (push-stack-align image 16 0)))
	(if (not (null? texts))
	    (let ((current-offset (push-stack-size image)))
	      (for-each
	       (lambda (reloc)
; skip 'rel relocs in the text section unless their targets are
; symbols with an unknown offset (imported, or undefined and exported,
; like _GLOBAL_OFFSET_TABLE)
		 (when (not (and (eq? 'rel (sassy-reloc-type reloc))
				 (or (and (sassy-reloc-name reloc)
					  (sassy-symbol-offset
					   (hash-table-ref (sassy-symbol-table
							    sassy-output)
							   (sassy-reloc-name
							    reloc)
							   (lambda () #f))))
				     (not (sassy-reloc-name reloc)))))
		       (push-reloc reloc)))
	       (quicksort-records texts sassy-reloc-offset <=))
	      (section-header-set! dot-rel-text
				   (string-table-set! sh-strtab dot-rel-text)
				   sht-rel
				   0
				   0
				   current-offset
				   (- (push-stack-size image) current-offset)
				   (entry-ref sh-table-entries dot-symtab)
				   (entry-ref sh-table-entries dot-text)
				   4
				   8)
	      (push-stack-align image 16 0)))))

; Handle the optional user section (sht_louser)

    (if (not (null? user-section))
	(let ((user-section-name (car user-section))
	      (user-section-data (cadr user-section)))
	  (section-header-set! user-section-name
			       (string-table-set! sh-strtab user-section-name)
			       sht-louser
			       0
			       0
			       (push-stack-size image)
			       (string-length user-section-data)
			       0
			       0
			       1
			       0)
	  (let ((lng (string-length user-section-data)))
	    (do ((i 0 (+ i 1)))
		((= i lng))
	      (push-stack-push
	       image
	       (char->integer (string-ref user-section-data i))))
	    (push-stack-align image 16 0))))
    

; Handle sh-strtab

    (let ((index (string-table-set! sh-strtab dot-shstrtab)))
      (section-header-set! dot-shstrtab
			   index
			   sht-strtab
			   0
			   0
			   (push-stack-size image)
			   (push-stack-size sh-strtab)
			   0
			   0
			   1
			   0)
      (push-stack-append! image sh-strtab)
      (push-stack-align image 16 0))

; Patch the elf-header

    (e-shoff-patcher (number->byte-list (push-stack-size image) 4))
    (e-shnum-patcher (number->byte-list (number-of-entries sh-table-entries) 2))
    (e-shstrndx-patcher (number->byte-list (entry-ref sh-table-entries
						      dot-shstrtab)
					   2))

; Handle the section-header table ...

    (push-stack-append! image sh-table)

; ... and A-WAY-YAY we go!!!!

    (if (file-exists? output-file)
	(delete-file output-file))
    
    (with-output-to-file output-file
      (lambda ()
	(for-each (lambda (byte)
		    (write-byte byte))
		  (push-stack-items image))))
    
    )) ;end sassy-make-elf
