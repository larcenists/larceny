; flat-bin.scm - create bin files from Sassy's output
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


; module flat-bin
; import api
; export all



; This is a good demonstration of how to use the output API, and we
; should probably include it in chapter 7 of the docs.

; usage:

; procedure:
; (sassy-make-bin output-file sassy-output opts ...) => unspecified

; Combines the text and data sections of sassy-output and writes a
; flat binary file to output-file. If the file already exists it is
; deleted. Any (org) directive is taken into account, and relocations
; are performed on absolute references in the data section. The data
; segment is placed (and aligned properly) immediately after the text
; segment.

; opts can be none, one or both of the following quoted symbols:

; 'boot : Make the file be an x86 boot-sector. That is, zero-fill the
;         remainder of the file to 510 bytes, and add the x86 boot-sector
;         signature #x55aa to bytes 511 and 512.

; 'stats :  Display some stats about the output.



(define (sassy-make-bin output-file sassy-output . opts)

  (define boot-sector? (memq 'boot opts))
  (define stats?       (memq 'stats opts))

  (define data? (not (zero? (sassy-data-size sassy-output))))

  ; Grap some info for printout stats later.

  (define text-size (sassy-text-size sassy-output))
  (define data-size (sassy-data-size sassy-output))
  (define text-pad  0)

  (define (display-stat . itms)
    (for-each display itms)
    (newline))

  (define (needed-reloc? x)
    (and (eq? 'abs (sassy-reloc-type x))
	 (let ((name (sassy-reloc-name x)))
	   (if name
	       (let ((symbol (sassy-symbol-exists? sassy-output name)))
		 (and (eq? 'data (sassy-symbol-section symbol))
		      (not (eq? 'import (sassy-symbol-scope symbol)))))
	       (eq? 'data (sassy-reloc-section x))))))

  ; Align the end of the text-section to the align of the data section.
  ; The data section will begin at this point in the file.
  ; (nop) is used as the filler.

  (when data?
	(push-stack-align (sassy-text-stack sassy-output)
			  (sassy-data-align sassy-output)
			  #x90))

  (set! text-pad (- (sassy-text-size sassy-output) text-size))

  ; Since the text-section is going to be loaded at whatever the given
  ; (org) was, all absolute relocations in the text section that refer
  ; to other locations in the text section already have the offset of
  ; (org) added to them, so we don't have to apply relocations to
  ; those. And we don't need to relocate relative addresses in the
  ; text section (and relative relocations aren't allowed in the data
  ; section).

  ; So, we only need to relocate references to symbols defined in the data
  ; section, or anonymous relocs in the data section

  ; Also, we need to grab all those references both from the
  ; text-section _and_ the _data_ section
  
  (when data?
	(let ((data-relocs-to-do
	       (filter needed-reloc? (sassy-reloc-list sassy-output))))

    ; Now we get ready to apply the relocations taking into account the new
    ; end of the text-section (or beginning of the data-section).

    ; For each reloc-to-do, we're going to apply it's patcher
    ; to the the data-offset plus the value already there

	  (let* ((text-offset (sassy-text-org sassy-output))
		 (data-offset (+ text-offset (sassy-text-size sassy-output))))
	    (for-each (lambda (reloc)
			((sassy-reloc-patcher reloc)
			 (+ data-offset (sassy-reloc-value reloc))))
		      data-relocs-to-do))

      ; Now all we have to to is append the data to the text, mark it as
      ; a boot sectior, and spit it out.

      ; The fastest way to tack a data section on to a text section is
      ; the following
      ; !!!!NOTE: This actually alters the text section (append!)
	  (push-stack-append! (sassy-text-stack sassy-output)
			      (sassy-data-stack sassy-output))))
      
      (when boot-sector?
	    ; sanity check
	    (if (> (sassy-text-size sassy-output) 510)
		(error "segment too big for a boot sector")
		(begin
 		; mark it as a boot sector
		  (push-stack-align (sassy-text-stack sassy-output) 510 0)
		  (push-stack-push (sassy-text-stack sassy-output)
				   (list #x55 #xaa)))))

      ; dump to file
      (when (file-exists? output-file)
	    (delete-file  output-file))
      (with-output-to-file output-file
	(lambda ()
	  (for-each write-byte (sassy-text-list sassy-output))))

    (when stats?
	  (display-stat "Text size: "    text-size " bytes")
	  (display-stat "Data size: "    data-size " bytes")
	  (display-stat "Data align: "
			(sassy-data-align sassy-output)
			" byte boundary")
	  (display-stat
	   "Total size: "
	   (+ text-size data-size text-pad)
	   " bytes, with " text-pad " bytes of padding in the text section.")
	  (when boot-sector?
		(display-stat "Made a boot sector"))))
