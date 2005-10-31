; 15 June 2005
;
; Routines for dumping a Petit Larceny heap image using NASM for x86
; Windows systems

; Hook for a list of libraries for your platform.  This is normally
; set by code in the nasm-*.sch file, after this file is loaded.

(define win32/petit-lib-library-platform '())

; Hook for a set of switches that tells the compiler where to look for
; standard include files.  If twobit is not used for compiler
; development then this is usually set to reference the 'include' dir
; of the Larceny install directory.

(define win32/petit-include-path "-IRts\\Sys\\ -IRts\\Standard-C\\ -IRts\\Build\\")

; Hooks for library names.  These are normally set by code in the 
; nasm-*.sch file, after this file is loaded.

(define win32/petit-rts-library "Rts\\libpetit.a")
(define win32/petit-lib-library "libheap.a")

; Hook called from dumpheap-extra.sch to create the heap library file

(define (build-petit-larceny heap output-file-name input-file-names)
  (c-link-library win32/petit-lib-library
		  (remove-duplicates
		   (append (map (lambda (x)
				  (rewrite-file-type x ".lop" ".obj"))
				input-file-names)
			   (list (rewrite-file-type *temp-file* ".asm" ".obj")))
		   string=?)
		  '()))

; General interface for creating an executable containing the standard
; libraries and some additional files.

(define (build-application executable-name lop-files)
  (let ((src-name (rewrite-file-type executable-name '("") ".asm"))
        (obj-name (rewrite-file-type executable-name '("") ".obj")))
    (init-variables)
    (for-each create-loadable-file lop-files)
    (dump-loadable-thunks src-name)
    (c-compile-file src-name obj-name)
    (c-link-executable executable-name
                       (cons obj-name
                             (map (lambda (x)
                                    (rewrite-file-type x ".lop" ".obj"))
                                  lop-files))
                       `(,win32/petit-rts-library
                         ,win32/petit-lib-library
                         ,@win32/petit-lib-library-platform))
    executable-name))

; Compiler definitions

(define (assembler:nasm-win32 asm-name o-name)
  (execute
   (twobit-format 
    #f
    "nasmw -O1 -f win32 -IRts\\Intel\\ -IRts\\Build\\ -o \"~a\" \"~a\""
    o-name
    asm-name)))

(define (create-indirect-file filename object-files)
  (delete-file filename)
  (call-with-output-file filename
    (lambda (out)
      (for-each (lambda (x)
		  (twobit-format out "\"~a\"~%" x))
		object-files))))

(define (c-library-linker:msvc-win32 output-name object-files libs)
  (let ((lnk-name (rewrite-file-type output-name ".lib" ".lnk"))
	(lib-name (rewrite-file-type output-name ".lib" "")))
    (create-indirect-file lnk-name object-files)
    (delete-file output-name)
    (execute
     (twobit-format #f "lib.exe /libpath:. /name:\"~a\" /out:\"~a\" @\"~a\"" lib-name output-name lnk-name))))

(define (c-linker:msvc-win32 output-name object-files libs)
  (create-indirect-file "petit-objs.lnk" object-files)
  (system "del vc60.pdb")
  (execute
   (twobit-format #f
		  "link.exe /debug /export:mc_alloc /out:\"~a\" @petit-objs.lnk \"~a\""
		  output-name
		  (apply string-append (insert-space libs)))))

(define (c-dll-linker:msvc-win32 output-name object-files . ignored)
  ;; FIXME!!  (why??)
  (create-indirect-file "petit-objs.lnk" object-files)
  (system "del vc60.pdb")
  (execute
   (twobit-format #f
		  "link.exe /dll /noentry /export:twobit_load_table /debug /out:\"~a\" @petit-objs.lnk petit.lib"
		  output-name)))

(define-compiler 
  "NASM+Microsoft Visual C++ under Win32"
  'nasm+msvc
  ".obj"
  (let ((host-os (nbuild-parameter 'host-os)))
    `((compile            . ,assembler:nasm-win32)
      (link-library       . ,c-library-linker:msvc-win32)
      (link-executable    . ,c-linker:msvc-win32)
      (link-shared-object . ,c-dll-linker:msvc-win32)
      (append-files       . ,append-file-shell-command-msdos)
      (make-configuration . X86-WIN32-STATIC-VISUALC-NASM))))

; eof

