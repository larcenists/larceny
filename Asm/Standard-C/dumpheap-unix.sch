; 9 November 2002
;
; Routines for dumping a Petit Larceny heap image using
; some standard C compiler under Unix or MacOS X.

(define unix/petit-rts-library "Rts/libpetit.a")
(define unix/petit-lib-library "libheap.a")
(define unix/petit-exe-name    "petit")
(define unix/petit-lib-library-platform '()) ; Set by platform code

(define (build-petit-larceny heap output-file-name input-file-names)
  (build-petit-lib-library input-file-names))

(define (build-petit-lib-library input-file-names)
  (c-link-library unix/petit-lib-library
		  (remove-duplicates
		   (append (map (lambda (x)
				  (rewrite-file-type x ".lop" ".o"))
				input-file-names)
			   (list (rewrite-file-type *temp-file* ".c" ".o")))
		   string=?)
		  '()))

(define (build-application executable-name lop-files)
  (let ((src-name (rewrite-file-type executable-name '("") ".c"))
        (obj-name (rewrite-file-type executable-name '("") ".o")))
    (init-variables)
    (for-each create-loadable-file lop-files)
    (dump-loadable-thunks src-name)
    (c-compile-file src-name obj-name)
    (c-link-executable executable-name
                       (cons obj-name
                             (map (lambda (x)
                                    (rewrite-file-type x ".lop" ".o"))
                                  lop-files))
                       `(,unix/petit-rts-library
                         ,unix/petit-lib-library
                         ,@unix/petit-lib-library-platform))
    executable-name))

(define (create-indirect-file filename object-files)
  (delete-file filename)
  (call-with-output-file filename
    (lambda (out)
      (for-each (lambda (x)
		  (display x out)
		  (newline out))
		object-files))))

; Compiler definitions

(define gcc-name
  (case (nbuild-parameter 'host-os)
    ((macosx) "cc")     ; Apple brain damage
    (else     "gcc")))

(define (c-compiler:gcc-unix c-name o-name)
  (execute
   (twobit-format 
    #f
    "~a -c -gstabs+ -IRts/Sys -IRts/Standard-C -IRts/Build -D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wimplicit ~a -o ~a ~a"
    gcc-name
    (if (optimize-c-code) "-O3 -DNDEBUG" "")
    o-name
    c-name)))

(define (c-library-linker:gcc-unix output-name object-files libs)
  (execute 
   (twobit-format 
    #f
    "ar -r ~a ~a; ranlib ~a"
    output-name
    (apply string-append (insert-space object-files))
    output-name)))

(define (c-linker:gcc-unix output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "~a -gstabs+ -o ~a ~a ~a"
    gcc-name
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define (c-so-linker:gcc-unix output-name object-files libs)
  (error "Don't know how to build a shared object under generic unix"))

(define (c-so-linker:gcc-macosx output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "~a -gstabs+ -r -shared -o ~a ~a ~a"
    gcc-name
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define-compiler 
  "GCC under Unix"
  'gcc
  ".o"
  `((compile            . ,c-compiler:gcc-unix)
    (link-library       . ,c-library-linker:gcc-unix)
    (link-executable    . ,c-linker:gcc-unix)
    (link-shared-object . ,(case (nbuild-parameter 'host-os)
			     ((macosx) c-so-linker:gcc-macosx)
			     (else     c-so-linker:gcc-unix)))
    (append-files       . ,append-file-shell-command-unix)))

(select-compiler 'gcc)

; eof

