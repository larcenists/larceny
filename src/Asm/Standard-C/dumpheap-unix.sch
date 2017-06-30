; 29 August 2003
;
; Routines for dumping a Petit Larceny heap image using some standard
; C compiler under Unix-like systems (Unix, Linux, MacOS X).

; Hook for a list of libraries for your platform.  This is normally
; set by code in the petit-*-*.sch file, after this file is loaded.

(define unix/petit-lib-library-platform '("-lm"))

; Hook for a set of switches that tells the compiler where to look for
; standard include files.  If twobit is not used for compiler
; development then this is usually set to reference the 'include' dir
; of the Larceny install directory.

(define unix/petit-include-path "-Iinclude -Iinclude/Sys -Iinclude/Standard-C -Iinclude/Shared")

; Hooks for library names.  These are normally set by code in the 
; petit-*-*.sch file, after this file is loaded.

(define unix/petit-rts-library (param-filename 'rts "libpetit.a"))
(define unix/petit-lib-library "libheap.a")

; Hook called from dumpheap-extra.sch to create the heap library file

(define (build-petit-larceny heap output-file-name input-file-names)
  (c-link-library unix/petit-lib-library
		  (remove-duplicates
		   (append (map (lambda (x)
				  (rewrite-file-type x ".lop" ".o"))
				input-file-names)
			   (list (rewrite-file-type *temp-file* ".c" ".o")))
		   string=?)
		  '()))

; General interface for creating an executable containing the standard
; libraries and some additional files.

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

; General classification of a Unix system, a little more fine grained than 
; SYSTEM-FEATURES currently provides.  A hack, really.

(define (classify-unix-system)
  (cond ((string=? "MacOS X" (cdr (assq 'os-name (system-features))))
         'macosx)
        ((and (string=? "BSD Unix" (cdr (assq 'os-name (system-features))))
	      (file-exists? "/Desktop"))
	 'macosx)
	((string=? "SunOS" (cdr (assq 'os-name (system-features))))
	 'sunos)
	((zero? (system "test \"`uname`\" = \"Linux\""))
	 'linux)
	(else
	 'generic)))
  
; Compiler definitions

(define (c-compiler:gcc-unix c-name o-name)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC -falign-functions=4 -c ~a ~a -D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wimplicit ~a -o ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    unix/petit-include-path
    (if (optimize-c-code) "-O3 -DNDEBUG" "")
    o-name
    c-name)))

(define (c-library-linker:gcc-unix output-name object-files libs)
  (execute 
   (twobit-format #f
		  "ar -r ~a ~a"
		  output-name (apply string-append 
				     (insert-space object-files))))
  (execute
   (twobit-format #f
		  "ranlib ~a"
		  output-name)))

(define (c-linker:gcc-linux output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC ~a -rdynamic -o ~a ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define (c-linker:gcc-macosx output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC ~a -o ~a ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define (c-linker:gcc-unix output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC ~a -Wl,-export-dynamic -o ~a ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define (c-so-linker:gcc-unix output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC ~a -shared -o ~a ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define (c-so-linker:gcc-linux output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC ~a -shared -o ~a ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

; Known to work with 10.2.8

(define (c-so-linker:gcc-macosx output-name object-files libs)
  (execute
   (twobit-format 
    #f
    "gcc -m32 -fPIC ~a -flat_namespace -bundle -undefined suppress -o ~a ~a ~a"
    (if (optimize-c-code) "" "-gstabs+")
    output-name
    (apply string-append (insert-space object-files))
    (apply string-append (insert-space libs)))))

(define-compiler 
  "GCC under Unix"
  'gcc
  ".o"
  (let ((host-os (classify-unix-system))
        (match-gcc-version?
         (lambda (ver-string)
           (let ((cmd (string-append "gcc --version | grep '" ver-string "'")))
             (zero? (system cmd))))))
    `((compile            . ,c-compiler:gcc-unix)
      (link-library       . ,c-library-linker:gcc-unix)
      (link-executable    . ,(case host-os
			       ((macosx) c-linker:gcc-macosx)
			       ((linux) c-linker:gcc-linux)
			       (else    c-linker:gcc-unix)))
      (link-shared-object . ,(case host-os
			       ((macosx) c-so-linker:gcc-macosx)
			       (else     c-so-linker:gcc-unix)))
      (append-files       . ,append-file-shell-command-unix)
      (make-configuration . ,(cond 
			      ((match-gcc-version? " 3.")
			       'petit-unix-static-gcc-v3)
			      ((match-gcc-version? " 4.")
			       'petit-unix-static-gcc-v4)
			      (else
			       (error 'dumpheap-unix.sch 
				      "Unmatched GCC version."))
			      )))))

; eof

