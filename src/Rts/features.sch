; Larceny configuration.
;
; When adding support for a new platform, you must define an attribute
; set that describes the platform for which you are building Larceny.
; The definition appears in the "User Definition Section", below.
;
;          DON'T PANIC! 
;
; Useful sets of attributes for many systems are defined in that
; section, just pick the one appropriate to your system.  Read the
; instructions at the head of that section carefully.

; TODO / FIXME
; - could also generate Rts/Makefile
; - we should get rid of all negative options, as they are too often confusing

; List of available features, with explanations.

(define features '(

 ; Architectures.  You need one of these.

 "SPARC" 			; Native: SPARC v8 or later
 "X86_SASSY"			; Native: Intel 486 using Sassy assembler
 "X86_NASM"                     ; Native: Intel 386 using NASM macro assembler
 "PPC_GAS"                      ; Native: PowerPC using GNU 'as'
 "PETIT_LARCENY"		; Portable: Hardware is irrelevant

 ; Architecture attributes.  You need bits and endianness at least.

 "BITS_32"			; 32-bit words
 "BITS_64"			; 64-bit words
 "ENDIAN_LITTLE"		; Least significant byte at lowest address
 "BIG_ENDIAN"		        ; Most significant byte at lowest address
 "EXPLICIT_DIVZ_CHECK"          ; Explicitly check for integer division by zero
				; Some systems, like the PPC, do not trap
				; integer division by zero
 "FLUSH_ALWAYS"                 ; Instruction cache flushing is normally
 "FLUSH_NEVER"                  ; handled by the run-time system and you don't
                                ; need to select either of these.  But if the
                                ; RTS cannot figure it out, then set 
				; FLUSH_ALWAYS to 1 to force flushing, and set
                                ; FLUSH_NEVER to 1 to disable it completely.
                                ; (The only system that is yet known to 
                                ; require these is the early SPARC
                                ; multiprocessors running SunOS 4.)
 "HARDWARE_DIVISION"            ; Set to 1 to use hardware division even
				; if that compromises backward compatibility.
				; Some RISC systems implement HW division
				; only in later architecture versions
 "SPARCV9"                      ; Use SPARC v9 instructions (default is SPARC 
                                ; v8 only)
 "PENTIUM"                      ; Use Pentium instructions (default is 386
                                ; only)

 ; Operating systems.  You need one of these.

 "SUNOS4"			; SunOS 4.x
 "SUNOS5"			; SunOS 5.x aka Solaris 2.x
 "LINUX"			; Generic GNU/Linux
 "MACOS_X"			; MacOS X *only* (~BSD (4.3ish) Unix)
 "BSD_UNIX"			; Generic BSD (4.3ish) Unix, also MacOS X
 "POSIX_UNIX"		        ; Generic POSIX-standard Unix
 "XOPEN_UNIX"		        ; Generic XOPEN-standard Unix
 "CYGWIN"                       ; Generic Unix with some twists
 "WIN32"			; Generic Windows 32-bit
 "MACOS"			; Generic Macintosh OS 9.x or earlier
 "GENERIC_OS"		        ; Anything else

 ; Synthesized OS names, don't define these yourself

 "SUNOS"			; Synthesized
 "UNIX"			        ; Synthesized

 ; Signal handling.  If you don't choose one, the script will guess.

 "BSD_SIGNALS"		        ; sigvec, as described by Leffler et al
 "STDC_SIGNALS"		        ; signal, as in ANSI/ISO C (weak)
 "POSIX_SIGNALS"		; sigaction + sa_handler, as described 
				; by Lewine
 "XOPEN_SIGNALS"		; sigaction + sa_sigaction
 "WIN32_SIGNALS"                ; another world altogether

 ; Other configuration options.

 "DOF_COLLECTOR"
    ; When set, includes the deferred-older-first collector when
    ; compiling the system.
    ;
    ; Recommended setting is off.

 "ROF_COLLECTOR"
    ; When set, includes the renewal-older-first collector when
    ; compiling the system.
    ; 
    ; Recommended setting is off.

 "STACK_UNDERFLOW_COUNTING"
    ; When set, enables stack underflow accounting.  (The performance
    ; impact of this is negligible.)
    ; 
    ; Recommended setting is on.

 "GCLIB_LARGE_TABLE"
    ; When set, preallocates a page table for the entire 4GB address
    ; space rather than allocating it lazily.  The table uses one byte
    ; per 4KB page, for a total of 1MB.  Enabling this option will
    ; probably reduce the cost of the write barrier both in the
    ; mutator and in the garbage collector, and reduce the register
    ; pressure in the inner loops of the collector.
    ; 
    ; Recommended setting is off, as it needs further evaluation.

 "RETURN_MEMORY_TO_OS"
    ; When set, the lowlevel memory manager eagerly returns memory
    ; blocks to the operating system when they are released by the
    ; garbage collector.
    ; 
    ; Recommended setting is off, since it tends to increase execution
    ; time due to added system overhead.
    ; 
    ; Even if off, the memory manager may return memory blocks to the
    ; operating system.  Even if on, the memory manager is not
    ; guaranteed to release memory.

 "GC_HIRES_TIMERS"
    ; When set, high-resolution timers for various parts of the
    ; garbage collector are enabled.  Only effective if the platform
    ; has high resolution timers.
    ;
    ; Recommended setting is off, unless you're doing GC research.

 "GC_EVENT_COUNTERS"
    ; When set, the garbage collector cores count the number of
    ; pointers copied, the number of those pointers that hit the write
    ; barrier (if applicable), and similar things.  Turning this on
    ; will slow down garbage collection, though I don't know by how
    ; much.
    ;
    ; Recommended setting is off, even if you're doing GC research.

 "USE_GENERIC_ALLOCATOR"
    ; When set, use a generic malloc-based low-level memory allocator
    ; rather than any OS-specific allocator.  The functions selected
    ; are osdep_alloc_aligned, osdep_free_aligned, and
    ; osdep_fragmentation.
    ;
    ; The generic allocator is selected automatically if the operating
    ; system is GENERIC_OS.
    ;
    ; (The generic allocator is in osdep-generic.c, and that file must
    ; be included in the compilation along with the osdep-* file for
    ; the appropriate operating system, unless the system is being
    ; compiled for a generic OS.  Normally there is no reason to worry
    ; about this as the Makefiles do the right thing.)
    ;
    ; Recommended setting is off, since the non-generic allocators are
    ; generally better.

 "USE_GENERIC_IO"
    ; When set, use a generic stdio-based low-level I/O system rather
    ; than any OS-specific I/O system.  The functions selected are
    ; osdep_openfile, osdep_closefile, osdep_readfile,
    ; osdep_writefile, and osdep_pollinput.
    ; 
    ; The generic I/O system is selected automatically if the
    ; operating system is GENERIC_OS.
    ; 
    ; Also see comments for USE_GENERIC_ALLOCATOR.
    ;
    ; Recommended setting is off, since the non-generic I/O is
    ; generally better.

 "USE_GENERIC_FILESYSTEM"
    ; When set, use generic low-level file-system support rather than
    ; any OS-specific file-system support.  The functions selected are
    ; osdep_unlinkfile, osdep_mtime, osdep_access, and osdep_rename.
    ; 
    ; The generic file system is selected automatically if the
    ; operating system is GENERIC_OS.
    ;
    ; Also see comments for USE_GENERIC_ALLOCATOR.
    ;
    ; Recommended setting is off, since the non-generic file system is
    ; generally better.

 "USE_STDIO"
    ; Console input is by default through stdin, and console output is 
    ; by default through stdout.  Used by generic I/O subsystem.
    ;
    ; Recommended setting is off.

 ; Petit Larceny options.  You do not need to select any of these
 ; unless you have special needs or are doing research.  The defaults
 ; (set in Rts/Standard-C/petit-config.h) are USE_RETURN_WITH_VALUE
 ; and USE_GOTOS_LOCALLY.  The defaults are OK for both portability
 ; and performance.

 "USE_LONGJUMP"
    ; Jump, invoke and return are implemented as calls; when the timer
    ; expires, a longjump is performed to prune the stack.  The jump 
    ; address is passed in a global variable.

 "USE_RETURN_WITHOUT_VALUE"
    ; Jump, invoke, and return are implemented as returns to a dispatch 
    ; loop, with the jump address passed in a global variable.

 "USE_RETURN_WITH_VALUE"
    ; Jump, invoke, and return are implemented as returns to a dispatch
    ; loop, with the jump address passed as a return value to the loop.

 "USE_GOTOS_LOCALLY"
    ; If set, distinguish between local control transfers (BRANCH, 
    ; BRANCHF, and SKIP) and nonlocal control transfers (INVOKE, RETURN, 
    ; APPLY, and JUMP).  The nonlocal transfers uses the discipline 
    ; selected above; the local transfers use GOTO.
    ;
    ; This local control transfer discipline improves performance, reduces
    ; code size, makes register caching worthwhile, and reduces the number 
    ; of function pointers (which makes life easier on MacOS, at least).

 "USE_CACHED_STATE"
    ; If set, C functions generated by Petit Larceny will cache a few
    ; virtual machine registers in local variables.  This will tend to
    ; increase code size and performance.
    ;
    ; Recommended setting is on.

 "DYNAMIC_LOADING"
    ; If set, allow .FASL files that reference external shared object files
    ; to be loaded.  This does not work on all systems because the addresses
    ; into the shared objects cannot be represented properly in Larceny's
    ; data structures.

 ; Special system attributes -- for use of non-portable extensions or 
 ; bug workarounds, or other weirdness.

 "CODEPTR_SHIFT1"
    ; Petit Larceny: C procedure addresses are aligned to 2-byte
    ; boundaries, so must be shifted left 1 bit when stored in
    ; Scheme data structures.  Depends on the high bit of a procedure
    ; address always being 0.

 "CODEPTR_SHIFT2"
    ; Petit Larceny: C procedure addresses are unaligned, so must be 
    ; shifted left 2 bits when stored in Scheme data structures.  Depends
    ; on the two high bits of a procedure address always being 0.

 "CODEWARRIOR"
    ; Metrowerks Codewarrior extensions.  Currently this is required
    ; for Petit Larceny on the Mac (MacOS 9 and earlier).

 "DEC_ALPHA_32BIT"
    ; DEC Alpha, in 32-bit mode.  This is needed to cope with some mixed
    ; word-length weirdness on that machine.

 "NO_ATOMIC_ALLOCATION"
    ; Boehm/Demers/Weiser conservative collector: do not allocate 
    ; bytevectors as atomic data.
    ;
    ; Recommended setting is off, and turning it on can have severe
    ; negative effects on GC overhead and heap size (as a result of
    ; false pointers).

 "NO_SYNCHRONOUS_SIGNALS"
    ; Disable the use of longjump out of signal handlers.  This may
    ; result in the loss of interruptibility in some cases and
    ; performance reduction in others, but for now it is required
    ; to deal with signals on those platforms (WinNT and Win2k) that
    ; run signal handlers in a separate thread.

 "DEBIAN_STRDUP_WEIRDNESS"
    ; Define this feature to prevent larceny from declaring strdup().
    ; The Debian header files define strdup() with a signature that
    ; is not compatible with the definition Larceny uses.

 "SIMULATE_NEW_BARRIER"
    ; Enable the simulation of Clinger's write barrier.  This will slow
    ; down execution substantially.  Useful for research only, and not
    ; supported by all the collectors.
    ;
    ; Recommended setting is off.

 ; Library/feature attributes.  These macros declare the existence of
 ; particular functions in the run-time library or as compiler
 ; intrinsics.  It's always OK to leave these macros undefined, as
 ; Larceny will supply portable definitions.  It may be desirable for
 ; performance or functionality reasons to define these macros below, 
 ; if your library or compiler supplies any of the functions -- for
 ; example, the portable definition of gethrtime() always returns 0,
 ; and the portable definition of stat() always returns the same
 ; file modification time.

 "HAVE_RINT"		; Library has rint() -- round to even
 "HAVE_AINT"		; Library has aint() -- round to zero
 "HAVE_STRNCASECMP"	; Library has strncasecmp()
 "HAVE_STRDUP"		; Library has strdup()
 "HAVE_HRTIME_T"        ; Library has hrtime_t and gethrtime(),
			; and one can get hrtime_t by including
			; <sys/time.h>
 "HAVE_STAT"            ; Library has 'struct stat' and stat(),
			; and one can get at them by including
			; <stat.h>
 "HAVE_POLL"            ; Library has poll()
 "HAVE_SELECT"          ; Library has select()
 "HAVE_DLFCN"		; Library has dlfcn.h, dlopen(), and dlsym()
))


; USER DEFINITION SECTION
;
; Choose an existing feature set, or make your own.
;
; Then change the value of the variable SELECTED-FEATURE-SET, below,
; to reference your preferred feature set.
;
; If you make your own feature set, you can choose from the features
; in the list above.  If you do not define the type of signal handling
; you want, then the script will attempt to guess based on your
; operating system.
;
; For Petit Larceny, selecting the precise operating system is not
; crucial; for example, POSIX_UNIX or BSD_UNIX will work OK on
; platforms that conform.  For native systems, you need to be more
; specific, so define SUNOS4/SUNOS5 as appropriate for the SPARC
; native version.

; Here are some sets of settings that work for me.

(define features-sparc-solaris		; solaris 2.5 or later
  '("SPARC"
    "SUNOS5"
    "BITS_32" 
    "BIG_ENDIAN" 
    "HAVE_RINT"
    "HAVE_STRDUP"
    "HAVE_STRNCASECMP"
    "HAVE_HRTIME_T"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "GC_HIRES_TIMERS"
    ))

(define features-petit-solaris		; solaris 2.5 or later
  '("PETIT_LARCENY"
    "SUNOS5"
    "BITS_32"
    "BIG_ENDIAN" 
    "HAVE_RINT"
    "HAVE_STRDUP"
    "HAVE_STRNCASECMP"
    "HAVE_HRTIME_T"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "GC_HIRES_TIMERS"
    "USE_CACHED_STATE"
    ))

(define features-petit-macosx		; gcc and GNU libc
  '("PETIT_LARCENY"
    "BITS_32"
    "BIG_ENDIAN"
    "MACOS_X"
    "BSD_UNIX"
    "HAVE_RINT"
    "HAVE_STRDUP"
    "HAVE_STRNCASECMP"
    "HAVE_SELECT"
    "HAVE_DLFCN"                        ; only if you have installed the dlcompat package
    "DYNAMIC_LOADING"                   ; only makes a difference if HAVE_DLFCN is defined
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"		; some weirdness with mmap
    "USE_CACHED_STATE"
    ))

(define features-petit-macosx-el		; gcc and GNU libc
  '("PETIT_LARCENY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "MACOS_X"
    "BSD_UNIX"
    "HAVE_RINT"
    "HAVE_STRDUP"
    "HAVE_STRNCASECMP"
    "HAVE_SELECT"
    "HAVE_DLFCN"                        ; only if you have installed the dlcompat package
    "DYNAMIC_LOADING"                   ; only makes a difference if HAVE_DLFCN is defined
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"		; some weirdness with mmap
    "USE_CACHED_STATE"
    ))

(define features-petit-win32		; works for Mingw; believed to work
                                        ; for CodeWarrior 6, and probably
                                        ; for Microsoft Visual C/C++ 6
  '("PETIT_LARCENY"
    "WIN32"
    "BITS_32"
    "ENDIAN_LITTLE"
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"
    "USE_GENERIC_IO"
    "USE_GENERIC_FILESYSTEM"
    "USE_STDIO"
    "NO_SYNCHRONOUS_SIGNALS"
    "HAVE_STAT"
;    "HAVE_RINT"
;    "HAVE_STRNCASECMP"
    "USE_CACHED_STATE"
    "CODEPTR_SHIFT2"
    ))

(define features-petit-linux		; Debian GNU/Linux 3.0 (woody), x86
  '("PETIT_LARCENY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "LINUX"
    "HAVE_RINT"
    "HAVE_STRNCASECMP"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "DEBIAN_STRDUP_WEIRDNESS"
    "USE_CACHED_STATE"
    ))

(define features-petit-cygwin		; Tested with cygwin 1.5.10 (May 2004)
  '("PETIT_LARCENY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "CYGWIN"
    "HAVE_RINT"
    "HAVE_STRNCASECMP"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    ;"DYNAMIC_LOADING"                  ; Never tested
    "STACK_UNDERFLOW_COUNTING"
    "DEBIAN_STRDUP_WEIRDNESS"
    "USE_CACHED_STATE"
    ))

(define features-x86-nasm-linux		; Debian GNU/Linux 3.0 (woody), x86
  '("X86_NASM"
    "BITS_32"
    "ENDIAN_LITTLE"
    "LINUX"
    "HAVE_RINT"
    "HAVE_STRNCASECMP"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "DEBIAN_STRDUP_WEIRDNESS"
    ))

(define features-x86-sassy-linux		; Debian GNU/Linux 3.0 (woody), x86
  '("X86_SASSY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "LINUX"
    "HAVE_RINT"
    "HAVE_STRNCASECMP"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "DEBIAN_STRDUP_WEIRDNESS"
    ))

(define features-x86-sassy-macosx
  '("X86_SASSY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "MACOS_X"
    "BSD_UNIX"
    "HAVE_RINT"
    "HAVE_STRNCASECMP"
    "HAVE_STRDUP"
    "HAVE_SELECT"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"		; some weirdness with mmap
    "USE_CACHED_STATE"
    ))

(define features-x86-nasm-win32		; Windows, x86
  '("X86_NASM"
    "BITS_32"
    "ENDIAN_LITTLE"
    "WIN32"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"
    "USE_GENERIC_IO"
    "USE_GENERIC_FILESYSTEM"
    "USE_STDIO"
    "NO_SYNCHRONOUS_SIGNALS"
    "HAVE_STAT"
    ))

(define features-x86-sassy-win32	; Windows, x86
  '("X86_SASSY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "WIN32"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"
    "USE_GENERIC_IO"
    "USE_GENERIC_FILESYSTEM"
    "USE_STDIO"
    "NO_SYNCHRONOUS_SIGNALS"
    "HAVE_STAT"
    ))


(define features-petit-linux-redhat5	; Very old, Redhat linux 5.1
  '("PETIT_LARCENY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "LINUX"
    "HAVE_RINT"
    "HAVE_STRNCASECMP"
    "HAVE_STRDUP"
    "HAVE_POLL"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    ))

(define features-sparc-linux-debian	; Very old, SPARC Debian v2(?)
  '("SPARC"
    "LINUX"
    "DEBIAN_SPARC"
    "DEBIAN_STRDUP_WEIRDNESS"
    "BITS_32"
    "BIG_ENDIAN"
    "HAVE_RINT"
    "HAVE_STRDUP"
    "HAVE_STRNCASECMP"
    "HAVE_DLFCN"
    "DYNAMIC_LOADING"
    "STACK_UNDERFLOW_COUNTING"
    "XOPEN_SIGNALS"
    ))

(define features-petit-macos9-cw3	; Very old (ca v0.48), CW Pro 3
  '("PETIT_LARCENY"
    "MACOS"
    "BITS_32"
    "BIG_ENDIAN"
    "EXPLICIT_DIVZ_CHECK"
    "STDC_SIGNALS"
    "CODEWARRIOR"
    "HAVE_RINT"
    "STACK_UNDERFLOW_COUNTING"
    "USE_GENERIC_ALLOCATOR"
    ))

(define features-petit-osf4-alpha	; Very old, OSF/1 4.0 on DEC Alpha
  '("PETIT_LARCENY"
    "BITS_32"
    "ENDIAN_LITTLE"
    "XOPEN_UNIX"
    "DEC_ALPHA_32BIT"
    "STACK_UNDERFLOW_COUNTING"))

(define selected-feature-set features-x86-nasm-win32)

; ------ END USER DEFINITION SECTION ------

(define feature-set-c-file-name (param-filename 'include "config.h"))
(define feature-set-asm-file-name (param-filename 'include "config.ah"))

(define features-boilerplate-start 
"/* DO NOT EDIT THIS FILE.  
   It is generated by the program features.sch in the Larceny root directory.
   */
#ifndef INCLUDED_CONFIG_H
#define INCLUDED_CONFIG_H
")

(define features-boilerplate-end 
"#ifndef DATE
#  ifdef __DATE__
#    define DATE __DATE__
#  else
#    define DATE \"Jan 29 1964\"
# endif
#endif
#ifndef TIME
#  ifdef __TIME__
#    define TIME __TIME__
#  else
#    define TIME \"03:00:00\"
#  endif
#endif
#ifndef USER
#  ifdef __USER__
#    define USER __USER__
#  else
#    define USER \"stanley\"
#  endif
#endif
#endif /* INCLUDED_CONFIG_H */
")

(define (read-existing-feature-set)
  (if (and (file-exists? feature-set-c-file-name)
	   (file-exists? feature-set-asm-file-name))
      (call-with-input-file feature-set-c-file-name
	(lambda (in)
	  (let loop ((fs '()))
	    (let ((l (read-line in)))
	      (cond ((eof-object? l)
		     (reverse fs))
		    ((and (> (string-length l) 8)
			  (string=? (substring l 0 8) "#define "))
		     (let cloop ((i 8))
		       (cond ((= i (string-length l))
			      (loop fs))
			     ((char=? (string-ref l i) #\space)
			      (loop (cons (substring l 8 i) fs)))
			     (else
			      (cloop (+ i 1))))))
		    (else
		     (loop fs)))))))
      '()))

(define (write-feature-set fs)
  (delete-file feature-set-c-file-name)
  (call-with-output-file feature-set-c-file-name
    (lambda (out)
      (display features-boilerplate-start out)
      (for-each (lambda (f)
		  (twobit-format out "#undef ~a~%" f))
		features)
      (for-each (lambda (f)
		  (twobit-format out "#define ~a 1~%" f))
		fs)
      (display features-boilerplate-end out)))

  (delete-file feature-set-asm-file-name)
  (let ((format-string (cond ((member "SPARC" fs)
			      "#define ~a 1~%")
			     ((member "X86_NASM" fs)
			      "%define ~a 1~%")
			     ((member "X86_SASSY" fs)
			      "%define ~a 1~%")
			     (else 
			      "define(`~a',1)~%"))))
    (call-with-output-file feature-set-asm-file-name
      (lambda (out)
	(for-each (lambda (f)
		    (twobit-format out format-string f))
		  fs)))))

(define (feature-set=? s1 s2)
  (and (every? (lambda (f) (member f s2)) s1)
       (every? (lambda (f) (member f s1)) s2)))

(define (feature-set-invalids fs)
  (cond ((null? fs)
	 '())
	((member (car fs) features)
	 (feature-set-invalids (cdr fs)))
	(else
	 (cons (car fs) (feature-set-invalids (cdr fs))))))

(define (feature-set-valid? fs)
  (null? (feature-set-invalids fs)))

(define (synthesize-features fs)

  (define (extend x fs)
    (if (not (member x fs))
	(cons x fs)
	fs))

  (let* ((fs (if (member "NO_SYNCHRONOUS_SIGNALS" fs)
		 (extend "EXPLICIT_DIVZ_CHECK" fs)
		 fs))
	 (fs (if (or (member "SUNOS4" fs) 
		     (member "SUNOS5" fs))
		 (extend "SUNOS" fs)
		 fs))
	 (fs (if (or (member "SUNOS" fs) 
		     (member "LINUX" fs)
		     (member "CYGWIN" fs)
		     (member "BSD_UNIX" fs)
		     (member "POSIX_UNIX" fs)
		     (member "XOPEN_UNIX" fs))
		 (extend "UNIX" fs)
		 fs))
	 (fs (if (or (member "BSD_SIGNALS" fs)
		     (member "POSIX_SIGNALS" fs)
		     (member "STDC_SIGNALS" fs)
		     (member "XOPEN_SIGNALS" fs)
		     (member "WIN32_SIGNALS" fs))
		 fs
		 (cond ((or (member "SUNOS4" fs)
			    (member "BSD_UNIX" fs))
			(extend "BSD_SIGNALS" fs))
		       ((or (member "LINUX" fs)
			    (member "CYGWIN" fs)
			    (member "POSIX_UNIX" fs))
			(extend "POSIX_SIGNALS" fs))
		       ((or (member "XOPEN_UNIX" fs)
			    (member "SUNOS5" fs))
			(extend "XOPEN_SIGNALS" fs))
		       ((member "WIN32" fs)
			(extend "WIN32_SIGNALS" fs))
		       (else
			(extend "STDC_SIGNALS" fs))))))
    fs))

(define (feature-sanity-checks fs)
  (if (member "BITS_64" fs)
      (error "Larceny cannot yet handle 64-bit systems."))

  (if (not (or (member "BITS_32" fs) (member "BITS_64" fs)))
      (error "You need to select a word size"))

  (if (not (or (member "ENDIAN_LITTLE" fs) (member "BIG_ENDIAN" fs)))
      (error "You need to select an endian-ness")))

(define (define-feature-set)
  (let ((old (read-existing-feature-set))
	(new selected-feature-set))
    (let ((invalid-entries (feature-set-invalids new)))
      (if (not (null? invalid-entries))
	  (error "The new feature set is not valid due to these entries: "
		 invalid-entries)))
    (feature-sanity-checks new)
    (let ((new (synthesize-features new)))
      (if (not (and (feature-set=? old new)
		    (feature-set-valid? old)))
	  (begin
	    (display "Redefined feature set!")
	    (newline)
            (if (file-exists? "Rts/Makefile")
                (delete-file "Rts/Makefile"))
	    (write-feature-set new))))))

(define (read-line . rest)

  (define (finish l k)
    (let ((s (make-string k)))
      (do ((i (- k 1) (- i 1))
	   (l l (cdr l)))
	  ((< i 0) s)
	(string-set! s i (car l)))))

  (define (loop p l k)
    (let ((c (read-char p)))
      (cond ((eof-object? c)
	     (if (null? l)
		 c
		 (finish l k)))
	    ((char=? c #\newline)
	     (finish l k))
	    (else
	     (loop p (cons c l) (+ k 1))))))

  (if (null? rest)
      (loop (current-input-port) '() 0)
      (loop (car rest) '() 0)))

; eof
