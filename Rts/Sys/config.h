/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * You must define the attributes for the system you're compiling in the
 * "User Definition Section", below.  
 *
 *          DON'T PANIC!  
 *
 * Useful sets of attributes for many systems are defined in that 
 * section, just pick the one appropriate to your system.
 *
 * Refer to the installation notes for more elaborate instructions, or
 * read the comments in this file carefully.
 *
 * ---
 *
 * References (for signal handling specifications):
 *  Leffler, McKusick, Karels, Quarterman: The Design and Implementation
 *    of the 4.3 BSD UNIX Operating System.  Addison Wesley, 1989.
 *
 *  ANSI/ISO C Standard (FIXME)
 *
 *  Donald Lewine, POSIX programmer's guide.  O'Reilly.  
 */

#if !defined(INCLUDED_CONFIG_H)
#define INCLUDED_CONFIG_H

/* Undef all the preprocessor symbols that Larceny reserves */

/* 
 * Architectures -- we could destinguish on models, but don't need to yet.
 * Select PETIT_LARCENY if that's what you're building.
 */
#undef SPARC			/* Native: SPARC v8 or later */
#undef X86_NASM                 /* Native: Intel 386 using NASM assembler */
#undef PETIT_LARCENY		/* Portable: Hardware is irrelevant */

/* 
 * Architecture attributes. 
 */
#undef BITS_32			/* 32-bit words */
#undef BITS_64			/* 64-bit words */
#undef ENDIAN_LITTLE		/* Least significant byte at lowest address */
#undef BIG_ENDIAN		/* Most significant byte at lowest address */
#undef EXPLICIT_DIVZ_CHECK      /* Explicit check for integer division by zero.
				   Some systems, like the PPC, do not trap
				   integer division by zero. */
#undef FLUSH_ALWAYS             /* Set to 1 to force icache flushing */
#undef FLUSH_NEVER              /* Set to 1 to disable icache flushing */
#undef HARDWARE_DIVISION        /* Set to 1 to use hardware division even
				   if that compromises backward compatibility.
				   Some RISC systems implement HW division
				   only in later architecture versions. */
#undef SPARCV9                  /* Use v9 instructions */

/* 
 * Operating systems. 
 */
#undef SUNOS4			/* SunOS 4.x */
#undef SUNOS5			/* SunOS 5.x */
#undef LINUX			/* Generic GNU/Linux */
#undef BSD_UNIX			/* Generic BSD (4.3ish) Unix, also MacOS X */
#undef POSIX_UNIX		/* Generic POSIX-standard Unix */
#undef XOPEN_UNIX		/* Generic XOPEN-standard Unix */
#undef WIN32			/* Generic Windows 32-bit */
#undef MACOS			/* Generic Macintosh OS 9.x or earlier */
#undef GENERIC_OS		/* Anything else */

#undef SUNOS			/* Synthesized */
#undef UNIX			/* Synthesized */


/* 
 * Operating system attributes.
 */
#undef BSD_SIGNALS		/* sigvec, as described by Leffler et al */
#undef STDC_SIGNALS		/* signal, as in ANSI/ISO C (weak) */
#undef POSIX_SIGNALS		/* sigaction + sa_handler, as described 
				   by Lewine */
#undef XOPEN_SIGNALS		/* sigaction + sa_sigaction */
#undef WIN32_SIGNALS            /* another world altogether */

/*
 * Other configuration options.
 */
#undef DOF_COLLECTOR
  /* When set, makes the deferred-older-first collector to be compiled
     into the system.  For specially interested only.
     
     Recommended setting is 0.
     */

#undef ROF_COLLECTOR
  /* When set, makes the renewal-older-first collector to be compiled
     into the system.  For specially interested only.
     
     Recommended setting is 0.
     */

#undef STACK_UNDERFLOW_COUNTING 
  /* When set, enables stack underflow accounting.  The performance
     impact is negligible.  
     
     Recommended setting is 1.
     */

#undef GCLIB_LARGE_TABLE        
  /* When set, preallocates a page table for the entire 4GB address space
     rather than allocating it lazily.  The table uses one byte per 4KB
     page, for a total of 1MB.  Enabling this option will probably reduce
     the cost of the write barrier both in the mutator and in the garbage
     collector, and reduce the register pressure in the inner loops of
     the collector.
     
     Recommended setting: probably 0, as it needs further evaluation.
     */

#undef RETURN_MEMORY_TO_OS
  /* When set, the lowlevel memory manager eagerly returns memory blocks
     to the operating system when they are released by the garbage 
     collector.  
     
     Recommended setting is 0 (or undefined), since it tends to increase
     execution time due to added system overhead.  
     
     Even if 0 or undefined, the memory manager may return memory blocks
     to the operating system.  Even if 1, the memory manager is not
     guaranteed to release memory.  
     */

#undef GC_HIRES_TIMERS
  /* When set, high-resolution timers for various parts of the garbage
     collector are enabled.  Only effective if the platform has high
     resolution timers.

     Recommended setting is 0, unless you're doing GC research.
     */

#undef GC_EVENT_COUNTERS
  /* When set, the garbage collector cores count the number of pointers
     copied, the number of those pointers that hit the write barrier (if
     applicable), and similar things.  Turning this on will slow down
     garbage collection, though I don't know by how much.  
  
     Recommended setting is 0, even if you're doing GC research :-)
     */

#undef USE_GENERIC_ALLOCATOR
  /* When set, use a generic malloc-based low-level memory allocator
     rather than any OS-specific allocator.  The functions selected are
     osdep_alloc_aligned, osdep_free_aligned, and osdep_fragmentation.

     The generic allocator is selected automatically if the operating
     system is GENERIC_OS.

     (The generic allocator is in osdep-generic.c, and that file must
     be included in the compilation along with the osdep-* file for
     the appropriate operating system, unless the system is being
     compiled for a generic OS.  Normally there is no reason to worry
     about this as the Makefiles do the right thing.)
     */

#undef USE_GENERIC_IO
  /* When set, use a generic stdio-based low-level I/O system rather
     than any OS-specific I/O system.  The functions selected are
     osdep_openfile, osdep_closefile, osdep_readfile, osdep_writefile,
     and osdep_pollinput.
     
     The generic I/O system is selected automatically if the operating
     system is GENERIC_OS.
     
     Also see comments for USE_GENERIC_ALLOCATOR.
     */

#undef USE_GENERIC_FILESYSTEM
  /* When set, use generic low-level file-system support rather
     than any OS-specific file-system support.  The functions selected
     are osdep_unlinkfile, osdep_mtime, osdep_access, and osdep_rename.
     
     The generic I/O system is selected automatically if the operating
     system is GENERIC_OS.

     Also see comments for USE_GENERIC_ALLOCATOR.
     */

#undef USE_STDIO
  /* Console input is by default through stdin, and console output is 
     by default through stdout.  Used by generic I/O subsystem.
     */


/* 
 * Special system attributes -- for use of non-portable extensions or 
 * bug workarounds, or other weirdness.
 */
#undef CODEWARRIOR
  /* Metrowerks Codewarrior extensions.  Currently this is required
     for Petit Larceny on the Mac.
     */

#undef DEC_ALPHA_32BIT		
  /* DEC Alpha, in 32-bit mode.  This is needed to cope with some mixed
     word-length weirdness on that machine.
     */

#undef NO_ATOMIC_ALLOCATION
  /* Boehm/Demers/Weiser conservative collector: do not allocate 
     bytevectors specially (not recommended in general).
     */

#undef NO_SYNCHRONOUS_SIGNALS
  /* Disable the use of longjump out of signal handlers.  This may
     result in the loss of interruptibility in some cases and
     performance reduction in others, but for now it is required
     to deal with signals on those platforms (WinNT and Win2k) that
     run signal handlers in a separate thread.
     */

#undef SIMULATE_NEW_BARRIER     
  /* Enable the simulation of Clinger's write barrier.  This will slow
     down execution substantially.  Useful for research only, and not
     supported by all the collectors.
     */

/*
 * Library/feature attributes.  These macros declare the existence of
 * particular functions in the run-time library or as compiler
 * intrinsics.  It's always OK to leave these macros undefined, as
 * Larceny will supply portable definitions.  It may be desirable for
 * performance or functionality reasons to define these macros below, 
 * if your library or compiler supplies any of the functions -- for
 * example, the portable definition of gethrtime() always returns 0,
 * and the portable definition of stat() always returns the same
 * file modification time.
 */
#undef HAVE_RINT		/* Library has rint() -- round to even */
#undef HAVE_AINT		/* Library has aint() -- round to zero */
#undef HAVE_STRNCASECMP		/* Library has strncasecmp() */
#undef HAVE_STRDUP		/* Library has strdup() */
#undef HAVE_HRTIME_T            /* Library has hrtime_t and gethrtime(),
				   and one can get hrtime_t by including
				   <sys/time.h>
				   */
#undef HAVE_STAT                /* Library has 'struct stat' and stat(),
				   and one can get at them by including
				   <stat.h> */
#undef HAVE_POLL                /* Library has poll() */
#undef HAVE_SELECT              /* Library has select() */

/* ------ USER DEFINITION SECTION ------- */
/*
 * Define those symbols from the above set that correspond to the 
 * system you're building.  You can define the type of signal handling 
 * facilities here, but if you don't, then the condition nest below will
 * make a guess based on the operating system you've selected.  
 *
 * For Petit Larceny, selecting the precise operating system is not
 * crucial; for example, POSIX_UNIX or BSD_UNIX will work OK on
 * platforms that conform.  For native systems, you need to be 
 * more specific, so define SUNOS4/SUNOS5 as appropriate for the SPARC
 * native version.
 */

/* Here are some sets of settings that work for me. */

/* Sun/SPARC Solaris (2.5 and better, at least); native. */
#if 0
# define SPARC                     1
# define SUNOS5                    1
# define BITS_32                   1
# define BIG_ENDIAN                1
# define HAVE_RINT                 1
# define HAVE_STRDUP               1
# define HAVE_STRNCASECMP          1
# define HAVE_HRTIME_T             1
# define HAVE_POLL                 1
# define STACK_UNDERFLOW_COUNTING  1
# define GC_HIRES_TIMERS           1
# define GC_EVENT_COUNTERS         0
# define GCLIB_LARGE_TABLE         0
#endif

/* Sun/SPARC Solaris (2.5 and better, at least), Petit Larceny */ 
#if 0
# define PETIT_LARCENY             1
# define SUNOS5                    1
# define BITS_32                   1
# define BIG_ENDIAN                1
# define HAVE_RINT                 1
# define HAVE_STRDUP               1
# define HAVE_STRNCASECMP          1
# define HAVE_HRTIME_T             1
# define HAVE_POLL                 1
# define STACK_UNDERFLOW_COUNTING  1
# define GC_HIRES_TIMERS           1
# define GC_EVENT_COUNTERS         0
# define GCLIB_LARGE_TABLE         0
#endif

/* MacOS X; gcc; GNU libc (Petit Larceny). */
#if 0
# define PETIT_LARCENY             1
# define BITS_32                   1
# define BIG_ENDIAN                1
# define BSD_UNIX                  1
# define HAVE_RINT                 1
# define HAVE_STRNCASECMP          1
# define HAVE_STRDUP               1
# define HAVE_SELECT               1
# define STACK_UNDERFLOW_COUNTING  1
# define USE_GENERIC_ALLOCATOR     1     /* Weirdness with mmap */
#endif

/* Windows 2000 on x86; MetroWerks Codewarrior Pro 6; (Petit Larceny). */
#if 0
# define PETIT_LARCENY             1
# define WIN32                     1
# define BITS_32                   1
# define ENDIAN_LITTLE             1
# define STACK_UNDERFLOW_COUNTING  1
# define USE_GENERIC_ALLOCATOR     1
# define USE_GENERIC_IO            1
# define USE_STDIO                 1
# define USE_GENERIC_FILESYSTEM    1
# define NO_SYNCHRONOUS_SIGNALS    1
# define HAVE_STAT                 1
# define HAVE_RINT                 1
# define HAVE_STRNCASECMP          1
#endif

/* Debian Linux 3.0; gcc; GNU libc (Petit Larceny). */
#if 0
# define PETIT_LARCENY             1
# define BITS_32                   1
# define ENDIAN_LITTLE             1
# define LINUX                     1
# define HAVE_RINT                 1
# define HAVE_STRNCASECMP          1
# define HAVE_STRDUP               1
# define HAVE_POLL                 1
# define STACK_UNDERFLOW_COUNTING  1
# define DEBIAN_STRDUP_WEIRDNESS   1
#endif

/* Debian Linux 3.0; gcc+nasm; GNU libc (x86-nasm native). */
#if 1
# define X86_NASM                  1
# define BITS_32                   1
# define ENDIAN_LITTLE             1
# define LINUX                     1
# define HAVE_RINT                 1
# define HAVE_STRNCASECMP          1
# define HAVE_STRDUP               1
# define HAVE_POLL                 1
# define STACK_UNDERFLOW_COUNTING  1
# define DEBIAN_STRDUP_WEIRDNESS   1
#endif

/* RedHat Linux 5.1; gcc; GNU libc (Petit Larceny). */
#if 0
# define PETIT_LARCENY             1
# define BITS_32                   1
# define ENDIAN_LITTLE             1
# define LINUX                     1
# define HAVE_RINT                 1
# define HAVE_STRNCASECMP          1
# define HAVE_STRDUP               1
# define HAVE_POLL                 1
# define STACK_UNDERFLOW_COUNTING  1
#endif

/* Sun/SPARC Debian Linux; gcc; native.
   Note, this is old and may not be completely adequate. */
#if 0
# define SPARC                     1
# define LINUX                     1
# define DEBIAN_SPARC              1
# define DEBIAN_STRDUP_WEIRDNESS   1
# define BITS_32                   1
# define BIG_ENDIAN                1
# define HAVE_RINT                 1
# define HAVE_STRDUP               1
# define HAVE_STRNCASECMP          1
# define STACK_UNDERFLOW_COUNTING  1
# define XOPEN_SIGNALS             1
#endif

/* MacOS 8 or 9; Metrowerks codewarrior (Petit Larceny). 
   This worked once upon a time around Larceny 0.48, possibly
   with CodeWarrior Pro 3.
   */
#if 0
# define PETIT_LARCENY             1
# define MACOS                     1
# define BITS_32                   1
# define BIG_ENDIAN                1
# define EXPLICIT_DIVZ_CHECK       1
# define STDC_SIGNALS              1
# define CODEWARRIOR               1
# define HAVE_RINT                 1
# define STACK_UNDERFLOW_COUNTING  1
# define USE_GENERIC_ALLOCATOR     1
#endif

/* DEC OSF/1 4.0 on DEC Alpha, at least (Petit Larceny);
   running in 32-bit mode on 64-bit platform.  These defs
   are probably out of date.
   */
#if 0
# define PETIT_LARCENY             1
# define BITS_32                   1
# define ENDIAN_LITTLE             1
# define XOPEN_UNIX                1
# define DEC_ALPHA_32BIT           1
# define STACK_UNDERFLOW_COUNTING  1
#endif


/* ------ END USER DEFINITION SECTION ------ */

/* Synthesize some attributes */

#if defined(NO_SYNCHRONOUS_SIGNALS)
#  define EXPLICIT_DIVZ_CHECK 1
#endif

#if defined(SUNOS4) || defined(SUNOS5)
#  define SUNOS
#endif

#if defined(SUNOS) || defined(LINUX) || defined(BSD_UNIX) || \
    defined(POSIX_UNIX) || defined(XOPEN_UNIX)
#  define UNIX
#endif

/* Guess signal handling facilities if none specified.  Some systems
   support several (indeed, all!) interfaces; pick the best one.
   */

#if !defined(BSD_SIGNALS) && !defined(POSIX_SIGNALS) && \
    !defined(STDC_SIGNALS) &&!defined(XOPEN_SIGNALS) && \
    !defined(WIN32_SIGNALS)
#  if defined(SUNOS4) || defined(BSD_UNIX)
#    define BSD_SIGNALS
#  elif defined(LINUX) || defined(POSIX_UNIX)
#    define POSIX_SIGNALS
#  elif defined(XOPEN_UNIX) || defined(SUNOS5)
#    define XOPEN_SIGNALS
#  elif defined(WIN32)
#    define WIN32_SIGNALS
#  else
#    define STDC_SIGNALS
#  endif
#endif


/* Sanity checks */

#if defined(BITS_64)
#  error "Larceny cannot yet handle 64-bit systems."
#endif

#if !defined(BITS_32) && !defined(BITS_64)
#  error "You need to select a word size"
#endif

#if !defined(ENDIAN_LITTLE) && !defined(BIG_ENDIAN)
#  error "You need to select an endian-ness"
#endif

#if !defined(DATE)		/* Defined by Makefile on some systems */
#  define DATE "1964-01-29"     /* Build date */
#endif

#if !defined(USER)		/* Defined by Makefile on some systems */
#  define USER "stanley"        /* Build user */
#endif

#endif  /* INCLUDED_CONFIG_H */

/* eof */
