/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * You must define the attributes for the system you're compiling in 
 * the section for user definitions, below.
 *
 * Don't panic!  Useful sets of attributes for many systems are defined
 * below.
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

/* Architectures -- we could destinguish on models, but don't need to yet.
   Select PETIT_LARCENY if that's what you're building.
   */
#undef SPARC			/* Native: SPARC v8 or later */
#undef PETIT_LARCENY		/* Portable: Hardware is irrelevant */


/* Architecture attributes. */
#undef BITS_32			/* 32-bit words */
#undef BITS_64			/* 64-bit words */
#undef ENDIAN_LITTLE		/* least significant byte at lowest address */
#undef BIG_ENDIAN		/* most significant byte at lowest address */
#undef EXPLICIT_DIVZ_CHECK      /* Explicit check for integer division by zero.
				   Some systems, like the PPC, do not trap
				   integer division by zero.
				   */
#undef FLUSH_ALWAYS             /* Set to 1 to force icache flushing */
#undef FLUSH_NEVER              /* Set to 1 to disable icache flushing */


/* Operating systems. */
#undef SUNOS4			/* SunOS 4.x */
#undef SUNOS5			/* SunOS 5.x */
#undef LINUX			/* Generic GNU/Linux */
#undef BSD_UNIX			/* Generic BSD (4.3ish) Unix */
#undef POSIX_UNIX		/* Generic POSIX-standard Unix */
#undef XOPEN_UNIX		/* Generic XOPEN-standard Unix */
#undef WIN32			/* Generic Windows 32-bit */
#undef MACOS			/* Generic Macintosh OS */
#undef GENERIC_OS		/* Anything else */

#undef SUNOS			/* Synthesized */
#undef UNIX			/* Synthesized */


/* Operating system attributes */
#undef BSD_SIGNALS		/* sigvec, as described by Leffler et al */
#undef STDC_SIGNALS		/* signal, as in ANSI/ISO C (weak) */
#undef POSIX_SIGNALS		/* sigaction + sa_handler, as described 
				   by Lewine */
#undef XOPEN_SIGNALS		/* sigaction + sa_sigaction */


/* Other configuration options */
#undef STACK_UNDERFLOW_COUNTING /* Set to 1 to enable this (recommended) */


/* Special system attributes -- for use of non-portable extensions or 
   bug workarounds, or other weirdness.
   */
#undef CODEWARRIOR              /* Metrowerks Codewarrior extensions */
#undef DEC_ALPHA_32BIT		/* DEC Alpha, in 32-bit mode.  Needed to
				   cope with some mixed-word-length weirdness.
				*/
#undef NO_ATOMIC_ALLOCATION     /* BDW collector: do not allocate bytevectors
				   specially (not recommended in general) */
#undef GCLIB_LARGE_TABLE        /* Wizards only */
#undef SIMULATE_NEW_BARRIER     /* Wizards only */


/* Library attributes.  These macros declare the existence of particular
   functions in the run-time library or as compiler intrinsics.
   It's always OK to leave these macros undefined, as Larceny will
   supply portable definitions.  It may be desirable for performance
   reasons to define these macros below, if your library or compiler
   supplies any of the functions.
   */
#undef HAVE_RINT		/* Library has rint() -- round to even */
#undef HAVE_AINT		/* Library has aint() -- round to zero */
#undef HAVE_STRNCASECMP		/* Library has strncasecmp() */
#undef HAVE_STRDUP		/* Library has strdup() */


/* ------ USER DEFINITION SECTION ------- */
/* Define those symbols from the above set that correspond to the 
   system you're building.  You can define the type of signal handling 
   facilities here, but if you don't, then the condition nest below will
   guess based on the operating system you've selected.  

   For Petit Larceny, selecting the precise operating system is not
   crucial; for example, POSIX_UNIX or BSD_UNIX will work OK on
   platforms that conform.  For native systems, you need to be 
   more specific, so define SUNOS4/SUNOS5 as appropriate for the SPARC
   native version.
   */

/* Here are some sets of settings that work for me. */

/* Sparc Solaris (2.5 and better, at least); native. */
#define SPARC                     1
#define SUNOS5                    1
#define BITS_32                   1
#define BIG_ENDIAN                1
#define HAVE_RINT                 1
#define HAVE_STRDUP               1
#define HAVE_STRNCASECMP          1
#define STACK_UNDERFLOW_COUNTING  1
#define GCLIB_LARGE_TABLE         0 /* Experimental! */

/* MacOS; Metrowerks codewarrior (Petit Larceny).
#define PETIT_LARCENY             1
#define MACOS                     1
#define BITS_32                   1
#define BIG_ENDIAN                1
#define EXPLICIT_DIVZ_CHECK       1
#define STDC_SIGNALS              1
#define CODEWARRIOR               1
#define HAVE_RINT                 1
#define STACK_UNDERFLOW_COUNTING  1
*/

/* Dec OSF/1 4.0 on DEC Alpha, at least (Petit Larceny);
   running in 32-bit mode on 64-bit platform.
#define PETIT_LARCENY             1
#define BITS_32                   1
#define ENDIAN_LITTLE             1
#define XOPEN_UNIX                1
#define DEC_ALPHA_32BIT           1
#define STACK_UNDERFLOW_COUNTING  1
*/

/* RedHat Linux 5.1; gcc; GNU libc (Petit Larceny).
#define PETIT_LARCENY             1
#define BITS_32                   1
#define ENDIAN_LITTLE             1
#define LINUX                     1
#define HAVE_RINT                 1
#define HAVE_STRNCASECMP          1
#define HAVE_STRDUP               1
#define STACK_UNDERFLOW_COUNTING  1
*/


/* ------ END USER DEFINITION SECTION ------ */

/* Synthesize some attributes */

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
    !defined(STDC_SIGNALS) &&!defined(XOPEN_SIGNALS)
#  if defined(SUNOS4) || defined(BSD_UNIX)
#    define BSD_SIGNALS
#  elif defined(LINUX) || defined(POSIX_UNIX)
#    define POSIX_SIGNALS
#  elif defined(XOPEN_UNIX) || defined(SUNOS5)
#    define XOPEN_SIGNALS
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

#if !defined(DATE)
#  define DATE "now"
#endif

#if !defined(USER)
#  define USER "superuser"
#endif

#endif  /* INCLUDED_CONFIG_H */

/* eof */
