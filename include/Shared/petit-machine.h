/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Abstractions for the instruction macros defined in twobit.h.  The
 * definitions in this file make the virtual machine a little more
 * pleasant (removing or glossing over a number of historical
 * accidents that are hard to fix without changing the SPARC
 * back-end).
 *
 * TODO, on an idle day: synchronize these names with the ones in
 * ../Intel/i386-machine.h, they are even better.
 */

#ifndef PETIT_MACHINE_H
#define PETIT_MACHINE_H

#ifndef G_SECOND
# define G_SECOND         G_ARGREG2
# define G_THIRD          G_ARGREG3
# define G_FOURTH         G_SCHCALL_ARG4
#endif
#define LASTREG           31
#define NREGS             32

/* Hacks to avoid having to mess with layouts.cfg yet */
#define PROC_CODE         PROC_CODEPTR
#define PROC_CONST        PROC_CONSTANTS
#define G_STKLIM          G_ETOP
#define STK_FRAMESIZE     STK_CONTSIZE
#define STK_RETURN        STK_RETADDR
/* End hacks */

#if CODEPTR_SHIFT1
# define DECODE_CODEPTR(w) ((codeptr_t)((word)w >> 1))
# define ENCODE_CODEPTR(p) ((word)p << 1)
#elif CODEPTR_SHIFT2
# define DECODE_CODEPTR(w) ((codeptr_t)((word)w >> 2))
# define ENCODE_CODEPTR(p) ((word)p << 2)
#else
# define DECODE_CODEPTR(w) ((codeptr_t)w)
# define ENCODE_CODEPTR(p) ((word)p)
#endif

#if defined PETIT_LARCENY && USE_GOTOS_LOCALLY
# define ENCODE_RETURN_ADDRESS(L_numeric,L_symbolic) ((word)L_numeric << 2)
# define DECODE_RETURN_ADDRESS(x) ((cont_t)((x) >> 2))
#else
# define ENCODE_RETURN_ADDRESS(L_numeric,L_symbolic) ENCODE_CODEPTR(L_symbolic)
# define DECODE_RETURN_ADDRESS(x) ((cont_t)DECODE_CODEPTR(x))
#endif

/* Exception codes */
/* These are strange cases; must check that Rts sources don't use the
   old/wrong names before renaming in except.cfg.
   */
#define EX_MAKE_VECTOR            EX_MKVL    /* Hack, FIXME */
#define EX_MAKE_BYTEVECTOR        EX_MKBVL   /* Hack, FIXME */
#define EX_BYTEVECTOR_LIKE_LENGTH EX_BVLLEN
#define EX_BYTEVECTOR_LIKE_REF    EX_BVLREF
#define EX_BYTEVECTOR_LIKE_SET    EX_BVLSET
#define EX_VECTOR_LIKE_REF        EX_VLREF
#define EX_VECTOR_LIKE_SET        EX_VLSET
#define EX_VECTOR_LIKE_LENGTH     EX_VLLEN
#define EX_MAKE_PROCEDURE         EX_MKVL

/* End exception codes */

extern int larceny_main( int argc, char **argv );

#endif /* PETIT_MACHINE_H */

/* eof */
