/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * A bunch of random hacks that belong elsewhere; here for the time being.
 */

#ifndef PETIT_HACKS_H
#define PETIT_HACKS_H

/* Gag.  FIXME! */
#define G_SECOND          G_ARGREG2
#define G_THIRD           G_ARGREG3
#define G_FOURTH          G_SCHCALL_ARG4
#define LASTREG           31
#define NREGS             32

/* Hacks to avoid having to mess with layouts.cfg yet */
#define PROC_CODE         PROC_CODEPTR
#define PROC_CONST        PROC_CONSTANTS
#define G_STKLIM          G_ETOP
#define STK_FRAMESIZE     STK_CONTSIZE
#define STK_RETURN        STK_RETADDR
/* End hacks */

/* Exception codes */
/* These are strange cases; must check that Rts sources don't use the
   old/wrong names before renaming in except.cfg.
   */
#define EX_MAKE_VECTOR            EX_MKVL
#define EX_MAKE_BYTEVECTOR        EX_MKBVL
#define EX_BYTEVECTOR_LIKE_LENGTH EX_BVLLEN
#define EX_BYTEVECTOR_LIKE_REF    EX_BVLREF
#define EX_BYTEVECTOR_LIKE_SET    EX_BVLSET
#define EX_VECTOR_LIKE_REF        EX_VLREF
#define EX_VECTOR_LIKE_SET        EX_VLSET
#define EX_VECTOR_LIKE_LENGTH     EX_VLLEN
#define EX_MAKE_PROCEDURE         EX_MKVL
/* End exception codes */

#endif

/* eof */
