/* A bunch of random hacks that belong elsewhere; here for the time being.
   $Id$
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

#define VEC_OVERHEAD      0

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

#define FIXTAGMASK        3

#define is_fixnum( x )        (((x) & FIXTAGMASK) == 0)
#define nonnegative_fixnum( x )  (((x) & FIXTAGMASK) == 0 && (s_word)(x) >= 0)
#define both_fixnums( x, y )  ((((x) | (y)) & FIXTAGMASK) == 0)

#define is_char( x )          (((x) & 255) == IMM_CHAR)
#define fixnum_to_char( x )        ((((x) & 1023) << 14) | IMM_CHAR)
#define int_to_char( x )           ((((x) & 1023) << 16) | IMM_CHAR)
#define charcode_as_fixnum( x )         ((x) >> 14)
#define charcode( x )                   ((x) >> 16)

#endif
