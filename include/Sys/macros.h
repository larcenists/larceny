/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- macros
 *
 * Machine and representation dependent (and independent) macros,
 * including tag definitions and tag masks, and interesting constants.
 *
 * For C-language routines.
 */

#ifndef INCLUDED_MACROS_H
#define INCLUDED_MACROS_H

#include <stdlib.h>             /* for abs() */

/* Various masks */
#define ISHDR_MASK      0x00000083      /* extract bits 7, 1, and 0 */
#define HDR_SIGN        0x00000082      /* header signature */
#define HDR_MASK        0x000000E3      /* Mask to extract header info */
#define FIXTAGMASK               3      /* FIXME: move to layouts.cfg */

/* Word size.  Fixnums always have a two-bit tag, so on 64-bit systems
 * a fixnum n is NOT the same as n*sizeof(word); use FIXNUM_TO_WORDBYTES
 * to convert a fixnum word index or count into a byte offset or count.
 */
#if defined(BITS_64)
# define WORD_SHIFT     3               /* log2(sizeof(word)) */
# define FIXNUM_TO_WORDBYTES( x ) ((word)(x) << 1)
# define WORDBYTES_TO_FIXNUM( x ) ((word)(x) >> 1)
#else
# define WORD_SHIFT     2
# define FIXNUM_TO_WORDBYTES( x ) ((word)(x))
# define WORDBYTES_TO_FIXNUM( x ) ((word)(x))
#endif

/* Convert integer to fixnum.  (Shifting a negative signed value is
   undefined behavior in C, so the shift is done on an unsigned word.) */
#define fixnum( x )  ((s_word)((word)(x) << 2))

/* Convert fixnum to integer. Watch the sign! Can't use a shift here (sigh).
 * Might be faster to keep the sign, shift the absvalue, and convert the
 * sign again than to actually divide the signed number. Not used much, tho.
 */
#define nativeint( x )      ((word) ((s_word)(x) / 4))
#define nativeuint( n )     ((word)(n) / 4)

/* Given tagged pointer, return tag */
#define tagof( w )          ((word)(w) & TAG_MASK)

/* Given tagged pointer, return pointer */
#define ptrof( w )          ((word *)((word)(w) & ~TAG_MASK))

/* May be faster on some architectures, and should never be slower (?) */
#define striptag( w, t )   ((word*)((word)(w) - (t)))

/* Given pointer and tag, return tagged pointer */
#define tagptr( w, tag )    ((word)(w) | (tag))

/* extract header tag (i.e. sans typetag) from a header word */
#define header( w )         ((word)(w) & HDR_MASK)

/* extract type tag from a header word */
#define typetag( w )        ((word)(w) & TYPETAG_MASK)

/* strip type tag from a header word */
#define striptypetag( w )   ((word)(w) & ~TYPETAG_MASK)

/* Create a header given integer (not fixnum!) size (in bytes), and tag. */
#define mkheader( size, tag )  (((word) (size) << 8) | (tag))

/* a word is a pointer if the low bit is set */
#define isptr(w)            is_ptr(w)    /* isptr old name */
#define is_ptr(w)           ((word)(w) & 0x01)

/* a word is a header if it has a header mask layout */
#define ishdr(w)            is_hdr(w)    /* ishdr is old name */
#define is_hdr(w)           (((word)(w) & ISHDR_MASK) == HDR_SIGN)

/* Extract size field from a header word. Hi bit is always 0. */
#define sizefield( w )      ((word)(w) >> 8)

/* miscellaneous */
#undef max
#define max( a, b )         ((a) > (b) ? (a) : (b))
#undef min
#define min( a, b )         ((a) < (b) ? (a) : (b))

#define ceildiv(a,b)        ((a)%(b) == 0 ? (a)/(b) : (a)/(b)+1)

#define roundup( a, m )     ((((a)+((m)-1)) / (m))*(m))
#define roundup2( a )       (((a) + 1) & ~1)
#define roundup4( a )       (((a) + 3) & ~3)
#define roundup8( a )       (((a) + 7) & ~7)

#define roundup16( a )      (((a) + 15) & ~(word)15)

#if defined(BITS_64)
#define roundup_word( a )   roundup8( a )
#define roundup_dword( a )  roundup16( a )
#else
#define roundup_word( a )   roundup4( a )
#define roundup_dword( a )  roundup8( a )
#endif

/* Rounding macros for wordsize-independence */
#define roundup_walign(a)   roundup2( a )   /* Word alignment: 2 words */
#define roundup_balign(a)   roundup_dword( a ) /* Byte alignment: 2 words */

/* Total size in bytes (header + data + padding) of a vector-like,
 * bytevector-like, or procedure object given its header word.
 */
#define object_size_bytes( hdr ) \
  roundup_balign( sizefield( hdr ) + sizeof(word) )

#define the_gc( globals )      ((gc_t*)globals[ G_GC ])

#define bytes2words(x)      (((word) (x)) / sizeof(word))
#define words2bytes(x)      ((x) * sizeof(word))

/* Macros for manipulating Scheme data types. */

#define is_pair( ptr )          (tagof(ptr) == PAIR_TAG)
#define is_char( x )            (((word)(x) & 255) == IMM_CHAR)
#define is_fixnum( x )          (((word)(x) & FIXTAGMASK) == 0)
#define is_nonnegative_fixnum( x ) \
  (((x) & FIXTAGMASK) == 0 && (s_word)(x) >= 0)
#define is_both_fixnums( x, y ) \
  ((((x) | (y)) & FIXTAGMASK) == 0)

#define pair_car( ptr )        (*ptrof( ptr ))
#define pair_cdr( ptr )        (*(ptrof( ptr )+1))

#define string_length( x )     (sizefield(*ptrof( x )))
#define string_data( x )       ((char*)ptrof( x )+sizeof(word))

#define vector_length( vp )    (sizefield(*ptrof(vp)) >> WORD_SHIFT)
#define vector_ref( vp, i )    (ptrof( vp )[ VEC_HEADER_WORDS+(i) ])
#define vector_set( vp, i, v ) (ptrof( vp )[ VEC_HEADER_WORDS+(i) ] = (v))

#define procedure_length( pp ) (sizefield(*ptrof(pp)) >> WORD_SHIFT)
#define procedure_ref( pp, i ) (ptrof(pp)[ PROC_HEADER_WORDS+(i) ])
#define procedure_set( pp, i, x )  (ptrof(pp)[ PROC_HEADER_WORDS+(i) ] = (x))

#define bytevector_length(x)   (sizefield(*ptrof(x)))
#define bytevector_ref(x,i)    (*((byte*)(ptrof(x)+1)+i))

/* Bignums: the first 32 bits of data hold the sign (bit 24) and the
 * number of 32-bit digits (low 24 bits); then follow the 32-bit digits,
 * least significant first.  The layout of the data is the same for
 * 32-bit and 64-bit systems.
 */
#define bignum_data32( x )     ((u32*)(ptrof(x)+1))
#define mkbignum_header( sign, length )  (((sign) << 24) | length)
#define bignum_length( x )     (bignum_data32(x)[0] & 0x00FFFFFF)
#define bignum_sign( x )       ((bignum_data32(x)[0] >> 24) & 1)
#define bignum_ref32( x, i )   (bignum_data32(x)[1+(i)])

/* Flonums and compnums: on 32-bit systems a pad word follows the header
 * so that the doubles are 8-byte aligned; on 64-bit systems the doubles
 * immediately follow the header.
 */
#if defined(BITS_64)
# define FLONUM_PAD_WORDS      0
#else
# define FLONUM_PAD_WORDS      1
#endif
#define FLONUM_DATA_BYTES      (FLONUM_PAD_WORDS*sizeof(word)+sizeof(double))
#define COMPNUM_DATA_BYTES     (FLONUM_PAD_WORDS*sizeof(word)+2*sizeof(double))

#define real_part( x )         (*(double*)(ptrof(x)+1+FLONUM_PAD_WORDS))
#define imag_part( x )         (*((double*)(ptrof(x)+1+FLONUM_PAD_WORDS)+1))

#define global_cell_ref( cp )  (pair_car( cp ))
#define global_cell_set( x, y ) (pair_car( x ) = (y))

#define fixnum_to_char( x )     (((x) << 6) | IMM_CHAR)
#define int_to_char( x )        (((x) << 8) | IMM_CHAR)
#define charcode_as_fixnum( x ) ((x) >> 6)
#define charcode( x )           ((x) >> 8)

#endif

/* eof */
