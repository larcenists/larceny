/* Rts/Sys/macros.h
 * Larceny run-time system -- macros
 *
 * $Id: macros.h,v 1.3 1997/07/07 20:13:53 lth Exp $
 *
 * Machine and representation dependent (and independent) macros,
 * including tag definitions and tag masks, and interesting constants.
 *
 * For C-language routines.
 */

/* Various masks. Change BIT_MASK if your word is not 32 bits long. */
#define ISHDR_MASK	0x00000083	/* extract bits 7, 1, and 0 */
#define HDR_SIGN	0x00000082	/* header signature */
#define HDR_MASK	0x000000E3	/* Mask to extract header info */
#define BIT_MASK	0x80000000	/* Mask for 'traced' bit */

/* Convert integer to fixnum */
#define fixnum( x )  ((x) << 2)

/* Convert fixnum to integer. Watch the sign! Can't use a shift here (sigh).
 * Might be faster to keep the sign, shift the absvalue, and convert the
 * sign again than to actually divide the signed number. Not used much, tho.
 */
#define nativeint( x )      ((word) ((long)(x) / 4))
#define nativeuint( n )     ((word)(n) / 4)

/* Given tagged pointer, return tag */
#define tagof( w )          ((word)(w) & TAG_MASK)

/* Given tagged pointer, return pointer */
#define ptrof( w )          ((word *)((word)(w) & ~TAG_MASK))

/* May be faster on some architectures, and should never be slower (?) */
#define striptag( w, t )   ((word*)((w) ^ t))

/* Given pointer and tag, return tagged pointer */
#define tagptr( w, tag )    ((word)(w) | (tag))

/* extract header tag (i.e. sans typetag) from a header word */
#define header( w )         ((word)(w) & HDR_MASK)

/* extract type tag from a header word */
#define typetag( w )	    ((word)(w) & TYPETAG_MASK)

/* strip type tag from a header word */
#define striptypetag( w )   ((word)(w) & ~TYPETAG_MASK)

/* Create a header given integer (not fixnum!) size (in bytes), and tag. */
#define mkheader( size, tag )  (((word) (size) << 8) | (tag))

/* a word is a pointer if the low bit is set */
#define isptr( w )          ((word)(w) & 0x01)

/* a word is a header if it has a header mask layout */
#define ishdr( w )          (((word)(w) & ISHDR_MASK) == HDR_SIGN)

/* Extract size field from a header word. Hi bit is always 0. */
#define sizefield( w )      ((w) >> 8)

/* Is a word a pointer into a particular space? */
/* OBSOLETE
#define pointsto( p,lo,hi ) (isptr(p) && ptrof(p) >= (lo) && ptrof(p) < (hi))
*/

/* miscellaneous */
#define max( a, b )         ((a) > (b) ? (a) : (b))
#define min( a, b )         ((a) < (b) ? (a) : (b))

#define ceildiv(a,b)        ((a)%(b) == 0 ? (a)/(b) : (a)/(b)+1)

#define roundup( a, m )     ((((a)+((m)-1)) / (m))*(m))
#define roundup2( a )       (((a) + 1) & ~0x01)
#define roundup4( a )       (((a) + 3) & ~0x03)
#define roundup8( a )       (((a) + 7) & ~0x07)

/* The following true on 32-bit machines */
#define roundup_word( a )   roundup4( a )
#define roundup_dword( a )  roundup8( a )

/* Rounding macros for wordsize-independence */
#define roundup_walign(a)   roundup2( a )   /* Word alignment: 2 words */
#define roundup_balign(a)   roundup8( a )   /* Byte alignment: 8 bytes */

/* Manipulating 'traced' bit in vector headers */
/* OBSOLETE
#define get_bit( w )        ((w) & BIT_MASK)
#define set_bit( w )        ((w) |= BIT_MASK)
#define reset_bit( w )      ((w) &= ~BIT_MASK)
*/

/* macros for manipulating Scheme data types */
#define pair_car( ptr )        *ptrof( ptr )
#define pair_cdr( ptr )        *(ptrof( ptr ) + 1)

#define string_length( x )     sizefield( *ptrof( x ) )
#define string_data( x )       ((char*)ptrof( x )+4)

#define gcell_ref( cp )        pair_car( cp )
#define vector_ref( vp, i )    (*(ptrof( vp )+VEC_HEADER_WORDS+(i)))
#define vector_set( vp, i, v ) (*(ptrof( vp )+VEC_HEADER_WORDS+(i)) = (v))

#define bytevector_length(x) (sizefield(*ptrof(x)))
#define bytevector_ref(x,i)  (*((byte*)(ptrof(x)+1)+i))

#define mkbignum_header( sign, length )  (((sign) << 16) | length)
#define bignum_length( x )   (*(ptrof(x)+1) & 0x0000FFFF)
#define bignum_sign( x )     (*(ptrof(x)+1) >> 16)
#define bignum_ref32( x, i ) (*(ptrof(x)+2+i))

#define real_part( x )       (*(double*)(ptrof(x)+2))
#define imag_part( x )       (*((double*)(ptrof(x)+2)+1))

/* eof */
