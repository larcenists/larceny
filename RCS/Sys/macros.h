/* -*- Fundamental -*-
 *
 * Scheme Run-Time System
 * Machine and representation dependent (and independent) macros,
 * including tag definitions and tag masks, and interesting constants.
 *
 * For C-language routines.
 *
 * $Id: macros.h,v 1.3 91/06/26 16:44:44 lth Exp Locker: lth $
 */

/* Type tags as found at the low end of a scheme object pointer/value */
#define FIX1_TAG            0x0		/* fixnum */
#define FIX2_TAG            0x4		/* fixnum */
#define IMM1_TAG            0x2		/* immediate */
#define IMM2_TAG            0x6		/* immediate */
#define PAIR_TAG            0x1		/* pair pointer */
#define VEC_TAG             0x3		/* vector pointer */
#define BVEC_TAG            0x5		/* bytevector pointer */
#define PROC_TAG            0x7		/* procedure pointer */

/* Header tags */
#define RES_HDR             0x82	/* reserved */
#define VEC_HDR             0xA2	/* vector-like */
#define BV_HDR              0xC2	/* bytevector-like */
#define PROC_HDR            0xFE	/* procedure */

/* Subtags go in the 'xxx' fields in the low bytes of a vector or bytevector
 * header, and are picked from the following set of values:
 *
 *     0x00, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x18, 0x1C
 */

/* Subtags for vector headers. */
#define VEC_SUBTAG	0x00		/* vector */
#define CONT_SUBTAG	0x04		/* continuation frame */
#define SYM_SUBTAG      0x0C            /* symbol */

/* Subtags for bytevector headers. */
#define STR_SUBTAG	0x00		/* string */
#define BVEC_SUBTAG	0x04		/* bytevector */

/* Made-up headers */
#define SYMBOL_HDRTAG   (VEC_HDR | SYM_SUBTAG)

/* Constants */
#define TRUE_CONST	0x00000006
#define FALSE_CONST	0x00000002
#define NIL_CONST	0x0000000A

/* Various masks. Change BIT_MASK if your word is not 32 bits long. */
#define TAG_MASK	0x00000007	/* extract bits 2, 1, and 0 */
#define ISHDR_MASK	0x00000083	/* extract bits 7, 1, and 0 */
#define HDR_SIGN	0x00000082	/* header signature */
#define HDR_MASK	0x000000E3	/* Mask to extract header info */
#define BIT_MASK	0x80000000	/* Mask for 'traced' bit */

/* Convert integer to fixnum */
#define fixnum( x )  ((x) << 2)

/* Convert fixnum to integer. Watch the sign! Can't use a shift here (sigh).
 * Might be faster to keep the sign, shift the absvalue, and convert the
 * sign again than to actually divide the signed number.
 */
#define nativeint( x ) ((word) ((long) (x) / 4))

/* Given tagged pointer, return tag */
#define tagof( w )          ((word)(w) & TAG_MASK)

/* Given tagged pointer, return pointer */
#define ptrof( w )          (word *) ((word)(w) & ~TAG_MASK)

/* Given pointer and tag, return tagged pointer */
#define tagptr( w, tag )    (word *)((word)(w) | (tag))

/* extract header tag from a header word */
#define header( w )         ((word) (w) & HDR_MASK)

/* Create a header given integer (not fixnum!) size (in bytes), and tag. */
#define mkheader( size, tag )  (((word) (size) << 8) | (tag))

/* a word is a pointer if the low bit is set */
#define isptr( w )          ((word) (w) & 0x01)

/* a word is a header if it has a header mask layout */
#define ishdr( w )          (((word) (w) & ISHDR_MASK) == HDR_SIGN)

/* extract size field from a header word, accounting for a set hi bit */
#define sizefield( w )      ((((word) (w)) & ~BIT_MASK) >> 8)

/* Is a word a pointer into a particular space? */
#define pointsto( p,lo,hi ) (isptr(p) && ptrof(p) >= (lo) && ptrof(p) <= (hi))

/* miscellaneous */
#define max( a, b )         ((a) > (b) ? (a) : (b))
#define min( a, b )         ((a) < (b) ? (a) : (b))
#define roundup4( a )       (((a) + 3) & ~0x03)
#define roundup8( a )       (((a) + 7) & ~0x07)

/* Manipulating 'traced' bit in vector headers */
#define get_bit( w )        ((w) & BIT_MASK)
#define set_bit( w )        ((w) |= BIT_MASK)
#define reset_bit( w )      ((w) &= ~BIT_MASK)

