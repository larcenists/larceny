/* -*- Fundamental -*-
 *
 * Scheme Run-Time System
 * Machine and representation dependent (and independent) macros,
 * including tag definitions and tag masks, and interesting constants.
 *
 * For C-language routines.
 *
 * $Id: macros.h,v 1.4 91/12/06 15:06:43 lth Exp Locker: lth $
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

