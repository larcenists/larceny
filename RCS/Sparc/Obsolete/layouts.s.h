! -*-Fundamental -*-
!
! $Id: layouts.s.h,v 1.3 91/07/24 11:51:32 lth Exp Locker: lth $

#define TAGMASK			0x07

#define FIX1_TAG            0x0
#define FIX2_TAG            0x4
#define IMM1_TAG            0x2
#define IMM2_TAG            0x6
#define PAIR_TAG            0x1
#define VEC_TAG             0x3
#define BVEC_TAG            0x5
#define PROC_TAG            0x7

! Header tags
#define RES_HDR             0x82
#define VEC_HDR             0xA2
#define BV_HDR              0xC2
#define PROC_HDR            0xFE

! Subtags go in the 'xxx' fields in the low bytes of a vector or bytevector
! header, and are picked from the following set of values:
!
!     0x00, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x18, 0x1C

! Subtags for vector headers.

#define VEC_SUBTAG	0x00
#define CONT_SUBTAG	VEC_SUBTAG
#define RECT_SUBTAG     0x04
#define RAT_SUBTAG      0x08
#define SYM_SUBTAG      0x0C

! Subtags for bytevector headers.

#define BVEC_SUBTAG	0x00
#define STR_SUBTAG	0x04
#define FLO_SUBTAG      0x08
#define COMP_SUBTAG     0x0C
#define BIG_SUBTAG      0x10

! Constants

#define TRUE_CONST	0x00000006
#define FALSE_CONST	0x00000002
#define NIL_CONST	0x0000000A

! Offsets from untagged pointers are denoted by readable names;
! offsets from tagged pointers are prefixed with ``A_''.
!
! Vector offsets skip over the header word, so to
! access the header, the offsets must be adjusted by -4.

#define CAR_OFFSET	0
#define CDR_OFFSET	4
#define VEC_OFFSET	4

#define A_CAR_OFFSET	-1
#define A_CDR_OFFSET	3
#define A_VEC_OFFSET	1

! Procedure layout stuff

! Adjusts for tag and skips header

#define A_PROC_OFFSET	-3

! codevector offset in procedure struct

#define CODEVECTOR	4
#define A_CODEVECTOR	-3

! Offset from start of bytevector header to first instruction

#define CODEOFFSET	4
#define A_CODEOFFSET	-1

! Headers

#define COMPNUM_HDR     (BV_HDR | COMP_SUBTAG)
#define FLONUM_HDR      (BV_HDR | FLO_SUBTAG)
#define BIGNUM_HDR      (BV_HDR | BIG_SUBTAG)
#define RATNUM_HDR      (VEC_HDR | RAT_SUBTAG)
#define RECTNUM_HDR     (VEC_HDR | RECT_SUBTAG)
#define STRING_HDR      (BV_HDR | STR_SUBTAG)
#define BYTEVECTOR_HDR  (BV_HDR | BV_SUBTAG)
#define VECTOR_HDR      (VEC_HDR | VEC_SUBTAG)
#define SYMBOL_HDR      (VEC_HDR | SYM_SUBTAG)