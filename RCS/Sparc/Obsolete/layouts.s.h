! -*-Fundamental -*-
!
! $Id: layouts.s.h,v 1.2 91/07/12 03:11:47 lth Exp Locker: lth $

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
#define CONT_SUBTAG	0x04

! Subtags for bytevector headers.

#define STR_SUBTAG	0x00
#define BVEC_SUBTAG	0x04

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
