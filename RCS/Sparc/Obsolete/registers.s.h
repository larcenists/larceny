! Symbolic register definitions.
! Sparc version.
!
! $Id: registers.s.h,v 1.2 91/06/26 16:46:26 lth Exp Locker: lth $

#define E_TOP		o0
#define E_LIMIT		o1
#define STKP		o2
#define RESULT		o3
#define ARGREG2		o4
#define ARGREG3		o5

! g1 *must* be a temp register
#define TMP0		g1

#define TIMER		i0
#define TMP1		i1
#define TMP2		i2
#define TMP3		i3
#define TMP4		i4
#define MILLICODE	i5
#define GLOBALS		i7

#define REG0		l0
#define REG1		l1
#define REG2		l2
#define REG3		l3
#define REG4		l4
#define REG5		l5
#define REG6		l6
#define REG7		l7

! C language registers; use with care.

#define C_PREV_SP       i6
#define C_RESULT        o0
#define C_SP		o6
#define C_RETADDR	o7

