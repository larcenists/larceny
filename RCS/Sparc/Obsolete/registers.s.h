! Symbolic register definitions.
! Sparc version.
!
! $Id: registers.s.h,v 1.1 91/06/20 15:49:02 lth Exp Locker: lth $

#define E_TOP		o0
#define E_LIMIT		o1
#define STKP		o2
#define RESULT		o3
#define ARGREG2		o4
#define ARGREG3		o5

! g1 *must* be a temp register
#define TMP0		g1

#define TIMER		l0
#define TMP1		l1
#define TMP2		l2
#define TMP3		l3
#define TMP4		l4
#define TMP5		l5
#define MILLICODE	l6
#define GLOBALS		l7

#define REG0		i0
#define REG1		i1
#define REG2		i2
#define REG3		i3
#define REG4		i4
#define REG5		i5
#define REG6		i6
#define REG7		i7

! C language registers; use with care.

#define C_RESULT        o0
#define C_SP		o6
#define C_RETADDR	o7

