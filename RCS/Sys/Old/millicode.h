/* -*- Fundamental -*-
 *
 * Scheme Runtime System.
 * Millicode offsets.
 *
 * $Id: millicode.h,v 1.1 91/06/23 20:32:34 lth Exp Locker: lth $
 */

#ifdef ASSEMBLY
#define ADJUST		4
#else
#define ADJUST		1
#endif

/* Watch this! */
#define LAST_MILLICODE	16

#define MILLICODE_TABLE_SIZE	(LAST_MILLICODE+1)

#define M_STKOFLOW	(0*ADJUST)
#define M_STKUFLOW	(1*ADJUST)
#define M_ALLOC		(2*ADJUST)
#define M_ALLOCI	(3*ADJUST)
#define M_SETCAR	(4*ADJUST)
#define M_SETCDR	(5*ADJUST)
#define M_VECTORSET	(6*ADJUST)
#define M_GCSTART	(7*ADJUST)
#define M_EXCEPTION	(8*ADJUST)
#define M_ZEROP		(9*ADJUST)
#define M_ADD		(10*ADJUST)
#define M_SUB		(11*ADJUST)
#define M_MUL		(12*ADJUST)
#define M_QUOT		(13*ADJUST)
#define M_REM		(14*ADJUST)
#define M_DIV		(15*ADJUST)
#define M_NEG		(16*ADJUST)
