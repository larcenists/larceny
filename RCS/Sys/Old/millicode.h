/* -*- Fundamental -*-
 *
 * Scheme Runtime System.
 * Millicode offsets.
 *
 * $Id: millicode.h,v 1.2 91/06/26 13:30:02 lth Exp Locker: lth $
 */

#ifdef ASSEMBLY
#define ADJUST		4
#else
#define ADJUST		1
#endif

/* Watch this! */
#define LAST_MILLICODE	18

#define MILLICODE_TABLE_SIZE	(LAST_MILLICODE+1)

#define M_STKOFLOW		(0*ADJUST)
#define M_STKUFLOW		(1*ADJUST)
#define M_ALLOC			(2*ADJUST)
#define M_ALLOCI		(3*ADJUST)
#define M_SETCAR		(4*ADJUST)
#define M_SETCDR		(5*ADJUST)
#define M_VECTORSET		(6*ADJUST)
#define M_GCSTART		(7*ADJUST)
#define M_SAVE_CONTEXT		(8*ADJUST)
#define M_RESTORE_CONTEXT	(9*ADJUST)
#define M_EXCEPTION		(10*ADJUST)
#define M_ZEROP			(11*ADJUST)
#define M_ADD			(12*ADJUST)
#define M_SUB			(13*ADJUST)
#define M_MUL			(14*ADJUST)
#define M_QUOT			(15*ADJUST)
#define M_REM			(16*ADJUST)
#define M_DIV			(17*ADJUST)
#define M_NEG			(18*ADJUST)
