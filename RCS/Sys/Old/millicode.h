/* -*- Fundamental -*-
 *
 * Scheme 313 Runtime System
 * Millicode table offsets.
 *
 * $Id: millicode.h,v 1.6 91/08/23 22:12:44 lth Exp Locker: lth $
 */

#ifdef ASSEMBLY
#define M_ADJUST		8
#else
#define M_ADJUST		2
#endif

/* Watch this! */
#define LAST_MILLICODE	60

#define MILLICODE_TABLE_SIZE	(LAST_MILLICODE+1)

#define M_STKOFLOW		(0*M_ADJUST)
#define M_STKUFLOW		((1*M_ADJUST)+1)	/* Warped, I know */
#define M_ALLOC			(2*M_ADJUST)
#define M_ALLOCI		(3*M_ADJUST)
#define M_SETCAR		(4*M_ADJUST)
#define M_SETCDR		(5*M_ADJUST)
#define M_VECTORSET		(6*M_ADJUST)
#define M_GCSTART		(7*M_ADJUST)
#define M_SAVE_CONTEXT		(8*M_ADJUST)
#define M_RESTORE_CONTEXT	(9*M_ADJUST)
#define M_TYPE_EXCEPTION	(10*M_ADJUST)
#define M_ZEROP			(11*M_ADJUST)
#define M_ADD			(12*M_ADJUST)
#define M_SUB			(13*M_ADJUST)
#define M_MUL			(14*M_ADJUST)
#define M_QUOT			(15*M_ADJUST)
#define M_REM			(16*M_ADJUST)
#define M_DIV			(17*M_ADJUST)
#define M_NEG			(18*M_ADJUST)
#define M_CAPTURE		(19*M_ADJUST)
#define M_RESTORE		(20*M_ADJUST)
#define M_TIMER_EXCEPTION       (21*M_ADJUST)
#define M_PROC_EXCEPTION        (22*M_ADJUST)
#define M_ARG_EXCEPTION         (23*M_ADJUST)
#define M_VARARGS               (24*M_ADJUST)
#define M_APPLY                 (25*M_ADJUST)
#define M_NUMEQ                 (26*M_ADJUST)
#define M_NUMLT                 (27*M_ADJUST)
#define M_NUMLE                 (28*M_ADJUST)
#define M_NUMGT                 (29*M_ADJUST)
#define M_NUMGE                 (30*M_ADJUST)
#define M_GARBAGE_COLLECT       (31*M_ADJUST)
#define M_OPEN_FILE             (32*M_ADJUST)
#define M_CLOSE_FILE		(33*M_ADJUST)
#define M_CREATE_FILE		(34*M_ADJUST)
#define M_UNLINK_FILE		(35*M_ADJUST)
#define M_READ_FILE		(36*M_ADJUST)
#define M_WRITE_FILE		(37*M_ADJUST)
#define M_MOD			(38*M_ADJUST)
#define M_COMPLEXP		(39*M_ADJUST)
#define M_REALP			(40*M_ADJUST)
#define M_RATIONALP		(41*M_ADJUST)
#define M_INTEGERP		(42*M_ADJUST)
#define M_EXACTP		(43*M_ADJUST)
#define M_INEXACTP		(44*M_ADJUST)
#define M_EXACT2INEXACT		(45*M_ADJUST)
#define M_INEXACT2EXACT		(46*M_ADJUST)
#define M_MAKE_RECTANGULAR	(47*M_ADJUST)
#define M_REAL_PART		(48*M_ADJUST)
#define M_IMAG_PART		(49*M_ADJUST)
#define M_SQRT			(50*M_ADJUST)
#define M_ROUND			(51*M_ADJUST)
#define M_TRUNCATE		(52*M_ADJUST)
#define M_NOT_SUPPORTED		(53*M_ADJUST)
#define M_DEBUG                 (54*M_ADJUST)
#define M_RESET			(55*M_ADJUST)
#define M_EXIT			(56*M_ADJUST)
#define M_BREAK			(57*M_ADJUST)
#define M_TYPETAG		(58*M_ADJUST)
#define M_TYPETAGSET		(59*M_ADJUST)
#define M_EQV			(60*M_ADJUST)

/* EOF */
