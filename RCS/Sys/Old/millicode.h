/*
 * Millicode offsets.
 *
 * $Id$
 */

#ifndef ASSEMBLY

/* C version */

#define MILLICODE_TABLE_SIZE	(LAST_MILLICODE+1)

#define M_STKOFLOW	0
#define M_STKUFLOW	1
#define M_ALLOC		2
#define M_ALLOCI	3
#define M_SETCAR	4
#define M_SETCDR	5
#define M_VECTORSET	6
#define M_GCSTART	7
#define M_WRONGARGS	8
#define M_NONPROC	9
#define M_TIMEREXP	10

#define LAST_MILLICODE	10

#else

/* Assembly version */

#define M_STKOFLOW	0
#define M_STKUFLOW	4
#define M_ALLOC		8
#define M_ALLOCI	12
#define M_SETCAR	16
#define M_SETCDR	20
#define M_VECTORSET	24
#define M_GCSTART	28
#define M_WRONGARGS	32
#define M_NONPROC	36
#define M_TIMEREXP	40

#endif
