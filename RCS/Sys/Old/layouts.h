/* -*- Fundamental -*-
 *
 * Layout of continuation and procedure data structures.
 * This file defines symbolic names for interesting fields of the
 * data structures.
 *
 * $Id: layouts.h,v 1.1 91/06/27 16:36:02 lth Exp Locker: lth $
 */

/* Heap continuation offsets (from start of vector header) */

#define HC_HEADER         0
#define HC_DYNLINK        1
#define HC_RETOFFSET      2
#define HC_PROC           3
#define HC_SAVED          HC_PROC

/* Number of "overhead" words in a heap continuation. The procedure is also
 * a saved value, so it's not overhead!
 */

#define HC_OVERHEAD       3

/* Stack continuation offsets (from stack pointer) */

#define STK_RETADDR       0
#define STK_CONTSIZE      1
#define STK_PROC          2
#define STK_SAVED         STK_PROC

/* Ditto overhead for stack. */

#define STK_OVERHEAD      2

/* Procedure layout offsets (from start of procedure header) */

#define PROC_CODEPTR      1
#define PROC_LINK         2
#define PROC_CONSTANTS    3
#define PROC_DATASLOTS    4
