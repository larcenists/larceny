/* -*- Fundamental -*-
 *
 * Scheme Run-time System
 *
 * Machine selection header file.
 *
 * $Id: machine.h,v 1.1 91/06/26 16:44:02 lth Exp Locker: lth $
 */

/* Let's hope that Sparc1 == Sparc2, always... */

#define SPARC1		1		/* Sparc 1, 1+, SLC, IPC */
#define SPARC2		0		/* Sparc 2 */
#define SYMMETRY	0		/* Sequent Symmetry, 80386 */

#define SPARC	(SPARC1 || SPARC2)
