/* -*- Fundamental -*-
 *
 * Scheme Run-time System
 *
 * Machine selection header file.
 *
 * $Id: machine.h,v 1.2 91/07/12 03:13:41 lth Exp $
 */

/* Let's hope that Sparc1 == Sparc2, always... */

#define SPARC1		1		/* Sparc 1, 1+, SLC, IPC */
#define SPARC2		0		/* Sparc 2 */
#define SYMMETRY	0		/* Sequent Symmetry, 80386 */

#define SPARC	(SPARC1 || SPARC2)
