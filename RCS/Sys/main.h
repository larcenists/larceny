/* -*- Fundamental -*-
 *
 * Scheme Run-time System
 * Procedures and variables defined globally in "main.c".
 *
 * $Id: main.h,v 1.2 91/07/12 03:12:36 lth Exp Locker: lth $
 */
 
extern word globals[];
extern word (*millicode[])();
extern void panic();
extern int debuglevel;
