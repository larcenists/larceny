/*
 * User-defined parameters for garbage collector.
 *
 * $Id: gc.h,v 1.5 1992/05/15 22:18:39 lth Exp lth $
 * 
 * Whoever writes the program that interfaces to the collector must define
 * the desired defaults in this file for the garbage collector to use.
 *
 * This file is included once by the garbage collector. In principle you
 * may put procedure definitions in this file, as long as they don't refer
 * to any type or variable declared within the GC module.
 *
 * Assumptions: 1 word = 4 bytes.
 */

/* 
 * The following limits are all in bytes and should be evenly 
 * divisible by 8.
 *
 * MIN_E_SIZE is really a function of the stack size since we need room in
 * the ephemeral area for the stack overflow area; perhaps we should make
 * MIN_E_SIZE a function or macro w/ argument...
 */
#define MIN_S_SIZE          0                /* Minimum size of static area */
#define MIN_T_SIZE          0     /* Minimum size of tenured area */
#define MIN_E_SIZE          0             /* Minimum size of ephemeral area */
#define MIN_STK_SIZE        0                /* Minimum size of stack cahce */
