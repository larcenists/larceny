/*
 * User-defined parameters for garbage collector.
 *
 * $Header$
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
#define MIN_T_SIZE          (1024*1024)     /* Minimum size of tenured area */
#define MIN_E_SIZE          (128*1024)    /* Minimum size of ephemeral area */
#define MIN_STK_SIZE        (8*1024)         /* Minimum size of stack cahce */


/*
 * Size, in words, of the largest continuation frame that can possibly be
 * created. If your application can create an arbitrarily large continuation
 * frame, you're up a creek.
 */
#define MAX_CONT_SIZE       258             /* Size of largest continuation */


/*
 * oflosize() is used to compute the size of the stack overflow area
 * (in the ephemeral area), as a function of the stack size.
 * The overflow area must be big enough to hold as many continuations as 
 * can fit on the stack in the worst case. (Don't forget that space is 
 * allocated in 8-byte chunks!)
 *
 * 'stksize' is given in bytes; 'oflosize()' must return a result in words.
 */

/* (This definition is almost certainly wrong) */

#define oflosize( stksize )  ((stksize) + ((stksize)/16))
