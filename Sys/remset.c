/*
 * This is the file Sys/remset.c.
 *
 * Larceny run-time system -- remembered set implementation.
 *
 * History
 *   June 27 - July 1, 1994 / lth (v0.20)
 *     Created for Larceny v0.20.
 *
 * The remembered set is implemented as a static store buffer for recording
 * references, combined with a simple chaining hash table to remove duplicates.
 *
 * The hash table starts at REMSET_TBLBOT and extends to REMSET_TBLLIM;
 * it is allocated at startup and never changes in size. Its size is always
 * a power of two. Each entry is a single word and is a pointer into the
 * pool of nodes.
 *
 * The pool of nodes for the hash table starts at REMSET_POOLBOT and
 * extends to REMSET_POOLLIM. The pool is a sequential array of words.
 * Each node takes up two words: the first is a Scheme object (a root, as
 * it were), the second is either 0 or a pointer to the next node in the
 * chain. REMSET_POOLTOP points to the first free word.
 *
 * In addition there is a fixed-size static store buffer (SSB) used by
 * the mutator to record references to tenured objects when intergenerational
 * pointers are created. This buffer starts at SSBBOT and extends to
 * SSBLIM, traced by SSBTOP. When the SSB fills up, it is moved into the
 * hash table; if the hash table fills up, a GC must be performed, otherwise 
 * the program gets to continue.
 *
 * The implementation was inspired by a description in the following paper:
 *   Anthony L. Hosking, J. Eliot B. Moss, and Darko Stefanovic:
 *   "A comparative performance evaluation of write barrier implementations"
 *   Proceedings of OOPSLA '92, pp 92-109.
 * Their implementation has a rather more complex hash table.
 *
 * The main advantages of this implementation are:
 *  - simplicity
 *  - scanning during an ephemeral collection takes time proportional to
 *    the number of remembered objects (Hosking's scheme appears to take time
 *    proportional to the size of the hash table).
 *  - decent memory utilization
 *
 * The main disadvantage with a hash table is that a fair bit of work
 * has to be performed when it is cleared. If this becomes a problem, we
 * can use a card-marking scheme to clear only those parts of it which have
 * been dirtied.
 */

#include <sys/types.h>
#include "larceny.h"
#include "macros.h"
#include "cdefs.h"

static int log2();

/* 
 * Initialize the remembered set. Alters the program break.
 * Tblsize must be power of 2; if it is not, the default size is used.
 *
 * Returns 1 if initialization went OK, 0 if it did not (no memory).
 */
int create_remset( tblsize, poolsize, ssbsize )
int tblsize;  /* size of hash table, 0 = default */
int poolsize; /* size of remset, 0 = default */
int ssbsize;  /* size of ssb, 0 = default */
{
  word *heapptr;

  heapptr = (word*)sbrk(0);

  /* round up heap ptr to 8-byte boundary */
  heapptr = (word*)sbrk( roundup8( (unsigned)heapptr ) - (word)heapptr );
  if ((caddr_t)heapptr == (caddr_t)-1)
    return 0;
  
  if (poolsize == 0) poolsize = DEFAULT_REMSET_POOLSIZE;
  if (tblsize == 0) tblsize = DEFAULT_REMSET_TBLSIZE;
  if (ssbsize == 0) ssbsize = DEFAULT_SSB_SIZE;

  if (log2( tblsize ) == -1)
    tblsize = DEFAULT_REMSET_TBLSIZE;   /* too facist, but works */
  
  if ((caddr_t)sbrk( poolsize * 8 + tblsize * 4 + ssbsize * 4 ) == (caddr_t)-1)
    return 0;

  globals[ G_REMSET_TBLBOT ] = (word)heapptr;
  heapptr += tblsize;
  globals[ G_REMSET_TBLLIM ] = (word)heapptr;
  globals[ G_REMSET_POOLBOT ] = (word)heapptr;
  heapptr += poolsize*2;
  globals[ G_REMSET_POOLLIM ] = (word)heapptr;
  globals[ G_SSBBOT ] = (word)heapptr;
  heapptr += ssbsize;
  globals[ G_SSBLIM ] = (word)heapptr;
  clear_remset();
  return 1;
}


/*
 * Clear the remembered set. The hash table has to be zeroed out;
 * using bzero() (or memset()) is presumably more efficient than
 * a hand-coded loop.
 *
 * WARNING: assumes the null pointer has an all-0 bit pattern.
 */
void clear_remset()
{
  globals[ G_SSBTOP ] = globals[ G_SSBBOT ];
  globals[ G_REMSET_POOLTOP ] = globals[ G_REMSET_POOLBOT ];
  memset( (char*)globals[ G_REMSET_TBLBOT ],
	  0,
	  (int)globals[ G_REMSET_TBLLIM ] - globals[ G_REMSET_TBLBOT ] );
}


/*
 * compact_ssb(): compact SSB.
 *
 * Returns 1 if the SSB was successfully compacted, 0 if the hash table
 * filled up. In the latter case, only a tenuring or full collection will
 * clear it.
 */
int compact_ssb()
{
  word *p, *q, mask, *tbl, w, *b, *pooltop, *poollim, tblsize, h;
  int rval;

  p = (word*)globals[ G_SSBBOT ];
  q = (word*)globals[ G_SSBTOP ];
  pooltop = (word*)globals[ G_REMSET_POOLTOP ];
  poollim = (word*)globals[ G_REMSET_POOLLIM ];
  tbl = (word*)globals[ G_REMSET_TBLBOT ];
  tblsize = (word*)globals[ G_REMSET_TBLLIM ] - tbl;
  mask = tblsize-1;

  memstat_transactions_allocated( q - p );

  /* scan downward to leave a coherent SSB in case the hash table overflows */

  rval = 1;
  while (q > p) {
    w = *(q-1);
    h = (w >> 4) & mask;    /* experimental */
    b = (word*)tbl[ h ];
    while (b != 0 && *b != w) b = (word*)*(b+1);
    if (b == 0) {
      if (pooltop == poollim) {
	rval = 0;
	break;
      }
      *pooltop = w;
      *(pooltop+1) = tbl[h];
      tbl[h] = (word)pooltop;
      pooltop += 2;
    }
    q--;
  }

  /* If the loop ran to completion, q == p == SSBBOT; otherwise, q points to
   * the last completed transaction. Thus, assigning q to SSBTOP throws away
   * exactly those transactions which have been completed.
   */

  globals[ G_REMSET_POOLTOP ] = (word)pooltop;
  globals[ G_SSBTOP ] = (word)q;

  return rval;
}


/*
 * Given a procedure and three arguments for it, apply the procedure to
 * each member of the remembered set (and the extra arguments).
 * Called from the garbage collector; makes the implementation of the
 * remembered set abstract.
 */
void enumerate_remset( scanner, p1, p2, p3 )
int (*scanner)();
word *p1, *p2, **p3;
{
  word *p, *q;

  p = (word*)globals[ G_REMSET_POOLBOT ];
  q = (word*)globals[ G_REMSET_POOLTOP ];
  memstat_transactions_scanned( (q - p)/2 );
  while (p < q) {
    scanner( *p, p1, p2, p3 );
    p += 2;
  }

  /* Sometimes the hash table filled up and we must scan the ssb during
   * root scanning. Note the restrictions on this set forth in gc.c.
   */
  p = (word*)globals[ G_SSBBOT ];
  q = (word*)globals[ G_SSBTOP ];
  memstat_transactions_scanned( q - p );
  while (p < q) {
    scanner( *p, p1, p2, p3 );
    p++;
  }
}


/*
 * If n is a power of 2, return log2(n). Otherwise return -1.
 */
static int log2( n )
unsigned n;
{
  int p;

  p = 0;
  while (n > 0 && (n & 1) == 0) {
    n /= 2;
    p++;
  }
  if (n & 1) {
    n /= 2; p++;
    return (n == 0 ? p : -1);
  }
  else
    return 0;
}

/* eof */
