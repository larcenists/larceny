/* Rts/Sys/remset_t.h
 * Larceny run-time system -- remembered-set data structure.
 *
 * $Id$
 *
 * Remembered sets are objects with operations encoded as
 * function pointers in the instance.  
 *
 * Remembered sets remember objects.  The basic assumption in the current
 * implementation is that there will be one set for each generation, and 
 * it will remember objects in the generation that may contain pointers 
 * to younger generations.
 */

#ifndef INCLUDED_REMSET_T_H
#define INCLUDED_REMSET_T_H

#include "larceny-types.h"

/* Remembered set statistics */

struct remset_stats {
  unsigned ssb_recorded;     /* SSB entries recorded */
  unsigned hash_recorded;    /* Hash table entries recorded */
  unsigned hash_scanned;     /* Hash table entries scanned */
  unsigned words_scanned;    /* Words of objects scanned for pointers */
  unsigned hash_removed;     /* Hash table entries removed */
};

remset_t *
create_remset( unsigned tbl_ent, unsigned pool_ent, unsigned ssb_ent,
	       word **ssb_bot_loc, word **ssb_top_loc, word **ssb_lim_loc );

struct remset {
  void (*clear)( remset_t *remset );
    /* Method that clears the set of its contents; this is needed after 
       data are promoted out of the generations the remembered set belongs to.
       */

  int  (*compact)( remset_t *remset );
    /* A method that moves the contents of the SSB into the set and clears
       the SSB.  It returns 1 if the remembered set overflowed during the
       compaction.
       */
     
  void (*enumerate)(remset_t *remset, int (*scanner)(word,void*, unsigned*),
		    void*data);
    /* A method that takes a scanner function and calls it once with each
       object pointer remembered by the set; the scanner function can then 
       scan the object looking for pointers into younger generations.  If 
       the scanner returns 1, the remembered pointer is retained; if it 
       returns 0, the pointer may be removed from the set.
       */

  void (*stats)( remset_t *remset, remset_stats_t *stats );
    /* A method that fills the statistics structure.
       */

  int  (*has_overflowed)( remset_t *remset );
    /* A method that returns 1 if the remembered set has filled to 
       overflowing since the last time it was cleared.
       */

  void (*assimilate)( remset_t *borg, remset_t *human );
    /* A method that folds the set 'human' into the set 'borg'.
       */

  /* For the write barrier. */
  word **ssb_bot;
  word **ssb_top;
  word **ssb_lim;

  /* Private */
  void *data;
};

#define rs_clear( r )            ((r)->clear( r ))
#define rs_compact( r )          ((r)->compact( r ))
#define rs_enumerate( r, s, d )  ((r)->enumerate( r, s, d ))
#define rs_stats( r, s )         ((r)->stats( r, s ))
#define rs_has_overflowed( r )   ((r)->has_overflowed( r ))
#define rs_assimilate( r1, r2 )  ((r)->assimilate( r1, r2 ))

#endif /* INCLUDED_REMSET_T_H */

/* eof */
