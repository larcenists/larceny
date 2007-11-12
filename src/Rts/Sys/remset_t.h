/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Remembered-set interface.
 *
 * Remembered sets remember objects.  The basic assumption in the current
 * implementation is that there will be one set for each generation, and 
 * it will remember objects in the generation that may contain pointers 
 * to generations that will be collected before the generation with which
 * the set is collected (a "points-out" set).
 */

#ifndef INCLUDED_REMSET_T_H
#define INCLUDED_REMSET_T_H

#include "config.h"
#include "larceny-types.h"
#include "seqbuf_t.h"

struct remset {
  int identity;
    /* A positive integer that identifies this set uniquely.
       */

  int live;
    /* Number of live entries in the set.  This is imprecise only
       when the SSB is non-empty.
       */

  bool has_overflowed;
    /* TRUE if the remembered set node pool has overflowed since the
       last time the set was cleared.
       */

  void *data;			/* Implementation's data */
};

remset_t *
create_remset( int tbl_ent,	   /* Number of entries in hash table */
	       int pool_ent        /* Number of entries in initial node pool */
	       );
  /* Create a remembered set and return a pointer to it.

     The parameters tbl_ent, pool_ent, and ssb_ent can all be zero, in
     which case default values are used.

     The parameters ssb_bot_loc, ssb_top_loc, and ssb_lim_loc are all
     pointers to the client's locations for these values, allowing the
     variables to be shared between the client and the remembered set
     and allowing these variables for all the remembered sets to be
     located together in an array (for the benefit of assembly code).

     tbl_ent >= 0 must be a power of 2.
     pool_ent >= 0
     ssb_ent >= 0 
     */

remset_t *
create_labelled_remset( int tbl_ent,
			int pool_ent,
			int major_id,
			int minor_id );
  /* Exactly like create_remset except that the given major and minor ID
     are used to label the remset in the stats() module.
     */

void rs_clear( remset_t *remset );
  /* Clears the remembered set.
     */

bool rs_add_elem( remset_t *rs, word w );
  /* Copies w into the remset.rs.
     w is subject to a collision check.

     Returns TRUE if the remset overflowed during the addition.
     */

bool rs_add_elems( remset_t **remset, word *bot, word *top );
  /* Copies the elements in the buffer [bot,top) into remset[i],
     where i is the gno for each element.
     Every element in the buffer is subject to a collision check.

     Returns TRUE if the remset overflowed during the addition.
     */

bool rs_compact( remset_t *remset );
  /* Moves the contents of the set's SSB into the set and clears the SSB.
     Only entries not already in the set are added to the set, so every
     element in the SSB is subject to a collision check.

     Returns TRUE if the remembered set overflowed during the compaction.
     */

bool rs_compact_nocheck( remset_t *remset );
  /* Moves the contents of the set's SSB into the set and clears the SSB.
     Entries are added to the set without checking for duplicates.

     Returns TRUE if the remembered set overflowed during the compaction.
     */
     
void rs_enumerate( remset_t *remset, 
		   bool (*scanner)(word loc, void *data, unsigned *stats),
		   void *data );
  /* Calls the scanner function once with each object pointer ('loc') in
     the remembered set.  Each call also passes 'data' and a pointer to
     a statistics variable.

     The scanner function should add the number of words it scans to the
     statistics variable.

     If the scanner function returns TRUE then the object pointer is
     retained in the set, otherwise it is removed.
     */

int  rs_size( remset_t *remset );
  /* Returns the amount of memory occupied by the remembered set's data
     structures, including its SSB, in bytes.
     */

void rs_stats( remset_t *remset );
  /* Add current counters to the global accumulators.
     */

void rs_assimilate( remset_t *dest, remset_t *src );
  /* Folds the set 'src' into the set 'dest'.
     */

void rs_assimilate_and_clear( remset_t *dest, remset_t *src );
  /* Folds the set 'src' into the set 'dest', and clears 'src'.
     */

bool rs_isremembered( remset_t *rs, word w );
  /* Returns TRUE if the object denoted by w is in the remembered set.
     */

void rs_consistency_check( remset_t *rs, int gen_no );
  /* Perform a consistency check and print some statistics.  Useful mainly
     when called from an interactive debugger.  If gen_no >= 0 then a
     check will be performed that every entry in the set points to a
     heap page with that generation number, and that it points to
     an apparently valid object.
     */

#if defined(REMSET_PROFILE)
void rs_print_crossing_stats( remset_t *rs );
  /* Prints out a bunch of information about the volume of pointers from
     the remembered set into various generations.
     */
#endif
#endif /* INCLUDED_REMSET_T_H */

/* eof */
