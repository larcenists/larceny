/* Copyright 2010 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Location (remembered) set interface.
 *
 * INTRODUCTION
 * ------------
 * Locsets remember locations: slots within objects.
 * 
 * There are almost no a priori assumptions about built-in
 * relationships of locsets and the rest of the heap.
 * 
 * One assumption/invariant that is expected is that the locations are
 * slots within non-bytevector-like objects (vector-likes, pairs, and
 * procedures).  This potentially enables doing tricks like searching
 * backwards for the header of an object, under certain assumptions.
 *
 * There is a decision about what strategy to employ when representing
 * slots within objects: one could represent it by the raw address of
 * the slot, or one could represent it by an object+offset pair.  An
 * advantage of the latter approach is that it is feasible for such a
 * set to be easily updated in response to a relocating garbage
 * collection.  An advantage of the former approach is that such a
 * representation inherently requires half as much space.
 *
 * SOME INITIAL THOUGHTS
 * ---------------------
 * My first attempt at this interface will try to support both
 * strategies, hiding the choice of representation behind an ADT.  But
 * this may not be reasonable.  For example, completely hiding this
 * choice without any jumping-through-hoops would require inputs to a
 * locset take the form of object+offset pairs, while the outputs
 * would take the form of raw pointers, and that would mean that one
 * could not compose such structure: the output from one locset could
 * not be used to construct another.
 * 
 * (It may be feasible to infer object+offset results from a raw
 * location, *if* one can scan backwards to find the beginning of an
 * object.  This is tricky since we have header-less pairs; perhaps
 * our objects with headers should also have footers!  Another
 * approach would be to have appropriate parts of the API also pass
 * along a parameter whether the slot is part of a pair or not, and
 * then use that knowledge to reconstruct object+offset accordingly.
 * Wait, if we get a tagged object initially, maybe we *can* do that
 * already... hmm...)
 * 
 */
#ifndef INCLUDED_LOCSET_T_H
#define INCLUDED_LOCSET_T_H

#include "config.h"
#include "larceny-types.h"
#include "summary_t.h"

struct locset {
  int live; 
    /* Number of live entries in the set. */
  void *data;
};

locset_t *
create_locset( int tbl_ent,        /* Number of entries in hash table */
               int pool_ent        /* Number of entries in initial node pool */
               );
  /* Returns pointer to freshly constructed location set.

     Either or both of the parameters tbl_ent and pool_ent can be
     zero, in which case default values are used.

      tbl_ent >= 0 must be a power of 2.
     pool_ent >= 0
   */

void ls_clear( locset_t *locset );
  /* Clears the location set. */

void ls_add_obj_offset( locset_t *ls, word w, int offset);
  /* Copies slot for w.offset into ls. */
void ls_add_loc( locset_t *ls, loc_t loc );
  /* Copies loc into ls.  Note that this method potentially keeps us
   * from reconstructing <obj,offset> pair (see trio below). */

void ls_add_nonpair( locset_t *ls, word *loc );
void ls_add_paircar( locset_t *ls, word *loc );
void ls_add_paircdr( locset_t *ls, word *loc );
  /* Copies slot corresponding to memory address loc into ls. 
   * 
   * The three kinds of slots are handled independently so that one
   * can potentially reconstruct the <obj,offset> pair by internally
   * tracking non-pair-slots, pair-cars, and pair-cdrs separately.
   */

bool ls_ismember_loc( locset_t *ls, loc_t loc );
bool ls_ismember( locset_t *ls, word *loc );
  /* Is slot corresponding to memory address loc in ls?
   */

void ls_copy_all_from( locset_t *ls, locset_t *source );
  /* Add all locs from source into ls. 
   */

void ls_enumerate_locs( locset_t *ls, 
                        bool (*scanner)(loc_t loc, void *data ),
                        void *data );
void ls_enumerate( locset_t *ls, 
                   bool (*scanner)(word *loc, void *data ),
                   void *data );
  /* Calls the scanner function once with each slot in ls.
   *
   * If the scanner returns TRUE then the loc is retained in the set;
   * otherwise it is removed. */

void ls_init_summary( locset_t *ls, int max_words_per_step,
                      /* out parameter */ summary_t *s );
  /* Exposes iteration over 'ls' via the summary_t abstraction.
   * 's' is mutated so that it traverses the elements of 'ls'.
   * If max_words_per_step is positive, then it bounds the number of 
   * words in the range established by each invocation of the 
   * 's->next_chunk' method; if max_words_per_step is -1, then no such 
   * bound is imposed (though the summary_t may still choose to choose 
   * its own chunk size).
   */

void ls_add_elems_funnel( locset_t *ls, word *bot, word *top );
  /* (placeholders) */

#endif /* INCLUDED_LOCSET_T_H */
/* eof */
