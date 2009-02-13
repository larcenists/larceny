/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 */
#ifndef INCLUDED_SUMMARY_T_H
#define INCLUDED_SUMMARY_T_H

#include "larceny-types.h"

struct summary {
  int entries;
    /* Count of entries in summary.
       This number is at *least* the number of distinct words in the 
       summary, and at *most* an upper bound on the number of words 
       that a series of invocations of next_chunk would iterate over.

       (The process of iteration does not modify this value; it is 
       set at construction time and left alone thereafter.)
     */

  bool composed_summary;
    /* (set by summary_compose to establish that this is the
       composition of several sub-summaries.) */

  bool (*next_chunk)( summary_t *this, /* remaining are "out" parameters */
                      word **start, word **lim );
    /* Simple wrapper around next_chunk_with_flags method; 
       client must assume most conservative results for all flags. 
    */ 

  bool (*next_chunk_with_flags)( summary_t *this, /* remaining are "out" parameters */
                                 word **start, word **lim, 
                                 bool *all_unseen_before );
    /* If returns false, then this is exhausted, and values of out 
       parameters are unspecified.  If returns true, then [*start,*lim) 
       are a range of words W held in this, and *all_unseen_before tells 
       whether the members of W may be duplicates of words that may have 
       already been seen in the course of the iteration.
     */

  void (*dispose)( summary_t *this );
    /* If non-null, client is responsible for invoking dispose after
       it has finished its iteration.

       (Note that summary_dispose convenience method below checks for
        NULL, so simplest protocol is to unconditionally invoke that
        macro once and only once after iteration is complete.)
    */

  bool (*filter)( summary_t *this, word w );
    /* If non-null, invoked on words during the enumeration; only
     * words for which this returns TRUE are scanned.
     */

  /*** Implementation private state follows ***/

  /* These fields give implementors option of saving cursor state
   * without allocating (and disposing) privately held state. 
   * Note that the init functions below set all of these fields to
   * NULL or 0 as appropriate; if the implementor wants a different
   * initial state, it must set that itself *after* calling the init
   * function. 
   */
  void *cursor1;
  void *cursor2;
  void *cursor3;
  void *cursor4;
  int  icursor1;
  int  icursor2;
  int  icursor3;
  int  icursor4;
};

void summary_init( summary_t *summary, 
                   int entries, 
                   bool (*next_chunk)( summary_t *this, 
                                       word **start,
                                       word **lim,
                                       bool *all_unseen_before ) );
/*
 * Creates a summary, using remaining arguments to initialize its
 * fields.  For obligations of next_chunk fcn ptr, see description of
 * next_chunk_with_flags field above.
 */

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *all_unseen_before ), 
                           void (*dispose)( summary_t *this ),
                           bool (*filter)( summary_t *this, word w ));
/*
 * Creates a summary, using remaining arguments to initialize its
 * fields.  For obligations of next_chunk fcn ptr, see description of
 * next_chunk_with_flags field above.
 * Any necessary cleanup can be encoded in the dispose fcn ptr.
 */

void summary_compose( summary_t *fst, summary_t *snd, 
                      summary_t *thd, summary_t *fth, summary_t *recv );
  /* Initializes recv so that it iterates through fst, snd, and thd
     each in turn. */

void summary_enumerate( summary_t *summary,
                        void (*scanner)(word loc, void *data, unsigned *stats),
                        void *data );
  /* Invokes scanner on each word produced by iterating through summary.
     Does *not* call summary_dispose when enumeration is complete.
   */

#define summary_next_chunk( summary, start_var, lim_var ) \
    ((summary)->next_chunk( summary, start_var, lim_var ))

#define summary_next_chunk_with_flags( summary, start_var, lim_var, u_var ) \
    ((summary)->next_chunk_with_flags( summary, start_var, lim_var, u_var ))

#define summary_dispose( summary ) \
  do { if ((summary)->dispose) { (summary)->dispose( summary ); }} while (0)

#endif
