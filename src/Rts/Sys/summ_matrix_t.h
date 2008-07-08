/* Copyright 2008 Felix S Klock II
 * 
 * $Id: $
 * 
 * Summarization set matrix interface.
 * 
 * Summarization sets (or just "summaries") hold objects to prepare
 * for future collections.  A summary for a region i holds objects
 * that have references to objects in i (a "points-into" set).
 * 
 * Client-selected parameters:
 * c : construction parameter, 0 < c <= 1
 * p : popularity parameter, p >= 2 
 *     [[ perhaps p > 1 suffices, but p >= 2 is easier to reason about. ]]
 * 
 * == WHY A MATRIX? ==
 * 
 * Given a heap of N words with regions of size R words, there will be
 * c*N/R summaries completed or under construction at any point in
 * time.  Each summary contains at most p*N = O(N) objects.
 * 
 * The collector can migrate (ie copy) objects.  When an object A is
 * migrated, any of A's entries in the summaries (both those completed
 * and under construction; see below) must be updated accordingly.
 * 
 * The summary update in response to the collection of a region (which
 * should take O(R) time) must not take O(N) time.  Therefore we
 * cannot scan all of the objects in all of the O(N/R) summaries.
 * 
 * So we adopt a sparse matrix representation for the summary sets,
 * with c*N/R columns and N/R rows.  Each column of the matrix
 * corresponds to the summary for some region r, and the rows of the
 * matrix are the contributions of a region r to the current set of
 * summaries.  Updating the summaries in response migrations within a
 * region r is a matter of scanning the row associated with r; using a
 * summary to guide a collection of r is a matter of scanning the
 * column associated with r.
 * 
 * == INCREMENTAL, CONCURRENT, PARALLEL ==
 * 
 * Summary sets are constructed incrementally: construction of a
 * particular summary may be interrupted by a collection, and thus the
 * entry updates (in response to object migration by the collector)
 * may need to be applied to partially constructed summaries as well
 * as completed summaries.
 * 
 * Summary construction may occur in parallel with the mutator, but
 * *not* with the collector.  That is, the construction code must be
 * thread-safe with respect to any functions the mutator invokes via
 * the write-barrier, but not with respect to functions that the
 * collector invokes.
 * 
 */

#ifndef INCLUDED_SUMM_MATRIX_T_H
#define INCLUDED_SUMM_MATRIX_T_H

#include "larceny-types.h"

struct summ_matrix {
  gc_t *collector;
    /* The garbage collector that uses and controls these summaries. */

  void *data;                   /* Implementation's data */
};

summ_matrix_t *
create_summ_matrix( gc_t *gc, int first_gno, int initial_num_rgns, 
                    double c, double p );

void sm_expand_gnos( summ_matrix_t *summ, int fresh_gno );

void sm_prepare_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim );

void sm_dispose_cols( summ_matrix_t *summ, int col_gno, int col_gno_lim );

void sm_construction_progress( summ_matrix_t *summ, 
                                int* word_countdown,
                                int* object_countdown );

void sm_enumerate_row( summ_matrix_t *summ,
                       int row_gno, 
                       bool (*scanner)(word loc, void *data),
                       void *data );
  /* Calls the scanner function once with each object reference ('loc')
     in row 'row_gno' of the matrix.  Each call passes 'data' along.

     If the scanner function returns TRUE then the object pointer is
     retained in the row, otherwise it is removed.

     XXX (to be determined): can references appear more than once in a column?
     (Deleting one does not mean the others will also be removed.)

     */

void sm_enumerate_col( summ_matrix_t *summ, 
                       int col_gno, 
                       bool (*scanner)(word loc, void *data),
                       void *data );
  /* Calls the scanner function once with each object reference ('loc')
     in column 'col_gno' of the matrix.  Each call passes 'data' along.

     If the scanner function returns TRUE then the object reference is
     retained in the column, otherwise it is removed.

     XXX (to be determined): can references appear more than once in a column?
     (Deleting one does not mean the others will also be removed.)

     */

void sm_add_entry( summ_matrix_t *summ, word source_obj, int target_gno );
  /* Records that source_obj contains a reference into target_gno.
   * 
   * (Note that this is not the main source of such information within
   *  a summary; the summarization process is supposed to be gathering
   *  data on its own.  This is a channel for asynchronous updates from
   *  the write barrier.)
   */

void sm_next_summary( summ_matrix_t *summ, 
                      /* out parameter */ summary_t *column);
  /* Initializes 'column' to iterate over first available column in summ.
   * Note that this does *not* remove that column from the matrix
   * (so repeated calls to this function without intervening invocations 
   *  of sm_dispose_cols should return iterators for the same column).
   */

/* Functions below are for use when this structure is being used in a
 * concurrent (ie multi-threaded) regional collector. */

void sm_add_entry_concurrent( summ_matrix_t *summ, 
                              word source_obj,
                              int target_gno );

void sm_construction_concurrent( summ_matrix_t *summ,
                                 int grain_scan_words,
                                 int grain_scan_objects );

void sm_interrupt_construction( summ_matrix_t *summ );

#endif /* INCLUDED_SUMM_MATRIX_T_H */

/* eof */
