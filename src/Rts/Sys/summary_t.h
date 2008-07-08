/* Copyright 2008 Felix S Klock II 
 */
#ifndef INCLUDED_SUMMARY_T_H
#define INCLUDED_SUMMARY_T_H

struct summary {
  int entries;
    /* Count of entries in summary.
       This number is at *least* the number of distinct words in the 
       summary, and at *most* an upper bound on the number of words 
       that a series of invocations of next_chunk would iterate over.

       (The process of iteration does not modify this value; it is 
       set at construction time and left alone thereafter.)
     */

  bool (*next_chunk)( summary_t *this, /* remaining are "out" parameters */
                      word **start, word **lim, bool *duplicate_entries );
    /* If returns false, then this is exhausted, and values of out 
       parameters are unspecified.  If returns true, then [*start,*lim) 
       are a range of words W held in this, and *duplicate_entries tells 
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
                                       bool *duplicate_entries ) );

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *duplicate_entries ), 
                           void (*dispose)( summary_t *this ) );

#define summary_next_chunk( summary, start_var, lim_var, dupl_var ) \
    ((summary)->next_chunk( summary, start_var, lim_var, dupl_var ))

#define summary_dispose( summary ) \
  do { if ((summary)->dispose) { (summmary)->dispose( summary ); } while (0)

#endif
