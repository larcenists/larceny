/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- event log providing minimum mutator
 * utilization (MMU) data
 */

typedef enum {
  gc_log_phase_mutator, 
  gc_log_phase_misc_memmgr, gc_log_phase_minorgc, gc_log_phase_majorgc, 
  gc_log_phase_summarize, 
  gc_log_phase_smircy
} gc_log_phase_t;

gc_mmu_log_t *
create_gc_mmu_log( int *window_lens, int buffer_length, gc_log_phase_t init );
  /* Creates an event log accumulating N windows, where N = the length
   * of the longest prefix of positive entries in window_lens.
   * 
   * The i'th window tracks shifts over a window of size W_i milliseconds, 
   * where W_i = windows_lengths[i].  The log is backed by an internal
   * circular buffer of length L, where L = buffer_length.
   * 
   * If phase shifts happen so rapidly that the internal buffer
   * becomes exhausted, then the phase shift method is allowed to die
   * (loudly), drop the lost events (silently), or drop only the
   * windows which are too large to continue maintaining.  Thus this
   * interface is not terribly robust, but it should serve well enough
   * for a short term use.
   *
   * After this constructor returns, it will not retain a reference to
   * the window_lengths array.
   */

void gc_mmu_log_phase_shift( gc_mmu_log_t *log, 
                             gc_log_phase_t prev, gc_log_phase_t next );
  /* requires: prev is phase of last log entry and prev != next
   * 
   * effects: signals a control shift from one phase of runtime to another.
   *
   * (client provides prev so that we can sanity check the phase shifts) 
   */

void gc_mmu_log_print_data( gc_mmu_log_t *log, FILE *f );
  /* prints current MMU log data to f as a Scheme s-exp.
   * 
   * (see format used in stats.c to get a rough idea of how it will
   *  look, though Felix is planning to make this particular output
   *  more self-documenting.)
   */
