/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- event log providing minimum mutator
 * utilization (MMU) data
 */

#include <stdio.h>

#include "larceny.h"
#include "gc_mmu_log.h"

#define EXPORT

#if 0
#define CHECK_REP( log ) do { check_rep( log ); } while (0) 
#else 
#define CHECK_REP( log ) do {                   } while (0)
#endif
static void check_rep( gc_mmu_log_t *log );

struct event_phase_stats {
  int current_real;
  int current_cpu;
  int min_real;
  int min_cpu;
  int max_real;
  int max_cpu;
};

struct ref_to_window_start {
  int buf_idx;
  int entry_offset;
  /* Represents back pointers to the start of a window in the log
   * buffer.  (As new events are enqueued, the window shifts forward.)
   * The window *conceptually* starts at 
   *    STARTTIME(log->buffer.entries[buf_idx]) + entry_offset
   * 
   * If buf_idx is negative, then not enough phase events have been
   * enqueued to fill up the window.  In this case, the window starts
   * at STARTTIME(log->buf_first); log->buf_first should be 0 in any
   * case where this situation arises).
   * In this case, the entry_offset is *not* an offset in some
   * particular entry, but rather sums of the total lengths of the
   * events accumulated so far.
   *
   * When no references remain to an entry in the buffer, it can be
   * "dequeued" by shifting the buffer start pointer forward.
   */
};

struct event_window { 
  int size; /* size of the window in milliseconds */ 

  /* invariant: one entry for each gc_log_phase_t */
  struct event_phase_stats mutator;
  struct event_phase_stats misc_memmgr;
  struct event_phase_stats minorgc;
  struct event_phase_stats majorgc;
  struct event_phase_stats summarize;
  struct event_phase_stats smircy;

  struct ref_to_window_start window_start_real;
  struct ref_to_window_start window_start_cpu;
};

struct log_entry {
  gc_log_phase_t phase;
  unsigned elapsed_real;
  unsigned elapsed_cpu;
};

struct gc_mmu_log {
  struct {
    struct log_entry *entries;
    int capacity;
    int first;
    int end;
  } buffer;
  /* buffer is made up of entries: 
   *   [ entries[i] | f <= i < min(e,k) ] ++ [ buf[i] | 0 <= i < f ]
   * where 
   *   f = first 
   *   e = last 
   *   k = capacity
   */

  struct {
    gc_log_phase_t phase;
    unsigned start_real;
    unsigned start_cpu;
  } in_progress;

  struct {
    struct event_window *array;
    int len;
  } windows;
};

static int check_rep_total_log_time( gc_mmu_log_t *log, bool isreal_notcpu )
{
  int i, count;
  count = 0;
  for ( i = log->buffer.first; 
        i < log->buffer.end; 
        i = (i+1)%log->buffer.capacity ) {
    if (isreal_notcpu)
      count += log->buffer.entries[i].elapsed_real;
    else 
      count += log->buffer.entries[i].elapsed_cpu;
  }
  return count;
}

static int check_rep_total_phase_time_in_window( gc_mmu_log_t *log,
                                                 struct event_window *w,
                                                 gc_log_phase_t phase, 
                                                 bool isreal_notcpu )
{
  int i, start, end, cap, count; 
  if (isreal_notcpu) {
    start = w->window_start_real.buf_idx;
  } else {
    start = w->window_start_cpu.buf_idx;
  }
  if (start == -1) {
    start = log->buffer.first;
    count = 0;
  } else if (log->buffer.entries[start].phase == phase) {
    /* correct for first entry that we disregard as not
       part of the window */
    count = ( isreal_notcpu
              ? - w->window_start_real.entry_offset
              : - w->window_start_cpu.entry_offset );
  } else {
    count = 0;
  }
  end = log->buffer.end;
  cap = log->buffer.capacity;

  assert( start >= 0 );
  assert( start < cap );

  for( i = start; i != end; i = (i+1)%cap ) {
    if (log->buffer.entries[i].phase == phase) {
      int entry_time = ((isreal_notcpu) 
                        ? log->buffer.entries[i].elapsed_real
                        : log->buffer.entries[i].elapsed_cpu );
      count += entry_time;
    }
  }
  return count;
}

static void check_rep_window_start( gc_mmu_log_t *log,
                                    struct event_window *w,
                                    struct ref_to_window_start *s, 
                                    bool isreal_notcpu )
{
  struct { 
    int mutator;
    int misc_memmgr;
    int minorgc;
    int majorgc;
    int summarize;
    int smircy;
  } total_phase_time;

  total_phase_time.mutator = 
    check_rep_total_phase_time_in_window( log, w, gc_log_phase_mutator, 
                                          isreal_notcpu );
  total_phase_time.misc_memmgr = 
    check_rep_total_phase_time_in_window( log, w, gc_log_phase_misc_memmgr, 
                                          isreal_notcpu );
  total_phase_time.minorgc = 
    check_rep_total_phase_time_in_window( log, w, gc_log_phase_minorgc, 
                                          isreal_notcpu );
  total_phase_time.majorgc = 
    check_rep_total_phase_time_in_window( log, w, gc_log_phase_majorgc, 
                                          isreal_notcpu );
  total_phase_time.summarize = 
    check_rep_total_phase_time_in_window( log, w, gc_log_phase_summarize, 
                                          isreal_notcpu );
  total_phase_time.smircy = 
    check_rep_total_phase_time_in_window( log, w, gc_log_phase_smircy, 
                                          isreal_notcpu );

  if (s->buf_idx == -1) {
    int total_log_time = check_rep_total_log_time(log, isreal_notcpu);
    /* means window is not yet filled */
    assert( total_log_time < w->size );
  } else {
    assert( (total_phase_time.mutator + 
             total_phase_time.misc_memmgr + 
             total_phase_time.minorgc + 
             total_phase_time.majorgc + 
             total_phase_time.summarize + 
             total_phase_time.smircy) == w->size );
  }

  assert( total_phase_time.mutator == 
          ((isreal_notcpu) 
           ? w->mutator.current_real 
           : w->mutator.current_cpu) );
  assert( total_phase_time.misc_memmgr == 
          ((isreal_notcpu) 
           ? w->misc_memmgr.current_real 
           : w->misc_memmgr.current_cpu) );
  assert( total_phase_time.minorgc == 
          ((isreal_notcpu) 
           ? w->minorgc.current_real 
           : w->minorgc.current_cpu) );
  assert( total_phase_time.majorgc == 
          ((isreal_notcpu) 
           ? w->majorgc.current_real 
           : w->majorgc.current_cpu) );
  assert( total_phase_time.summarize == 
          ((isreal_notcpu) 
           ? w->summarize.current_real 
           : w->summarize.current_cpu) );
  assert( total_phase_time.smircy == 
          ((isreal_notcpu) 
           ? w->smircy.current_real 
           : w->smircy.current_cpu) );
}

static void check_rep_window( gc_mmu_log_t *log, struct event_window *w )
{
  check_rep_window_start( log, w, &w->window_start_real, TRUE );
  check_rep_window_start( log, w, &w->window_start_cpu, FALSE );
}

static void check_rep_windows( gc_mmu_log_t *log ) 
{
  int i; 
  for ( i = 0 ; i < log->windows.len; i++ ) {
    check_rep_window( log, &log->windows.array[i] );
  }
}

static void check_rep( gc_mmu_log_t *log ) 
{
  check_rep_windows( log );
}

static void clear_phase_stats( struct event_phase_stats *s )
{
  s->current_real = 0;
  s->current_cpu  = 0;
  s->min_real     = (int)(((unsigned)~0) >> 1);
  s->min_cpu      = (int)(((unsigned)~0) >> 1);
  s->max_real     = 0;
  s->max_cpu      = 0;

  assert( (s->min_real > 0) );
  assert( (s->min_real+1 < 0) );
}

static void clear_event_window( struct event_window *e )
{
  clear_phase_stats( &e->mutator );
  clear_phase_stats( &e->misc_memmgr );
  clear_phase_stats( &e->minorgc );
  clear_phase_stats( &e->majorgc );
  clear_phase_stats( &e->summarize );
  clear_phase_stats( &e->smircy );

  e->window_start_real.buf_idx = -1;
  e->window_start_real.entry_offset = 0;
  e->window_start_cpu.buf_idx = -1;
  e->window_start_cpu.entry_offset = 0;
}

EXPORT gc_mmu_log_t *
create_gc_mmu_log( int *window_lens, int buffer_length, gc_log_phase_t init )
{
  gc_mmu_log_t *log;

  log = (gc_mmu_log_t*)must_malloc( sizeof( gc_mmu_log_t ));

  log->buffer.entries = (struct log_entry*)
    must_malloc( buffer_length*sizeof( struct log_entry ));
  log->buffer.capacity = buffer_length;
  log->buffer.first = 0;
  log->buffer.end = 0;

  log->in_progress.phase = init;
  log->in_progress.start_real = osdep_realclock();
  log->in_progress.start_cpu  = osdep_cpuclock();

  { 
    int i;
    i = 0;
    while ( window_lens[i] != -1 ) {
      i++;
    }
    log->windows.len = i;
    log->windows.array = (struct event_window*)
      must_malloc( log->windows.len*sizeof( struct event_window ));
    for ( i = 0; i < log->windows.len; i++ ) {
      clear_event_window( &log->windows.array[i] );
      log->windows.array[i].size = window_lens[i];
    }
  }

  CHECK_REP( log );

  return log;
}

static char*
log_phase_name( gc_log_phase_t p ) 
{
  switch (p) {
  case gc_log_phase_mutator:     return "mutator";
  case gc_log_phase_misc_memmgr: return "misc_memmgr";
  case gc_log_phase_minorgc:     return "minorgc";
  case gc_log_phase_majorgc:     return "majorgc";
  case gc_log_phase_summarize:   return "summarize";
  case gc_log_phase_smircy:      return "smircy";
  default: assert(0);
  }
}

static struct event_phase_stats*
window_phase_stats( gc_mmu_log_t *log, 
                    struct event_window *w, gc_log_phase_t incoming )
{
  struct event_phase_stats *s;

  switch (incoming) {
  case gc_log_phase_mutator: 
    s = &w->mutator; 
    break;
  case gc_log_phase_misc_memmgr:
    s = &w->misc_memmgr;
    break;
  case gc_log_phase_minorgc:
    s = &w->minorgc;
    break;
  case gc_log_phase_majorgc:
    s = &w->majorgc;
    break;
  case gc_log_phase_summarize:
    s = &w->summarize;
    break;
  case gc_log_phase_smircy:
    s = &w->smircy;
    break;
  default: 
    assert(0);
  }

  return s;
}

#define ENTRY_ELAPSED( isreal, entry ) \
  ((isreal)?(entry).elapsed_real:(entry).elapsed_cpu)

/* A new event was just enqueued in w; updates window_start for w,
 * dropping entries that now fall outside the window size, and updates
 * the window's current times accordingly. */
static void remove_lead_entries( gc_mmu_log_t *log,
                                 struct event_window *w, 
                                 struct ref_to_window_start *r,
                                 unsigned new_elapsed,
                                 bool isreal_and_not_cpu ) 
{
  int size = w->size;
  int unshifted_plus_new;
  if (r->buf_idx < 0) {
    /* was still waiting for sufficient events to fill window ... */
    unshifted_plus_new = r->entry_offset + new_elapsed;
    if (unshifted_plus_new < size) {
      /* ... and we're still waiting. */
      r->entry_offset += new_elapsed;
      return;
    } else {
      /* now we have sufficient events to fill window. */
      assert( log->buffer.first == 0 );
    }
  } else {
    /* (increase the window in the other direction so that it starts
     *  at the beginning of the first entry; see comment below.) */
    struct event_phase_stats *s;
    s = window_phase_stats( log, w, log->buffer.entries[r->buf_idx].phase );

    unshifted_plus_new = size + new_elapsed + r->entry_offset;
    if (isreal_and_not_cpu) {
      s->current_real += r->entry_offset;
    } else {
      s->current_cpu += r->entry_offset;
    }
  }

  /* At this point, the conceptual window starts at the beginning of
   * the entry at log->buffer.first and we've enqueued an entry that
   * overflows the window; so we need to adjust the window
   * accordingly. */

  assert( unshifted_plus_new >= size );

  /* Step through log until we can go no further. */
  { 
    int i, shifted_plus_new, elapsed;
    gc_log_phase_t phase;
    struct event_phase_stats *s;
    i = r->buf_idx;
    if (i == -1)
      i = log->buffer.first;
    shifted_plus_new = unshifted_plus_new;
    elapsed = ENTRY_ELAPSED( isreal_and_not_cpu, log->buffer.entries[i] );
    phase = log->buffer.entries[i].phase;
    while ((shifted_plus_new - elapsed) >= size) {
      assert( i != log->buffer.end );
      shifted_plus_new -= elapsed;
      s = window_phase_stats( log, w, phase );

      if (isreal_and_not_cpu) {
        s->current_real -= elapsed;
      } else {
        s->current_cpu  -= elapsed;
      }
      i = ((i+1) % log->buffer.capacity);
      elapsed = ENTRY_ELAPSED(isreal_and_not_cpu, log->buffer.entries[i]);
      phase = log->buffer.entries[i].phase;
    }
    assert( elapsed == ENTRY_ELAPSED( isreal_and_not_cpu, log->buffer.entries[i] ));
    assert( (shifted_plus_new - elapsed) < size );
    assert( shifted_plus_new >= size );
    { 
      struct event_phase_stats *s;
      int partial_elapsed;
      s = window_phase_stats( log, w, phase );
      partial_elapsed = shifted_plus_new - size;
      if (isreal_and_not_cpu) {
        s->current_real -= partial_elapsed;
      } else {
        s->current_cpu  -= partial_elapsed;
      }
      r->buf_idx = i;
      r->entry_offset = partial_elapsed;
    }
  }
}
static int buf_min( gc_mmu_log_t *log, int idx_1, int idx_2 ) 
{
  int buf_end = log->buffer.end;
  if ((idx_1 - buf_end) * (idx_2 - buf_end) < 0) {
    /* one idx has wrapped around but other has not; the "min" is the
     * one closest to buf_end ==> the max. */
    return max( idx_1, idx_2 );
  } else {
    return min( idx_1, idx_2 );
  }
}

/* shifts the buf_idx and buf_entry_offsets forward to reflect the
 * enqueuing of the incoming event.
 * (Note that this may require stepping through an arbitrary number of
 *  events in the log.)
 * Assumes that the incoming event has already been enqueued in the log.
 *
 * Returns the "least" buf_idx installed (where the idx order must
 * consider buffer circularity), which corresponds to the *most*
 * entries we can drop (from this window's perspective).
 */
static int update_window_drop_outdated( gc_mmu_log_t *log,
                                        struct event_window *w, 
                                        gc_log_phase_t incoming, 
                                        unsigned elapsed_real,
                                        unsigned elapsed_cpu )
{
  int new_buf_idx_real, new_buf_idx_cpu;

  remove_lead_entries( log, w, &w->window_start_real, elapsed_real, TRUE );
  remove_lead_entries( log, w, &w->window_start_cpu,  elapsed_cpu, FALSE );

  new_buf_idx_real = w->window_start_real.buf_idx;
  new_buf_idx_cpu  = w->window_start_cpu.buf_idx;

  if (new_buf_idx_real < 0)
    new_buf_idx_real = log->buffer.first;
  if (new_buf_idx_cpu < 0)
    new_buf_idx_cpu = log->buffer.first;

  return buf_min( log, new_buf_idx_real, new_buf_idx_cpu );
}
static void update_windows_drop_outdated( gc_mmu_log_t *log, 
                                          gc_log_phase_t incoming, 
                                          unsigned elapsed_real,
                                          unsigned elapsed_cpu )
{
  struct event_window *w;
  int i;
  int choose_new_first = -1;
  int new_idx;
  for ( i = 0; i < log->windows.len; i++ ) {
    w = &log->windows.array[i];
    new_idx = update_window_drop_outdated( log, w, incoming, 
                                           elapsed_real, elapsed_cpu );
    if (choose_new_first < 0)
      choose_new_first = new_idx;
    else
      choose_new_first = buf_min( log, choose_new_first, new_idx );
  }

#if 0
  consolemsg("pre(first): %d post(first): %d", 
             log->buffer.first, choose_new_first);
#endif
  log->buffer.first = choose_new_first;
}

static void update_phase_stats_add( gc_mmu_log_t *log,
                                    struct event_window *w,
                                    struct event_phase_stats *s,
                                    unsigned elapsed_real, 
                                    unsigned elapsed_cpu ) 
{
  assert2( s->current_real >= 0 );
  assert2( s->current_cpu  >= 0 );

  s->current_real += elapsed_real;
  s->current_cpu  += elapsed_cpu;

  assert2( s->current_real >= 0 );
  assert2( s->current_cpu  >= 0 );
}

static void update_window_incoming_event( gc_mmu_log_t *log,
                                          struct event_window *w, 
                                          gc_log_phase_t incoming,
                                          unsigned elapsed_real,
                                          unsigned elapsed_cpu )
{
  struct event_phase_stats *s;
  s = window_phase_stats( log, w, incoming );
  update_phase_stats_add( log, w, s, elapsed_real, elapsed_cpu );
}

static void update_windows_incoming_event( gc_mmu_log_t *log,
                                           gc_log_phase_t incoming,
                                           unsigned elapsed_real,
                                           unsigned elapsed_cpu )
{
  struct event_window *w;
  int i;
  for ( i = 0; i < log->windows.len; i++ ) {
    w = &log->windows.array[i];
    update_window_incoming_event( log, w, incoming, elapsed_real, elapsed_cpu );
  }
}

static void phase_stats_recalc_minmax( gc_mmu_log_t *log,
                                       struct event_window *w, 
                                       struct event_phase_stats *s )
{
  s->min_real = min( s->current_real, s->min_real );
  s->min_cpu  = min( s->current_cpu,  s->min_cpu );
  s->max_real = max( s->current_real, s->max_real );
  s->max_cpu  = max( s->current_cpu,  s->max_cpu );
}

static void update_window_recalc_minmax( gc_mmu_log_t *log, 
                                         struct event_window *w) 
{
  phase_stats_recalc_minmax( log, w, &w->mutator );
  phase_stats_recalc_minmax( log, w, &w->misc_memmgr );
  phase_stats_recalc_minmax( log, w, &w->minorgc );
  phase_stats_recalc_minmax( log, w, &w->majorgc );
  phase_stats_recalc_minmax( log, w, &w->summarize );
  phase_stats_recalc_minmax( log, w, &w->smircy );
}

static void update_windows_recalc_minmax( gc_mmu_log_t *log ) 
{
  struct event_window *w;
  int i;
  for ( i = 0; i < log->windows.len; i++ ) {
    w = &log->windows.array[i];
    if (w->window_start_real.buf_idx != -1 
        && w->window_start_cpu.buf_idx != -1) {
      update_window_recalc_minmax( log, w );
    }
  }
}

static void enqueue( gc_mmu_log_t *log,
                     gc_log_phase_t incoming,
                     unsigned elapsed_real,
                     unsigned elapsed_cpu )
{
  int enq = log->buffer.end;

  log->buffer.entries[enq].phase = incoming;
  log->buffer.entries[enq].elapsed_real = elapsed_real;
  log->buffer.entries[enq].elapsed_cpu  = elapsed_cpu;

  update_windows_incoming_event( log, incoming, elapsed_real, elapsed_cpu );

  enq = (enq+1) % log->buffer.capacity;
  if ( enq == log->buffer.first ) {
    panic_exit( "Cannot enqueue event in MMU log of capacity %d entries "
                "(disable MMU or increase its capacity).",
                log->buffer.capacity );
  }
  log->buffer.end = enq;

  update_windows_drop_outdated( log, incoming, elapsed_real, elapsed_cpu );
  update_windows_recalc_minmax( log );
}

EXPORT
void gc_mmu_log_phase_shift( gc_mmu_log_t *log, 
                             gc_log_phase_t prev, gc_log_phase_t next ) 
{
  unsigned finis_real = osdep_realclock();
  unsigned finis_cpu  = osdep_cpuclock();
  unsigned start_real, start_cpu;
  unsigned elapsed_real, elapsed_cpu;

  CHECK_REP( log );

  assert( log->in_progress.phase == prev );
  assert( prev != next );

  start_real = log->in_progress.start_real;
  start_cpu  = log->in_progress.start_cpu;

  elapsed_real = finis_real - start_real;
  elapsed_cpu  = finis_cpu  - start_cpu;

  enqueue( log, prev, elapsed_real, elapsed_cpu );

  start_real = osdep_realclock();
  start_cpu  = osdep_cpuclock();

  log->in_progress.phase = next;
  log->in_progress.start_real = start_real;
  log->in_progress.start_cpu  = start_cpu;

  CHECK_REP( log );
}

static void print_event_stats( gc_mmu_log_t *log, struct event_window *w, 
                               char *name, struct event_phase_stats *s, 
                               FILE *f )
{
  fprintf( f, "(%s ", name);
  fprintf( f, "(min (real %d cpu %d)) ",  s->min_real,     s->min_cpu  );
  fprintf( f, "(curr (real %d cpu %d)) ", s->current_real, s->current_cpu );
  fprintf( f, "(max (real %d cpu %d)) ",  s->max_real,     s->max_cpu  );
  fprintf( f, ") ");
}
static void print_window( gc_mmu_log_t *log, struct event_window *w, FILE *f )
{
  fprintf( f, "(window (size %d) ", w->size );
  print_event_stats( log, w, "mutator",   &w->mutator,     f );
  print_event_stats( log, w, "memmgr",    &w->misc_memmgr, f );
  print_event_stats( log, w, "minorgc",   &w->minorgc,     f );
  print_event_stats( log, w, "majorgc",   &w->majorgc,     f );
  print_event_stats( log, w, "summarize", &w->summarize,   f );
  print_event_stats( log, w, "smircy",    &w->smircy,      f );
  fprintf( f, ") " );
}
static void print_windows( gc_mmu_log_t *log, FILE* f ) 
{
  struct event_window *w;
  int i;
  for ( i = 0; i < log->windows.len; i++ ) {
    w = &log->windows.array[i];
    print_window( log, w, f );
  }
}

EXPORT
void gc_mmu_log_print_data( gc_mmu_log_t *log, FILE *f )
{
  fprintf( f, "#(gc_mmu_log_t " );
  print_windows( log, f );
  fprintf( f, ") ");
}
