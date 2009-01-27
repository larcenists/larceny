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

struct event_phase_stats {
  unsigned current_real;
  unsigned current_cpu;
  unsigned min_real;
  unsigned min_cpu;
  unsigned max_real;
  unsigned max_cpu;
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

static void clear_phase_stats( struct event_phase_stats *s )
{
  s->current_real = 0;
  s->current_cpu  = 0;
  s->min_real     = 0;
  s->min_cpu      = 0;
  s->max_real     = 0;
  s->max_cpu      = 0;
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

  return log;
}

#define ENTRY_ELAPSED( isreal, entry ) \
  ((isreal)?(entry).elapsed_real:(entry).elapsed_cpu)

static void update_ref_to_start( gc_mmu_log_t *log,
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
    unshifted_plus_new = size + new_elapsed + r->entry_offset;
  }

  /* At this point, the conceptual window starts at the beginning of
   * the entry at log->buffer.first and we've enqueued an entry that
   * overflows the window; so we need to adjust the window
   * accordingly. */

  assert( unshifted_plus_new >= size );

  /* Step through log until we can go no further. */
  { 
    int i, shifted_plus_new, elapsed;
    i = log->buffer.first;
    shifted_plus_new = unshifted_plus_new;
    elapsed = ENTRY_ELAPSED( isreal_and_not_cpu, log->buffer.entries[i] );
    while ((shifted_plus_new - elapsed) > size) {
      assert( i != log->buffer.end );
      shifted_plus_new -= elapsed;
      i = ((i+1) % log->buffer.capacity);
      elapsed = ENTRY_ELAPSED(isreal_and_not_cpu, log->buffer.entries[i]);
    }
    assert( elapsed == ENTRY_ELAPSED( isreal_and_not_cpu, log->buffer.entries[i] ));
    assert( (shifted_plus_new - elapsed) <= size );
    assert( shifted_plus_new >= size );

    r->buf_idx = i;
    r->entry_offset = size - (shifted_plus_new - elapsed);
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

  update_ref_to_start( log, w, &w->window_start_real, elapsed_real, TRUE );
  update_ref_to_start( log, w, &w->window_start_cpu,  elapsed_cpu, FALSE );

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

static void enqueue( gc_mmu_log_t *log,
                     gc_log_phase_t incoming,
                     unsigned elapsed_real,
                     unsigned elapsed_cpu )
{
  int enq = log->buffer.end;

  log->buffer.entries[enq].phase = incoming;
  log->buffer.entries[enq].elapsed_real = elapsed_real;
  log->buffer.entries[enq].elapsed_cpu  = elapsed_cpu;

  enq = (enq+1) % log->buffer.capacity;
  if ( enq == log->buffer.first ) {
    panic_exit( "Cannot enqueue event in MMU log of capacity %d entries "
                "(disable MMU or increase its capacity).",
                log->buffer.capacity );
  }
  log->buffer.end = enq;

  update_windows_drop_outdated( log, incoming, elapsed_real, elapsed_cpu );
}

EXPORT
void gc_mmu_log_phase_shift( gc_mmu_log_t *log, 
                             gc_log_phase_t prev, gc_log_phase_t next ) 
{
  unsigned finis_real = osdep_realclock();
  unsigned finis_cpu  = osdep_cpuclock();
  unsigned start_real, start_cpu;
  unsigned elapsed_real, elapsed_cpu;

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
