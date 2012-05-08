/* Copyright 2008 Felix S Klock II              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 * 
 * Summary constructor implementations.
 * 
 * (These are somewhat trivial; it might be worthwhile to add some
 *  convenience methods, such as a couple example summary
 *  implementations and/or monolithic summary enumeration function.)
 */ 

#include "larceny.h"
#include "summary_t.h"

static bool my_next_chunk_wrapper( summary_t *this, word **start, word **lim )
{
  bool all_unseen_before; 
  /* (throwing away flag data) */
  return summary_next_chunk_with_flags( this, start, lim, &all_unseen_before );
}

void summary_init( summary_t *summary, 
                   int entries, 
                   bool (*next_chunk)( summary_t *this, 
                                       word **start,
                                       word **lim,
                                       bool *all_unseen_before ) )
{
  summary_init_dispose( summary, entries, next_chunk, NULL, NULL );
}

void summary_init_dispose( summary_t *summary, 
                           int entries, 
                           bool (*next_chunk)( summary_t *this, 
                                               word **start,
                                               word **lim,
                                               bool *all_unseen_before ), 
                           void (*dispose)( summary_t *this ),
                           bool (*filter)( summary_t *this, word w ) )
{
  summary->entries = entries; 
  summary->composed_summary = FALSE;
  summary->enumerate_locations_not_objects = FALSE;
  summary->next_chunk = my_next_chunk_wrapper;
  summary->next_chunk_with_flags = next_chunk;
  summary->next_chunk_enum_locs = NULL;
  summary->dispose = dispose;
  summary->filter = filter;
  summary->filter_loc = NULL;
  summary->cursor1 = summary->cursor2 = 
    summary->cursor3 = summary->cursor4 = NULL;
  summary->icursor1 = summary->icursor2 = 
    summary->icursor3 = summary->icursor4 = 0;
}

void summary_init_locs_dispose( summary_t *summary,
                                int entries,
                                bool (*next_chunk)( summary_t *this,
                                                    loc_t **start,
                                                    loc_t **lim,
                                                    bool *all_unseen_before ),
                                void (*dispose)( summary_t *this ),
                                bool (*filter)( summary_t *this, loc_t l ))
{
  summary->entries = entries;
  summary->composed_summary = FALSE;
  summary->enumerate_locations_not_objects = TRUE;
  summary->next_chunk = NULL;
  summary->next_chunk_with_flags = NULL;
  summary->next_chunk_enum_locs = next_chunk;
  summary->dispose = dispose;
  summary->filter = NULL;
  summary->filter_loc = filter;
  summary->cursor1 = summary->cursor2 = 
    summary->cursor3 = summary->cursor4 = NULL;
  summary->icursor1 = summary->icursor2 = 
    summary->icursor3 = summary->icursor4 = 0;
}

/* may or may not be exported by header file, so redundant declaration here */
void summary_enumerate( summary_t *summary,
                        void (*scanner)(word loc, void *data, unsigned *stats),
                        void *data );

static
void summary_enumerate_composed( summary_t *summary,
                                 void (*scanner)(word loc, 
                                                 void *data, 
                                                 unsigned *stats),
                                 void *data )
{
  if (summary->cursor1 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor1, scanner, data );
  if (summary->cursor2 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor2, scanner, data );
  if (summary->cursor3 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor3, scanner, data );
  if (summary->cursor4 != NULL) 
    summary_enumerate( (summary_t*)summary->cursor4, scanner, data );
}

static 
void summary_enumerate_dispatch( summary_t *summary,
                                 void (*scanner)(word loc, 
                                                 void *data, 
                                                 unsigned *stats),
                                 void *data )
{
  word *p, *q;
  unsigned word_count = 0;
  assert2( ! summary->enumerate_locations_not_objects );
  while( summary_next_chunk( summary, &p, &q ) ) {
    while (p < q) {
      if (*p != 0) {
        if (summary->filter != NULL && 
            ! summary->filter( summary, *p )) {
          p++;
          continue;
        }
        scanner( *p, data, &word_count );
      }
      p++;
    }
  }
}

void summary_enumerate( summary_t *summary,
                        void (*scanner)(word loc, void *data, unsigned *stats),
                        void *data )
{
  if (summary->composed_summary)
    summary_enumerate_composed( summary, scanner, data );
  else
    summary_enumerate_dispatch( summary, scanner, data );
}

struct apply_f_to_summary_obj_entry_data {
  void (*f)( word *addr, void *scan_data );
  void *scan_data;
};

static void apply_f_to_summary_obj_entry( word obj, void *data_orig, 
                                          unsigned *count )
{
  word *w;
  struct apply_f_to_summary_obj_entry_data *data;
  void *scan_data;
  void (*f)( word *addr, void *scan_data );

  data = (struct apply_f_to_summary_obj_entry_data*)data_orig;

  scan_data = data->scan_data;
  f         = data->f;
  w = ptrof(obj);
  if (tagof(obj) == PAIR_TAG) {
    f( w, scan_data );
    w += 1;
    f( w, scan_data );
  } else {
    word words = sizefield( *w ) / 4; /* XXX sizeof(word) for generality? */
    while (words--) {
      w += 1;
      f( w, scan_data );
    }
  }
}

static
void summary_enumerate_locs_dispatch( summary_t *summary, 
                                      void (*scanner)(word *loc, void *data ), 
                                      void *data )
{
  if (! summary->enumerate_locations_not_objects) {
    struct apply_f_to_summary_obj_entry_data summary_data;
    assert2( summary->filter_loc == NULL );
    summary_data.f = scanner;
    summary_data.scan_data = data;
    summary_enumerate( summary, 
                       apply_f_to_summary_obj_entry, 
                       (void*) &summary_data );
  } else {
    loc_t *p, *q;
    bool ign_all_unseen;
    assert2( summary->filter == NULL );
    while( summary_next_chunk_enum_locs( summary, &p, &q, &ign_all_unseen )) {
      while (p < q) {
        if (! loc_clear_p(*p)) {
          if (summary->filter_loc != NULL &&
              ! summary->filter_loc( summary, *p )) {
            p++;
            continue;
          }
          scanner( loc_to_slot(*p), data );
        }
        p++;
      }
    }
  }
}

/* may or may not be exported by header file, so redundant declaration here */
void summary_enumerate_locs( summary_t *summary,
                             void (*scanner)(word *loc, void *data),
                             void *data );

static
void summary_enumerate_locs_composed( summary_t *summary, 
                                      void (*scanner)(word *loc, void *data ),
                                      void *data )
{
  if (summary->cursor1 != NULL) 
    summary_enumerate_locs( (summary_t*)summary->cursor1, scanner, data );
  if (summary->cursor2 != NULL) 
    summary_enumerate_locs( (summary_t*)summary->cursor2, scanner, data );
  if (summary->cursor3 != NULL) 
    summary_enumerate_locs( (summary_t*)summary->cursor3, scanner, data );
  if (summary->cursor4 != NULL) 
    summary_enumerate_locs( (summary_t*)summary->cursor4, scanner, data );
}

void summary_enumerate_locs( summary_t *summary, 
                             void (*scanner)(word *loc, void *data ), 
                             void *data )
{
  if (summary->composed_summary)
    summary_enumerate_locs_composed( summary, scanner, data );
  else
    summary_enumerate_locs_dispatch( summary, scanner, data );
}

struct apply_f_to_summary_obj_entry2_data {
  void (*f)( loc_t loc, void *scan_data );
  void *scan_data;
};

static void apply_f_to_summary_obj_entry2( word obj, void *data_orig, 
                                           unsigned *count )
{
  word *w;
  struct apply_f_to_summary_obj_entry2_data *data;
  void *scan_data;
  void (*f)( loc_t loc, void *scan_data );

  data = (struct apply_f_to_summary_obj_entry2_data*)data_orig;

  scan_data = data->scan_data;
  f         = data->f;
  w = ptrof(obj);
  if (tagof(obj) == PAIR_TAG) {
    f( make_loc(obj, ((byte*)w-(byte*)ptrof(obj))), scan_data );
    w += 1;
    f( make_loc(obj, ((byte*)w-(byte*)ptrof(obj))), scan_data );
  } else {
    word words = sizefield( *w ) / 4; /* XXX sizeof(word) for generality? */
    while (words--) {
      w += 1;
      f( make_loc(obj, ((byte*)w-(byte*)ptrof(obj))), scan_data );
    }
  }
}

static
void summary_enumerate_locs2_dispatch( summary_t *summary, 
                                       void (*scanner)(loc_t loc,
                                                       void *data ), 
                                       void *data )
{
  if (! summary->enumerate_locations_not_objects) {
    struct apply_f_to_summary_obj_entry2_data summary_data;
    assert2( summary->filter_loc == NULL );
    summary_data.f = scanner;
    summary_data.scan_data = data;
    summary_enumerate( summary, 
                       apply_f_to_summary_obj_entry2, 
                       (void*) &summary_data );
  } else {
    loc_t *p, *q;
    bool ign_all_unseen;
    assert2( summary->filter == NULL );
    while( summary_next_chunk_enum_locs( summary, &p, &q, &ign_all_unseen )) {
      while (p < q) {
        if (! loc_clear_p(*p)) {
          if (summary->filter_loc != NULL &&
              ! summary->filter_loc( summary, *p )) {
            p++;
            continue;
          }
          assert_loc_ok( *p );
          scanner( *p, data );
        }
        p++;
      }
    }
  }
}

static
void summary_enumerate_locs2_composed( summary_t *summary, 
                                       void (*scanner)(loc_t loc,
                                                       void *data ),
                                       void *data )
{
  if (summary->cursor1 != NULL) 
    summary_enumerate_locs2( (summary_t*)summary->cursor1, scanner, data );
  if (summary->cursor2 != NULL) 
    summary_enumerate_locs2( (summary_t*)summary->cursor2, scanner, data );
  if (summary->cursor3 != NULL) 
    summary_enumerate_locs2( (summary_t*)summary->cursor3, scanner, data );
  if (summary->cursor4 != NULL) 
    summary_enumerate_locs2( (summary_t*)summary->cursor4, scanner, data );
}

void summary_enumerate_locs2( summary_t *summary, 
                              void (*scanner)(loc_t loc,
                                              void *data ), 
                              void *data )
{
  if (summary->composed_summary) 
    summary_enumerate_locs2_composed( summary, scanner, data );
  else 
    summary_enumerate_locs2_dispatch( summary, scanner, data );
}

void summary_compose( summary_t *fst, summary_t *snd, summary_t *thd, summary_t *fth, 
                      summary_t *recv )
{
  int entries = ((fst==NULL)?0:fst->entries) + ((snd==NULL)?0:snd->entries) + 
    ((thd==NULL)?0:thd->entries) + ((fth==NULL)?0:fth->entries);

  recv->entries = entries;
  recv->composed_summary = TRUE;
  recv->next_chunk = NULL;
  recv->next_chunk_with_flags = NULL;
  recv->dispose = NULL;
  recv->filter = NULL;

  recv->icursor1 = recv->icursor2 = recv->icursor3 = recv->icursor4 = 0;

  recv->cursor1 = fst;
  recv->cursor2 = snd;
  recv->cursor3 = thd;
  recv->cursor4 = fth;
}

/* eof */
