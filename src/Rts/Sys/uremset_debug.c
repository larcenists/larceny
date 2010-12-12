/* Copyright 2010 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Debugging wrapper around uremset_t that verifies dynamic system
 * invariants such as the invariant that elements are not repeated
 * during a traversal.
 *
 */

#include "larceny.h"
#include "remset_t.h"
#include "uremset_t.h"
#include "uremset_debug_t.h"

typedef struct uremset_debug_data uremset_debug_data_t;

struct uremset_debug_data {
  uremset_t *backing_urs;
};

#define DATA(urs) ((uremset_debug_data_t*)(urs->data))

static void assimilate_and_clear( uremset_t *urs, int g1, int g2 )
{
  assert(0);
}
static int live_count( uremset_t *urs, int gno )
{
  return urs_live_count(DATA(urs)->backing_urs, gno );
}
static void expand_remset_gnos( uremset_t *urs, int fresh_gno )
{
  urs_expand_remset_gnos( DATA(urs)->backing_urs, fresh_gno );
}
static void              clear( uremset_t *urs, int gno )
{
  urs_clear( DATA(urs)->backing_urs, gno );
}
static bool       add_elem_new( uremset_t *urs, word w )
{
  return urs_add_elem_new( DATA(urs)->backing_urs, w );
}
static bool           add_elem( uremset_t *urs, word w )
{
  return urs_add_elem( DATA(urs)->backing_urs, w );
}
static bool          add_elems( uremset_t *urs, word *bot, word *top )
{
  return urs_add_elems( DATA(urs)->backing_urs, bot, top );
}

struct debug_scan_wrapper_data {
  remset_t *rs;
  bool (*underlying_scanner)(word loc, void *data);
  void *underlying_scan_data;
};

static bool debug_scan_wrapper( word loc, void *debug_data )
{
  struct debug_scan_wrapper_data *data;
  data = (struct debug_scan_wrapper_data*)debug_data;
  assert( ! rs_isremembered( data->rs, loc ));
  rs_add_elem_new( data->rs, loc );
  return data->underlying_scanner( loc, data->underlying_scan_data );
}

static void init( struct debug_scan_wrapper_data *my_data, 
                  bool (*underlying_scanner)(word loc, void *data), 
                  void *underlying_scan_data )
{
  static remset_t *the_rs = NULL; /* XXX switch to general pool for this stuff */
  if (the_rs == NULL) {
    the_rs = create_remset( 0, 0 );
  }
  my_data->rs = the_rs;
  rs_clear( my_data->rs );
  my_data->underlying_scanner = underlying_scanner;
  my_data->underlying_scan_data = underlying_scan_data;
}

static void enumerate_gno( uremset_t *urs, 
                           bool incl_tag, 
                           int gno, 
                           bool (*scanner)(word loc, void *data), 
                           void *data )
{
  struct debug_scan_wrapper_data my_data;
  init(&my_data, scanner, data);
  urs_enumerate_gno( DATA(urs)->backing_urs, incl_tag, gno, 
                     debug_scan_wrapper, &my_data );
}
static void enumerate_allbutgno( uremset_t *urs, 
                                 bool incl_tag, 
                                 int gno, 
                                 bool (*scanner)(word loc, void *data), 
                                 void *data )
{
  assert(0);
}
static void enumerate_older( uremset_t *urs, 
                             bool incl_tag, 
                             int gno, 
                             bool (*scanner)(word loc, void *data), 
                             void *data )
{
  assert(0);
}
static void enumerate_minor_complement( uremset_t *urs, 
                                        bool incl_tag, 
                                        gset_t gset, 
                                        bool (*scanner)(word loc, void *data), 
                                        void *data )
{
  struct debug_scan_wrapper_data my_data;
  init(&my_data, scanner, data);
  urs_enumerate_minor_complement( DATA(urs)->backing_urs, incl_tag, gset, 
                                  debug_scan_wrapper, &my_data );
}
static void enumerate_complement( uremset_t *urs, 
                                  bool incl_tag, 
                                  gset_t gset, 
                                  bool (*scanner)(word loc, void *data), 
                                  void *data ) 
{
  struct debug_scan_wrapper_data my_data;
  init(&my_data, scanner, data);
  urs_enumerate_complement( DATA(urs)->backing_urs, incl_tag, gset, 
                            debug_scan_wrapper, &my_data );
}

static void          enumerate( uremset_t *urs, 
                                bool incl_tag, 
                                bool (*scanner)(word loc, void *data), 
                                void *data )
{
  struct debug_scan_wrapper_data my_data;
  init(&my_data, scanner, data);
  urs_enumerate( DATA(urs)->backing_urs, incl_tag, 
                 debug_scan_wrapper, &my_data );
}

static bool      is_remembered( uremset_t *urs, word w )
{
  return urs_isremembered( DATA(urs)->backing_urs, w );
}
static void              clear_minor( uremset_t *urs ) 
{
  urs_clear_minor( DATA(urs)->backing_urs );
}
static void copy_minor_to_major( uremset_t *urs ) 
{
  urs_copy_minor_to_major( DATA(urs)->backing_urs );
}

static void       init_summary( uremset_t *urs, 
                                int gno, 
                                int max_words_per_step, 
                                /* out parameter */ summary_t *s )
{
  assert(0);
}

static void checkpoint_stats( uremset_t *urs, int gno )
{
  urs_checkpoint_stats( DATA(urs)->backing_urs, gno );
}

uremset_t *alloc_uremset_debug( uremset_t *backing_urs )
{
  uremset_debug_data_t *data;

  data = (uremset_debug_data_t*)must_malloc( sizeof( uremset_debug_data_t ) );

  data->backing_urs = backing_urs;

  return create_uremset_t( backing_urs->collector, 
                           "debug wrapper",
                           (void*)data, 
                           expand_remset_gnos,
                           clear,
                           assimilate_and_clear, 
                           add_elem_new,
                           add_elem,
                           add_elems, 
                           enumerate_gno,
                           enumerate_allbutgno, 
                           enumerate_older, 
                           clear_minor, 
                           copy_minor_to_major, 
                           enumerate_minor_complement, 
                           enumerate_complement, 
                           enumerate,
                           is_remembered,
                           live_count, 
                           init_summary,
                           checkpoint_stats );
}
