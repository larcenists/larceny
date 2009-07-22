/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Refactoring previous array of hashtable (ie remset[] and major_remset[]) 
 * representation into implementation of uremset_t.
 *
 */

#include "larceny.h"
#include "remset_t.h"
#include "summary_t.h"
#include "uremset_t.h"

typedef struct uremset_array_data uremset_array_data_t;

struct uremset_array_data {
  remset_t **remset;
  remset_t **major_remset;
};

#define DATA(urs) ((uremset_array_data_t*)(urs->data))

static void expand_remset_gnos( uremset_t *urs, int fresh_gno )
{
}
static void              clear( uremset_t *urs, int gno )
{
}
static bool       add_elem_new( uremset_t *urs, word w )
{
}
static bool           add_elem( uremset_t *urs, word w )
{
}
static bool          add_elems( uremset_t *urs, word *bot, word *top )
{
}
static void          enumerate( uremset_t *urs, 
                                int gno, 
                                bool (*scanner)(word loc, 
                                                void *data), 
                                void *data )
{
}
static bool      is_remembered( uremset_t *urs, word w )
{
}
static void       init_summary( uremset_t *urs, 
                                int gno, 
                                int max_words_per_step, 
                                /* out parameter */ summary_t *s )
{
}

uremset_t *alloc_uremset_array( gc_t *gc )
{
  uremset_array_data_t *data;
  uremset_t *urs;
  data = (uremset_array_data_t*)must_malloc( sizeof( uremset_array_data_t ) );
  return create_uremset_t( "word hashset arrays",
                           (void*)data, 
                           expand_remset_gnos,
                           clear,
                           add_elem_new,
                           add_elem,
                           0, 
                           enumerate,
                           is_remembered,
                           init_summary );
}
