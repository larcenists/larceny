/* Copyright 2008 Felix S Klock II        -*- indent-tabs-mode: nil -*-
 *
 * $Id: smircy.h 5790 2008-09-08 17:25:21Z pnkfelix $
 *
 * Interface for incremental marking machine.
 */

#include "larceny-types.h"

typedef struct smircy_context smircy_context_t;

smircy_context_t *smircy_begin( gc_t *gc );

void smircy_push_roots( smircy_context_t *context );

void smircy_push_remset( smircy_context_t *context, remset_t *rs );

void smircy_progress( smircy_context_t *context, 
                      int mark_max, int trace_max, int mark_words_max,
                      int *marked, int *traced, int *words_marked );

bool smircy_object_marked_p( smircy_context_t *context, word obj );

void smircy_end( smircy_context_t *context );

void smircy_set_object_visitor( smircy_context_t *context, 
                                void* (*visitor)( word obj, 
                                                  word src, 
                                                  void *data ),
                                void *visit_data );

void* smircy_get_object_visitor_data( smircy_context_t *context );

/* eof */
