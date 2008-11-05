/* Copyright 2008 Felix S Klock II        -*- indent-tabs-mode: nil -*-
 *
 * $Id: smircy.h 5790 2008-09-08 17:25:21Z pnkfelix $
 *
 * Interface for incremental marking machine.
 */

#ifndef INCLUDED_SMIRCY_H
#define INCLUDED_SMIRCY_H

#include "larceny-types.h"

typedef struct smircy_context smircy_context_t;

smircy_context_t *smircy_begin( gc_t *gc, int num_rgns );

void smircy_push_roots( smircy_context_t *context );

void smircy_push_remset( smircy_context_t *context, remset_t *rs );

void smircy_push_elems( smircy_context_t *context, word *bot, word *top );

void smircy_progress( smircy_context_t *context, 
                      int mark_max, int trace_max, int mark_words_max,
                      int *marked, int *traced, int *words_marked );

bool smircy_stack_empty_p( smircy_context_t *context );

bool smircy_object_marked_p( smircy_context_t *context, word obj );

void smircy_minor_gc( smircy_context_t *context );
void smircy_major_gc( smircy_context_t *context, int rgn );

void smircy_when_object_forwarded( smircy_context_t *context, 
                                   word obj_orig, word obj_new );

void smircy_end( smircy_context_t *context );

void smircy_set_object_visitor( smircy_context_t *context, 
                                void* (*visitor)( word obj, 
                                                  word src, 
                                                  void *data ),
                                void *visit_data );

void* smircy_get_object_visitor_data( smircy_context_t *context );

void smircy_expand_gnos( smircy_context_t *context, int gno );
int smircy_objs_marked( smircy_context_t *context );
int smircy_arcs_traced( smircy_context_t *context );
int smircy_words_marked( smircy_context_t *context );

#endif /* INCLUDED_SMIRCY_H */

/* eof */
