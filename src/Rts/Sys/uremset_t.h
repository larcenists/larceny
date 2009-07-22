/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Unified Remembered-set interface.
 *
 * A remembered set is a set of objects.  This interface does not
 * assume a direct mapping from generations (aka regions) to remsets;
 * instead the abstraction is that there is a single remembered set
 * for the whole heap.  (The internal implementation is of course free
 * to maintain a mapping from generations to an internal set
 * structure.)
 * 
 * The standard invariant is: If object A has a region-crossing
 * reference to object B, then A is in the rembered set.
 */

#ifndef INCLUDED_UREMSET_T_H
#define INCLUDED_UREMSET_T_H

#include "larceny-types.h"

struct uremset {
  char *id;
    /* A human-readable string identifying the remset representation. 
     */

  gc_t *collector;

  void *data;

  void (*expand_remset_gnos)( uremset_t *urs, int fresh_gno );
  void              (*clear)( uremset_t *urs, int gno );
  bool       (*add_elem_new)( uremset_t *urs, word w );
  bool           (*add_elem)( uremset_t *urs, word w );
  bool          (*add_elems)( uremset_t *urs, word *bot, word *top );
  void          (*enumerate)( uremset_t *urs, int gno, 
                              bool (*scanner)(word loc, void *data), 
                              void *data );
  bool      (*is_remembered)( uremset_t *urs, word w );
  void       (*init_summary)( uremset_t *urs, int gno, int max_words_per_step, 
                              /* out parameter */ summary_t *s );
};

#define urs_expand_remset_gnos( r, g ) ((r)->expand_remset_gnos( (r),(g) ))
#define urs_clear( r, g )              ((r)->clear( (r),(g) ))
#define urs_add_elem_new( r, w )       ((r)->add_elem_new( (r),(w) ))
#define urs_add_elem( r, w )           ((r)->add_elem( (r),(w) ))
#define urs_add_elems( r, b, t )       ((r)->add_elems( (r),(b),(t) ))
#define urs_enumerate( r, g, s, d )    ((r)->enumerate( (r),(g),(s),(d) ))
#define urs_is_remembered( r, w )      ((r)->is_remembered( (r),(w) ))
#define urs_init_summary( r, g, m, s ) ((r)->init_summary( (r),(g),(m),(s) ))

uremset_t
*create_uremset_t(char *id,
                  void *data, 
                  void (*expand_remset_gnos)( uremset_t *urs, int fresh_gno ), 
                  void            (*clear)( uremset_t *urs, int gno ), 
                  bool     (*add_elem_new)( uremset_t *urs, word w ), 
                  bool         (*add_elem)( uremset_t *urs, word w ), 
                  bool        (*add_elems)( uremset_t *urs, 
                                            word *bot, 
                                            word *top ),
                  void        (*enumerate)( uremset_t *urs, 
                                            int gno, 
                                            bool (*scanner)(word loc, 
                                                            void *data), 
                                            void *data ),
                  bool    (*is_remembered)( uremset_t *urs, word w ),
                  void     (*init_summary)( uremset_t *urs, 
                                            int gno, 
                                            int max_words_per_step, 
                                            /* out parameter */ summary_t *s )
                  );

#endif /* INCLUDED_UREMSET_T_H */

/* eof */
