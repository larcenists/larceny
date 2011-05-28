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
#include "gset_t.h"
#include "gc_t.h"

struct uremset {
  char *id;
    /* A human-readable string identifying the remset representation. 
     */

  gc_t *collector;

  void *data;

    /* METHODS BELOW */

  void   (*expand_remset_gnos)( uremset_t *urs, int fresh_gno );

  void                (*clear)( uremset_t *urs, int gno );
    /* Clear portion of remset associated with gno */

  bool         (*add_elem_new)( uremset_t *urs, word w );
  bool             (*add_elem)( uremset_t *urs, word w );
  bool            (*add_elems)( uremset_t *urs, word *bot, word *top );

  void        (*enumerate_gno)( uremset_t *urs, bool incl_tag, int gno, 
                                bool (*scanner)(word loc, void *data), 
                                void *data );
    /* Enumerates all objects in generation (region) gno 
     * that (may) have region-crossing references.
     *
     * If incl_tag is set, then loc will be a tagged word (otherwise
     * its tag may be stripped).
     */

  void  (*enumerate_allbutgno)( uremset_t *urs, bool incl_tag, int gno, 
                                bool (*scanner)(word loc, void *data), 
                                void *data );
    /* Enumerates all objects in generations (regions) *other than* gno
     * that (may) have region-crossing references.
     */

  void      (*enumerate_older)( uremset_t *urs, bool incl_tag, int gno, 
                                bool (*scanner)(word loc, void *data), 
                                void *data );
    /* Enumerates all objects in generations >= gno 
     * that (may) have region-crossing references.
     * 
     * (Note that gno *is* included in the enumeration.)
     */

  void            (*enumerate)( uremset_t *urs, bool incl_tag, 
                                bool (*scanner)(word loc, void *data), 
                                void *data );
    /* Enumerates all objects that (may) have region-crossing references.
     */

  bool        (*is_remembered)( uremset_t *urs, word w );
    /* True if w is in the remembered set. */

  int            (*live_count)( uremset_t *urs, int gno );
    /* Count of entries associated with gno */

  void         (*init_summary)( uremset_t *urs, int gno, 
                                int max_words_per_step, 
                                /* out parameter */ summary_t *s );
    /* Inializes summary_t that enumerates objects in generation (region) gno.
     */

  /* XXX deprecated methods follow. */

  void (*clear_minor)( uremset_t *urs );
    /* Clears all objects in the minor part.
     * (That is, the objects added via add_elems method but not
     *  add_elem_new or add_elem.)
     */
  void (*copy_minor_to_major)( uremset_t *urs );
    /* Copies whatever remains in the minor part to the major part. */
  void (*enumerate_minor_complement)( uremset_t *urs, bool incl_tag, 
                                      gset_t genset, 
                                      bool (*scanner)(word loc, void *data), 
                                      void *data );
    /* Enumerates all objects in generations (regions) that both 
     *  1.) are *not* in genset, and 
     *  2.) (may) have references into the nursery.
     * 
     * (Note that objects with region-crossing references that do not
     *  point into the nursery are not necessarily included in the 
     *  enumeration.)
     */

  void (*enumerate_complement)( uremset_t *urs, bool incl_tag, 
                                gset_t genset, 
                                bool (*scanner)(word loc, 
                                                void *data), 
                                void *data );
    /* Enumerates all objects in generations (regions) *not* in genset
     * that (may) have region-crossing references.
     *
     * XXX this is deprecated; try to remove uses of it.
     */

  void (*assimilate_and_clear)( uremset_t *urs, int g1, int g2 );
    /* Merge state of remset[g2] into remset[g1] and then clear remset[g2]. 
     *
     * XXX Deprecated (compatibility method for np collector).
     */

  void     (*checkpoint_stats)( uremset_t *urs, int gno );
    /* Add current counters of remset[gno] to the global accumulators.
     *
     * XXX Deprecated (compatibility method for np collector).
     */
};

#define urs_expand_remset_gnos( r,g )  ((r)->expand_remset_gnos( (r),(g) ))
#define urs_clear( r,g )               ((r)->clear( (r),(g) ))
#define urs_clear_minor( r )           ((r)->clear_minor( (r) ))
#define urs_copy_minor_to_major( r )   ((r)->copy_minor_to_major( (r) ))
#define urs_assimilate_and_clear( r,g1,g2 ) \
  ((r)->assimilate_and_clear( (r),(g1),(g2) ))
#define urs_add_elem_new( r,w )        ((r)->add_elem_new( (r),(w) ))
#define urs_add_elem( r,w )            ((r)->add_elem( (r),(w) ))
#define urs_add_elems( r,b,t )         ((r)->add_elems( (r),(b),(t) ))
#define urs_enumerate_gno( r,i,g,s,d ) \
  ((r)->enumerate_gno( (r),(i),(g),(s),(d) ))
#define urs_enumerate_allbutgno( r,i,g,s,d )    \
  ((r)->enumerate_allbutgno( (r),(i),(g),(s),(d) ))
#define urs_enumerate_complement( r,i,g,s,d )           \
  ((r)->enumerate_complement( (r),(i),(g),(s),(d) ))
#define urs_enumerate_minor_complement( r,i,g,s,d )     \
  ((r)->enumerate_minor_complement( (r),(i),(g),(s),(d) ))
#define urs_enumerate( r,i,s,d )         ((r)->enumerate( (r),(i),(s),(d) ))
#define urs_isremembered( r,w )        ((r)->is_remembered( (r),(w) ))
#define urs_init_summary( r,g,m,s )    ((r)->init_summary( (r),(g),(m),(s) ))
#define urs_checkpoint_stats( r,g )    ((r)->checkpoint_stats( (r),(g) ))
#define urs_live_count( r,g )          ((r)->live_count( (r),(g) ))

uremset_t
*create_uremset_t(gc_t *collector,
                  char *id,
                  void *data, 
                  void         (*expand_remset_gnos)( uremset_t *urs, 
                                                      int fresh_gno ), 
                  void                      (*clear)( uremset_t *urs, int gno ), 
                  void       (*assimilate_and_clear)( uremset_t *urs, 
                                                      int g1, int g2 ), 
                  bool               (*add_elem_new)( uremset_t *urs, word w ), 
                  bool                   (*add_elem)( uremset_t *urs, word w ), 
                  bool                  (*add_elems)( uremset_t *urs, 
                                                      word *bot, 
                                                      word *top ), 
                  void              (*enumerate_gno)( uremset_t *urs, 
                                                      bool incl_tag, 
                                                      int gno, 
                                                      bool (*scanner)(word loc, 
                                                                      void *data), 
                                                      void *data ), 
                  void        (*enumerate_allbutgno)( uremset_t *urs, 
                                                      bool incl_tag, 
                                                      int gno, 
                                                      bool (*scanner)(word loc, 
                                                                      void *data), 
                                                      void *data ), 
                  void            (*enumerate_older)( uremset_t *urs, 
                                                      bool incl_tag, 
                                                      int gno, 
                                                      bool (*scanner)(word loc, 
                                                                      void *data), 
                                                      void *data ), 
                  void                (*clear_minor)( uremset_t *urs ),
                  void        (*copy_minor_to_major)( uremset_t *urs ),
                  void (*enumerate_minor_complement)( uremset_t *urs, 
                                                      bool incl_tag, 
                                                      gset_t genset, 
                                                      bool (*scanner)(word loc, 
                                                                      void *data), 
                                                      void *data ), 
                  void       (*enumerate_complement)( uremset_t *urs, 
                                                      bool incl_tag, 
                                                      gset_t genset, 
                                                      bool (*scanner)(word loc, 
                                                                      void *data), 
                                                      void *data ), 
                  void                  (*enumerate)( uremset_t *urs, 
                                                      bool incl_tag, 
                                                      bool (*scanner)(word loc, 
                                                                      void *data), 
                                                      void *data ), 
                  bool              (*is_remembered)( uremset_t *urs, word w ), 
                  int                  (*live_count)( uremset_t *urs, int gno ),
                  void               (*init_summary)( uremset_t *urs, 
                                                      int gno, 
                                                      int max_words_per_step, 
                                                      summary_t *s_outparam ),
                  void           (*checkpoint_stats)( uremset_t *urs, int gno )
                  );
#endif /* INCLUDED_UREMSET_T_H */

/* eof */
