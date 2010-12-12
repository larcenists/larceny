/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 */

#include "larceny.h"
#include "gc_t.h"
#include "uremset_t.h"

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
                  )
{
  uremset_t *urs;
  urs = (uremset_t*)must_malloc( sizeof( uremset_t ));

  urs->collector = collector;
  urs->id = id;
  urs->data = data;

  urs->expand_remset_gnos         = expand_remset_gnos;
  urs->clear                      = clear;
  urs->assimilate_and_clear       = assimilate_and_clear;

  urs->add_elem_new               = add_elem_new;
  urs->add_elem                   = add_elem;
  urs->add_elems                  = add_elems;

  urs->enumerate_gno              = enumerate_gno;
  urs->enumerate_allbutgno        = enumerate_allbutgno;
  urs->enumerate_older            = enumerate_older;
  urs->clear_minor                = clear_minor;
  urs->copy_minor_to_major        = copy_minor_to_major;
  urs->enumerate_minor_complement = enumerate_minor_complement;
  urs->enumerate_complement       = enumerate_complement;
  urs->enumerate                  = enumerate;
  urs->is_remembered              = is_remembered;
  urs->live_count                 = live_count;
  urs->init_summary               = init_summary;
  urs->checkpoint_stats           = checkpoint_stats;

  return urs;
}
