/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 */

#include "larceny.h"
#include "uremset_t.h"

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
                  )
{
  uremset_t *urs;
  urs = (uremset_t*)must_malloc( sizeof( uremset_t ));

  urs->id = id;
  urs->data = data;

  urs->expand_remset_gnos = expand_remset_gnos;
  urs->clear              = clear;
  urs->add_elem_new       = add_elem_new;
  urs->add_elem           = add_elem;
  urs->add_elems          = add_elems;
  urs->enumerate          = enumerate;
  urs->is_remembered      = is_remembered;
  urs->init_summary       = init_summary;
}
