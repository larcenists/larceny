/* Copyright 2007 Felix S Klock II
 *
 * $Id: gc_t.h 5226 2007-12-17 14:51:13Z pnkfelix $
 *
 * Larceny run-time system -- sets of generations 
 * (an inadequate representation, in the technical sense of the term)
 */

#ifndef INCLUDED_GSET_T_H
#define INCLUDED_GSET_T_H

typedef enum { gs_nil, gs_singleton, gs_range } gset_tag_t;
typedef struct { gset_tag_t tag; int g1; int g2; } gset_t;
/* interpretation of gset_t:
 * [[ <gs_nil, g1, g2> ]]       = {    }
 * [[ <gs_singleton, g1, g2> ]] = { g1 }
 * [[ <gs_range, g1, g2> ]] = { i | g1 <= i < g2 }
 */
static gset_t gset_singleton( int g1 ) { 
  gset_t g; g.tag = gs_singleton; g.g1 = g1; return g; 
}
static gset_t gset_range( int g1, int g2 ) {
  gset_t g; g.tag = gs_range; g.g1 = g1; g.g2 = g2; return g; 
}
static gset_t gset_younger_than( int g2 ) { return gset_range( 0, g2 ); }
static bool gset_singleton_memberp( int gno, gset_t gs ) {
  assert2( gs.tag == gs_singleton ); return ( gs.g1 == gno ); 
}
static bool gset_range_memberp( int gno, gset_t gs ) {
  assert2( gs.tag == gs_range ); return ( gs.g1 <= gno && gno < gs.g2 ); 
}
static bool gset_memberp( int gno, gset_t gs ) {
  switch (gs.tag) {
  case gs_nil: return FALSE;
  case gs_singleton: return gset_singleton_memberp( gno, gs );
  case gs_range:     return gset_range_memberp( gno, gs );
  }
  assert2(0);
}
static bool gset_emptyp( gset_t gs ) {
  switch (gs.tag) {
  case gs_nil: return TRUE;
  case gs_singleton: return FALSE;
  case gs_range: return (gs.g2 > gs.g1);
  }
}
static int gset_count( gset_t gs ) {
  switch (gs.tag) {
  case gs_nil: return 0;
  case gs_singleton: return 1;
  case gs_range: return max(0,(gs.g2 - gs.g1));
  }
}
static bool gset_max_elem( gset_t gs ) {
  switch (gs.tag) {
  case gs_singleton: return gs.g1;
  case gs_range:     return gs.g2-1;
  }
  assert2(0);
}
static int gset_singleton_min_elem_greater_than( gset_t gs, int gno ) {
  if (gs.g1 > gno) return gs.g1; else return 0;
}
static int gset_range_min_elem_greater_than( gset_t gs, int gno ) {
  if (gno+1 <= gs.g1) return gs.g1;
  else if (gno+1 < gs.g2) return gno+1;
  else return 0;
}
static int gset_min_elem_greater_than( gset_t gs, int gno ) {
  /* returns 0 if no elem of gs is larger than gno */
  switch (gs.tag) {
  case gs_nil: return 0;
  case gs_singleton: return (gs.g1 > gno) ? gs.g1 : 0;
  case gs_range: return gset_range_min_elem_greater_than( gs, gno );
  }
}
static gset_t gset_remove( int gno, gset_t gs ) {
  gset_t rtn;
  switch (gs.tag) {
  case gs_nil: assert(0);
  case gs_singleton: rtn.tag = gs_nil; break;
  case gs_range: assert(gno == gs.g1); 
    if (gs.g1+1 == gs.g2) {
      rtn.tag = gs_nil;
    } else { assert2( gs.g1+1 < gs.g2 );
      rtn.tag = gs_range; rtn.g1 = gs.g1+1; rtn.g2 = gs.g2;
    }
  }
  return rtn;
}

#endif    /* INCLUDED_GSET_T_H */

/* eof */
