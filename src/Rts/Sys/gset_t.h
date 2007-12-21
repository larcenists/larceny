/* Copyright 2007 Felix S Klock II
 *
 * $Id: gc_t.h 5226 2007-12-17 14:51:13Z pnkfelix $
 *
 * Larceny run-time system -- sets of generations 
 * (an inadequate representation, in the technical sense of the term)
 */

#ifndef INCLUDED_GSET_T_H
#define INCLUDED_GSET_T_H

typedef enum { gs_singleton, gs_range } gset_tag_t;
typedef struct { gset_tag_t tag; int g1; int g2; } gset_t;
/* interpretation of gset_t:
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
  case gs_singleton: return gset_singleton_memberp( gno, gs );
  case gs_range:     return gset_range_memberp( gno, gs );
  }
  assert2(0);
}
static bool gset_max_elem( gset_t gs ) {
  switch (gs.tag) {
  case gs_singleton: return gs.g1;
  case gs_range:     return gs.g2-1;
  }
  assert2(0);
}

#endif    /* INCLUDED_GSET_T_H */

/* eof */
/* Copyright 2007 Felix S Klock II
 *
 * $Id: gc_t.h 5226 2007-12-17 14:51:13Z pnkfelix $
 *
 * Larceny run-time system -- sets of generations 
 * (an inadequate representation, in the technical sense of the term)
 */

#ifndef INCLUDED_GSET_T_H
#define INCLUDED_GSET_T_H

typedef enum { gs_singleton, gs_range } gset_tag_t;
typedef struct { gset_tag_t tag; int g1; int g2; } gset_t;
/* interpretation of gset_t:
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
  case gs_singleton: return gset_singleton_memberp( gno, gs );
  case gs_range:     return gset_range_memberp( gno, gs );
  }
  assert2(0);
}
static bool gset_max_elem( gset_t gs ) {
  switch (gs.tag) {
  case gs_singleton: return gs.g1;
  case gs_range:     return gs.g2-1;
  }
  assert2(0);
}

#endif    /* INCLUDED_GSET_T_H */

/* eof */
