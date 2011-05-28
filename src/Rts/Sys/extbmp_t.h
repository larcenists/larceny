/* Copyright 2009 Felix S Klock II.              -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Interface for a bitmap of untagged heap addresses.
 * 
 */

extbmp_t *create_extensible_bitmap( gc_t *gc, gc_param_t *info );
  /* Creates bitmap representing empty set of addresses */

bool extbmp_add_elem( extbmp_t *ebmp, word untagged_w );
  /* ebmp := ebmp U { untagged_w }.
   * Returns TRUE iff untagged_w was already in ebmp. */

void extbmp_del_elem( extbmp_t *ebmp, word untagged_w );
  /* ebmp := ebmp \ { untagged_w }. 
   * Does not require untagged_w was originally in ebmp. */

bool extbmp_is_member( extbmp_t *ebmp, word untagged_w );
  /* untagged_w in ebmp ? */

void extbmp_clear_members_in( extbmp_t *ebmp, int gno );
  /* ebmp := ebmp \ addresses(gno) */

int  extbmp_count_members_in( extbmp_t *ebmp, int gno );
  /* cardinality(ebmp & addresses(gno)) */

void extbmp_expand_remset_gnos( extbmp_t *ebmp, int fresh_gno );

void extbmp_enumerate( extbmp_t *ebmp,
                       bool incl_tag, 
                       bool (*scanner)(word loc, void *data), 
                       void *data );
  /* for each w in ebmp 
   *   invoke scanner( w, data )
   *   if scanner returns FALSE, ebmp := ebmp \ { w }
   *
   * If incl_tag TRUE, then w arg to scanner will be tagged; 
   * o/w tag might be stripped
   */

void extbmp_enumerate_in( extbmp_t *ebmp, 
                          bool incl_tag, 
                          int gno, 
                          bool (*scanner)(word loc, void *data), 
                          void *data );
  /* for each w in (ebmp & addresses(gno))
   *   invoke scanner( w, data )
   *   if scanner returns FALSE, ebmp := ebmp \ { w }
   * 
   * If incl_tag TRUE, then w arg to scanner will be tagged; 
   * o/w tag might be stripped
   */

