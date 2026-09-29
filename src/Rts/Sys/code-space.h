/* Copyright 2026.                        -*- indent-tabs-mode: nil -*-
 *
 * Larceny -- the code space.
 *
 * On CODE_SPACE systems, compiled code (code vectors, which are
 * bytevectors) does not live in the data heap but in a separate space
 * of executable memory.  On systems that forbid memory that is both
 * writable and executable, the code space is writable only while the
 * run-time system writes to it (see osdep_code_writing()).
 *
 * The code space is a semispace that belongs to the oldest non-static
 * generation (the dynamic area of the generational collector, or the
 * only area of the stop-and-copy collector): its pages carry that
 * generation number and the MB_CODE attribute.  Code objects contain
 * no pointers, so the code space never needs to be scanned, and
 * pointers into it from younger generations need not be remembered.
 *
 * When that generation is collected, every code object that is
 * reached is copied into a new code semispace; at the end of the
 * collection the old code semispace is freed.  The collector calls
 * cs_forward() for any object on a page with the MB_CODE attribute.
 *
 * The regional collector has no oldest generation that it collects,
 * so there the code space belongs to the static generation (which is
 * never collected, and pointers into which are not remembered), and
 * code is collected by a separate pass over all the heap memory that
 * forwards code objects and updates every pointer to them (see
 * collect_code_by_scanning() in memmgr.c).
 *
 * New code is installed by copying a code vector from the data heap
 * into the code space (cs_install()).  Heap images put code vectors in
 * their text segment, which is loaded into the code space.
 */

#ifndef INCLUDED_CODE_SPACE_H
#define INCLUDED_CODE_SPACE_H

#include "larceny-types.h"

#if CODE_SPACE

struct code_space {
  semispace_t *current;         /* Code in use */
  semispace_t *tospace;         /* Code copied by the current GC, or NULL */
  int         gen_no;           /* The generation that owns the code */
  int         writing;          /* Code memory writable? */
  word        *load_bot;        /* Range being loaded, or NULL */
  word        *load_top;
  int         installed;        /* Bytes installed since code was collected */
  int         live;             /* Bytes live after code was collected */
  int         by_scanning;      /* Collected by a separate scanning pass? */
  int         scan_requested;   /* Scanning pass requested after next GC? */
};

code_space_t *create_code_space( int gen_no, int by_scanning );
  /* Create a code space owned by the given generation.  If by_scanning
     is nonzero, the generation is never collected and code is
     collected by a separate scanning pass.
     */

void cs_set_gen_no( code_space_t *cs, int gen_no );
  /* The owning generation has been renumbered.
     */

word *cs_load_area( code_space_t *cs, int nbytes );
  /* Return a contiguous, writable area of nbytes bytes in the code
     space, to be filled with code objects by the heap loader.  The
     caller must call cs_load_complete() when the area has been filled.
     */

void cs_load_complete( code_space_t *cs );
  /* Make the code loaded into the area returned by cs_load_area()
     executable.
     */

word cs_install( code_space_t *cs, word bv );
  /* Copy the bytevector bv into the code space and return a tagged
     pointer to the copy, which is ready to execute.
     */

int cs_wants_collection( code_space_t *cs );
  /* Returns nonzero if so much code has been installed since the code
     space was last collected that its generation should be collected
     before more code is installed.
     */

void cs_collected( code_space_t *cs );
  /* Called after an explicit collection of the code space's generation.
     */

word cs_update_pointer( code_space_t *cs, word w );
  /* For the scanning pass: if w points to a code object that has not
     yet been copied, copy it; return the (possibly new) value of w.
     */

word cs_forward( code_space_t *cs, word obj );
  /* Called by the collector: copy the code object obj, which must not
     already have been forwarded, into the code space's to-space, leave
     a forwarding pointer in the original, and return the copy.
     */

void cs_after_collection( code_space_t *cs );
  /* Called after every collection: if code was copied, make the copies
     the current code space and free the old one.
     */

semispace_t *cs_semispace( code_space_t *cs );
  /* The semispace holding the code, for heap dumping.
     */

#endif /* CODE_SPACE */

#endif /* INCLUDED_CODE_SPACE_H */

/* eof */
