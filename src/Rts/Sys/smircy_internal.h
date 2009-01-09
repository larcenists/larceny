/* Copyright 2008 Felix S Klock II        -*- indent-tabs-mode: nil -*-
 *
 * $Id: smircy_internal.h 5790 2008-09-08 17:25:21Z pnkfelix $
 *
 * Shared header for internals of incremental marking machine.
 */

#ifndef INCLUDED_SMIRCY_INTERNAL_H
#define INCLUDED_SMIRCY_INTERnAL_H

#if defined(BITS_32)
# define BIT_IDX_SHIFT       3  /* shift to get doubleword bit address */
# define BIT_IDX_TO_WORD_IDX 5  /* shift to get word addr from bit addr */
# define BIT_IN_WORD_MASK   31  /* mask to get bit shift */
# define BITS_IN_WORD       32
#else
# error "Must define SMIRCY macros for non-32 bit systems."
#endif

#define OBJ_STACK_SIZE 2048
/* Should be large and chosen so that obj_stackseg occupies integral
 * number of pages. */

#define LOS_STACK_SIZE 1365
/* Should be large and chosen so that los_stackseg occupies integral
 * number of pages.  (Since a los cursor is > 1 word, I may need to
 * add explicit padding to los_stack to get that effect...) */

#define WINDOW_SIZE_LIMIT 1024
/* The max number of entries that we'll extract from a large object
 * and push onto the mark stack before processing the stack. */

typedef struct obj_stack_entry     obj_stack_entry_t;
typedef struct smircy_stack        smircy_stack_t;
typedef struct obj_stackseg        obj_stackseg_t;
typedef struct los_stackseg        los_stackseg_t;
typedef struct obj_stack           obj_stack_t;
typedef struct los_stack           los_stack_t;
typedef struct large_object_cursor large_object_cursor_t;

struct obj_stack_entry { 
  word              val;
  int               gno;
  obj_stack_entry_t *next_in_rgn;
};

struct obj_stack {
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  obj_stack_entry_t *stklim;
};

struct obj_stackseg {
  obj_stackseg_t *next;
  obj_stack_entry_t data[OBJ_STACK_SIZE];
};

struct los_stack {
  los_stackseg_t      *seg;
  large_object_cursor_t *stkp;
  large_object_cursor_t *stkbot;
  large_object_cursor_t *stklim;
};

struct large_object_cursor {
  word       object;
  int        index; /* Resumption point for scan (from start of object) */
  large_object_cursor_t    *next_in_rgn;
};

struct los_stackseg {
  struct large_object_cursor data[LOS_STACK_SIZE];
  los_stackseg_t            *next;
};

struct smircy_stack {
  obj_stack_t    obj;         /* Object stack */
  los_stack_t    los;         /* LOS cursor stack */
};

struct smircy_context {
  gc_t               *gc;
  int                num_rgns;
  word               *lowest_heap_address;
  word               *highest_heap_address;
  int                words_in_bitmap;
  word               *bitmap;
  smircy_stack_t     stack;
  obj_stack_entry_t  **rgn_to_obj_entry;
  large_object_cursor_t **rgn_to_los_entry;
  obj_stackseg_t     *freed_obj;  /* obj segments available for reuse */
  los_stackseg_t     *freed_los;  /* los segments available for reuse */
  int                total_traced;
  int                total_marked;
  int                total_words_marked;
};

#endif /* INCLUDED_SMIRCY_H */
