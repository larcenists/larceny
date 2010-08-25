/* Copyright 1998 Lars T Hansen.
 *
 * $Id: remset_t.h 5128 2007-11-13 20:13:45Z pnkfelix $
 *
 * Remembered-set interface.
 *
 * Remembered sets remember objects.  The basic assumption in the current
 * implementation is that there will be one set for each generation, and 
 * it will remember objects in the generation that may contain pointers 
 * to generations that will be collected before the generation with which
 * the set is collected (a "points-out" set).
 */

#ifndef INCLUDED_REMSET_NP_T_H
#define INCLUDED_REMSET_NP_T_H

#include "remset_t.h"

void rs_assimilate_and_clear( remset_t *r1, remset_t *r2 );  /* r1 += r2 */

#endif
