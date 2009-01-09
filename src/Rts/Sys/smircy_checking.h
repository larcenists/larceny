/* Copyright 2008 Felix S Klock II        -*- indent-tabs-mode: nil -*-
 *
 * $Id: smircy.h 5790 2008-09-08 17:25:21Z pnkfelix $
 *
 * Interface for incremental marking machine.
 */

#ifndef INCLUDED_SMIRCY_CHECKING_H
#define INCLUDED_SMIRCY_CHECKING_H

#include "msgc-core.h"

msgc_context_t *smircy_clone_begin( smircy_context_t *context_old, bool loud );
void smircy_clone_end( msgc_context_t *c );

#endif
