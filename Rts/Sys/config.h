/* This is a modification of config.h from the Boehm-Weiser collector. */
/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#if !defined(INCLUDED_CONFIG_H)
#define INCLUDED_CONFIG_H

# if defined(sun) &&(defined(sparc) || defined(__sparc))
#  define SPARC
     /* Test for SunOS 5.x */
#    include <errno.h>
#    ifdef ECHRNG
#      define SUNOS5
#    else
#      define SUNOS4
#    endif
# endif

#endif
