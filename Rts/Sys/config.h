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

/* This trick is brittle and depends on the contents of system-supplied
   header files.  In particular, it does not work with LCC, because LCC 
   cannot be relied on to parse system header files (in particular,
   SunOS 5.6 /usr/include/stdio.h is not standard-conforming in any real 
   sense); thus, using only system header files rather than LCC header
   files is not a useful alternative.
   */

#if !defined(STDC_SOURCE)
#  if !defined(INCLUDED_CONFIG_H)
#    define INCLUDED_CONFIG_H
#    if defined(sun) &&(defined(sparc) || defined(__sparc))
#      define SPARC
       /* Test for SunOS 5.x */
#      include <errno.h>
#      ifdef ECHRNG
#        define SUNOS5
#      else
#        define SUNOS4
#      endif
#    endif
#  endif
#endif

