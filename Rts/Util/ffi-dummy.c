/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Stand-in procedures for the dynamic linking library functions.
 */

int dlopen(void);
int dlsym(void);
int dlerror(void);

int dlopen(void)
{
  abort();
}

int dlsym(void)
{
  abort();
}

int dlerror(void)
{
  abort();
}
