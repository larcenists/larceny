/* Copyright 1999 Lars T Hansen
 *
 * $Id$
 *
 * Interface to operating-system dependent package. Unless where specified
 * otherwise, these functions may not block the process indefinitely.
 */

#if !defined(INCLUDED_OSDEP_H)
#define INCLUDED_OSDEP_H

typedef struct {
  unsigned sec;
  unsigned usec;
} stat_time_t;

extern void osdep_init( void );
  /* Initialize the operating systems package.  Must be called before any 
     other function in the package.
     */

extern unsigned osdep_realclock( void );
  /* Return the elapsed time in milliseconds since initialization.  
  
     FIXME: On 32-bit machines the result will overflow after 49.7 days;
     in this sense it is compatible with Windows 95 (sigh).  Should return
     a stat_time_t instead, or take one as a parameter; the UNSIGNED is a
     legacy thing.
     */

extern unsigned osdep_cpuclock( void );
  /* Return the cpu time in milliseconds since initialization.  
  
     FIXME: On 32-bit machines the result will overflow after 49.7 days;
     in this sense it is compatible with Windows 95 (sigh).  Should return
     a stat_time_t instead, or take one as a parameter; the UNSIGNED is a
     legacy thing.
     */

extern void osdep_time_used( stat_time_t *r, stat_time_t *u, stat_time_t *s );
  /* Fill in the structures with real (elapsed time), user (cpu) time, and
     system time, respectively.  Either of the pointers may be NULL.
     */

extern void osdep_pagefaults( unsigned *major, unsigned *minor );
  /* Fill in the integers with a count of major and minor page faults since 
     startup, if these numbers are available and meaningful for the platform.

     A 'major' fault is one that requires disk I/O; a 'minor' fault is one
     that does not.
     */
       
void osdep_poll_startup_events( void );
  /* Process any events that need to be processed at startup.  This function
     is only to be called before any Scheme procedures are run.
     */

extern void osdep_poll_events( word *globals );
  /* Poll for events that need to be polled for on this operating system, and
     dispatch on them if necessary.  This function is only to be called when
     the Scheme machine has taken a timer interrupt (and thus is not in a
     critical section).
     
     Examples include handling the event queue on MacOS and Windows and 
     handling asynchronous interrupts on Unix (SIGIO, SIGCHLD).
     */

extern void osdep_os_version( int *major, int *minor );
  /* Return the major and minor version numbers of the operating system.
     */

void *osdep_alloc_aligned( int bytes );
  /* Takes a request for a number of bytes, which must be a nonzero
     multiple of 4KB, and returns a pointer to a block of memory of the
     requested size aligned on a 4KB boundary.

     The allocator should attempt to cluster allocations in the address 
     space.  
     */

void osdep_free_aligned( void *block, int bytes );
  /* Takes a pointer returned from osdep_alloc_aligned() as well as
     the size of the pointed-to block and returns the block to
     the free memory pool.
     */

int osdep_fragmentation( void );
  /* Return the number of bytes of internal fragmentation in blocks
     managed the osdep allocator.  Internal fragmentation arises when
     the underlying operating system cannot be relied on to align
     blocks on a 4KB boundary and the osdep allocator must allocate larger
     blocks to ensure alignment.
     
     (As a rule of thumb, fragmentation is either 0 or 4KB per live block.)
     */
     

/* File system and I/O interface 
 *
 * General observations.
 *
 * The file system and I/O interface has a Unix heritage and may not port well
 * to all platforms.  For example, osdep_readfile() specifies poll + read 
 * semantics that may not be implementable everywhere: MacOS uses an async-read
 * + completion-handler model, and Plan 9 uses a threads + blocking I/O model. 
 * Those models don't fit into this interface at all unless a serious amount of
 * work is done on the C level.
 *
 * In some sense the most principled way of interfacing to the OS is to
 * do as little as possible on the C side -- essentially, only parameter 
 * translation -- and then write as much as possible in Scheme, to be able 
 * to control the blocking behavior properly.  That eschews portable interfaces
 * on the C level altogether.  There will be no common syscall for
 * "osdep_openfile()" -- there will instead be os-dependent syscalls to
 * stubs for OS interface code, eg. macos_FSpOpenFile.  In the general
 * case that involves operating on foreign data structures on the Scheme
 * level -- but system-specific compromises can be made.
 *
 * Another issue is that _readfile() and _writefile() on arbitrary descriptors
 * make sense on Unix but not necessarily on a system that distinguishes
 * between files and other kinds of channels (GUIs, networks).  Modula-3
 * made the distinction between 'blocked' and 'intermittent' channels, and
 * later, between 'seekable' and 'nonseekable' channels.  That, too, calls
 * for a possibly wider and certainly OS-dependent interface to the I/O
 * routines.
 */

extern void osdep_openfile( word fn, word flags, word mode );
  /* Open a file and store the resulting descriptor in globals[G_RESULT].
     'fn' is a Scheme string; 'flags' and 'mode' are fixnums.  
  
     FIXME: this function should have a less Unix-dependent interface.
     FIXME: this function should take globals[] as a parameter in any case.
     FIXME: there is no way to distinguish between errors.
     */
 
extern void osdep_unlinkfile( word fn );
  /* Remove the named file and store the result value in globals[G_RESULT].
     'fn' is a Scheme string.
     
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */

extern void osdep_closefile( word fd );
  /* Close the file associated with the descriptor 'fd' and store the result
     value in globals[G_RESULT].  'fd' is opaque.
  
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */

extern void osdep_readfile( word fd, word buf, word nbytes );
  /* Read at most 'nbytes' bytes from the file named by the descriptor 
     'fd' into the array 'buf'.  'nbytes' is a fixnum, 'buf' is a bytevector-
     like structure, and 'fd' is opaque.  The call may block the process if
     no input is available, but may not block the process waiting for 'nbytes'
     of input if less input is available.
     Place the number of bytes read as a fixnum in globals[G_RESULT], or
     0 on end-of-file and -1 on error.
     
     FIXME: this function should have a less Unix-dependent interface.
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */
     
extern void osdep_writefile( word fd, word buf, word nbytes, word offset );
  /* Write at most 'nbytes' bytes from 'buf' to the file described by 'fd',
     starting at byte 'offset' in 'buf'.  'nbytes' and 'offset' are fixnums,
     'buf' is a bytevector-like structure, and 'fd' is opaque.  The call may
     block the process if no bytes can be written, but may not block the
     process if less than 'nbytes' can be written.
     Place the number of bytes written as a fixnum in globals[G_RESULT],
     or -1 on error.
     
     FIXME: this function should have a less Unix-dependent interface.
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */

extern void osdep_mtime( word fn, word buf );
  /* 'fn' is a filename and 'buf' is a vector of 6 elements.  Obtain the
     modification time of the file and store it in the vector as fixnums:
     year, month [1..12], month-day [1..31], hour [0..23], minute [0..59],
     and second [0..61] (up to two leap seconds).  On error, place
     fixnum(-1) in globals[G_RESULT], otherwise fixnum(0).
     
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */
 
extern void osdep_access( word fn, word mode );
  /* 'fn' is a filename and 'mode' is an access mode (a fixnum).  Check
     whether the file can be accessed according to 'mode' and place the
     result in globals[G_RESULT]: fixnum(0) if yes, fixnum(-1) if no.
     
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to report errors.
     */
     
extern void osdep_rename( word oldname, word newname );
  /* 'oldname' and 'newname' are both file names.  Rename 'oldname' as
     'newname' and return the result code in globals[G_RESULT]: fixnum(0)
     if OK, fixnum(-1) on error.
     
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */
 
extern void osdep_pollinput( word fd );
  /* 'fd' is a file descriptor.  Return 0 in globals[G_RESULT] if input
     is not ready on the descriptor; 1 if at least one byte of input is
     ready.
     */

extern void osdep_system( word command );
  /* If COMMAND is #f, this function polls for the presence of a command
     processor.  If a command processor exists, return #t in globals[G_RESULT],
     otherwise return #f.
     
     If COMMAND is a string, then if the command processor exists pass
     the string to the command processor for execution and return the
     result code from the command processor in globals[G_RESULT].  If
     the command processor does not exist, return #f.
     
     FIXME: this function should take globals[] as a parameter.
     FIXME: there is no way to distinguish between errors.
     */

extern void osdep_chdir( word path );
  /* Change the process's working directory to PATH.

     Returns fixnum(0) on success, fixnum(-1) on failure or if chdir is
     not implemented on the platform.
     */

extern void osdep_cwd( void );
  /* Get the current working directory as a string.

     Returns #f on error or if unimplemented, otherwise the string.
     */

extern word osdep_dlopen( char *path );
  /* 'path' is an untagged pointer to a string.
      
     'Path' represents the name of a shared object in the system.
     osdep_dlopen() attempts to load that object, and if it is
     successful, returns a nonzero handle to it.  If the load failed,
     0 is returned.  (Some systems choose to kill the process if
     dlopen fails.  Sorry.)
      
     The string uses the operating system's native string
     representation -- `asciiz' on Unix or Win32, 'pascal' on MacOS.
      
     The mapping from the name of the shared object as specified by
     path and an actual shared object in the system is entirely
     OS-dependent.  UTSL.
     */

extern word osdep_dlsym( word handle, char *symbol );
  /* 'handle' is a non-zero value returned by osdep_dlopen(),
     representing a loaded shared object.
     'symbol' is an untagged pointer to a string.
     
     The string represents a symbol in the shared object's symbol
     table.  osdep_dlopen() returns the address of the symbol,
     or 0 if the symbol is not in the object or another error
     occured.

     The string uses the operating system's native string 
     representation -- `asciiz' on Unix or Win32, 'pascal' on MacOS.
     */

extern void osdep_open_shared_object( word params, word results );
  /* 'params' is a tagged pointer to a vector of parameters.
     'results' is a tagged pointer to a vector of length 2.
     
     The parameters specify a shared object in the system.  If the
     object can be found then the results vector is initialized with
     a pointer to a vector of code pointers and a pointer to a string
     representing program data, both obtained from the shared object,
     and the procedure returns #t.  If the object cannot be found, or
     it does not contain the expected data, then the procedure returns
     #f and leaves results unchanged.
     */

extern int osdep_setenv(const char *name, const char *value,
                         int overwrite);
  /* Set the environment variable name to the given value if either it
   * is unset or overwrite is true.  Returns non-zero on failure. */

#endif /* !defined(INCLUDED_OSDEP_H) */

/* eof */
