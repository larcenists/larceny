! -*- Fundamental -*-
! This is the file Sparc/unix.s.
!
! Larceny run-time system (SPARC) -- trampolines for Unix-related primitives.
!
! History:
!   June 26 - July 1, 1994 / lth (v0.20)
!     Taken out of "glue.s" and cleaned up.
!
! When we get generalized Scheme-to-C calling, these trampolines should go 
! away and be replaced by something using that mechaninsm.

#include "asmdefs.h"

	.global	_m_open_file
	.global	_m_close_file
	.global	_m_unlink_file
	.global	_m_read_file
	.global	_m_write_file
	.global _m_resource_usage
	.global _m_exit
	.global	_m_dumpheap


! _m_open_file: open a file, return the file descriptor or -1
!
! Call from: Scheme
! Input:     RESULT = string: filename
!            ARGREG2 = fixnum: flags
!            ARGREG3 = fixnum: mode
! Output:    RESULT = fixnum: file descriptor or -1
! Destroys:  Temporaries, RESULT

_m_open_file:
	set	_UNIX_openfile, %TMP0
	b	callout_to_C
	nop


! _m_unlink_file: delete a file.
!
! Call from: Scheme
! Input:     RESULT = string: filename
! Output:    RESULT = fixnum: return code from Unix
! Destroys:  Temporaries, RESULT

_m_unlink_file:
	set	_UNIX_unlinkfile, %TMP0
	b	callout_to_C
	nop


! CLOSE: close an open file
!
! Call from: Scheme
! Input    : RESULT = fixnum: file descriptor
! Output   : RESULT = fixnum: return code from Unix
! Destroys : Temporaries, RESULT

_m_close_file:
	set	_UNIX_closefile, %TMP0
	b	callout_to_C
	nop


! _m_read_file: read from file desriptor
!
! Call from: Scheme
! Input    : RESULT = fixnum: file desc
!            ARGREG2 = string: buffer
!            ARGREG3 = fixnum: byte count
! Output   : RESULT = fixnum: bytes actually read, or -1.
! Destroys : Temporaries, RESULT

_m_read_file:
	set	_UNIX_readfile, %TMP0
	b	callout_to_C
	nop


! _m_write_file: write to file descriptor
!
! Call from: Scheme
! Input    : RESULT = fixnum: file desc
!            ARGREG2 = string: buffer
!            ARGREG3 = fixnum: byte count
! Output   : RESULT = fixnum: bytes actually written, or -1.
! Destroys : Temporaries, RESULT

_m_write_file:
	set	_UNIX_writefile, %TMP0
	b	callout_to_C
	nop


! _m_resource_usage: get resource data.
!
! Call from: Scheme
! Input:     RESULT = vector: vector to be filled in.
! Output:    Nothing
! Destroys:  Temporaries

_m_resource_usage:
	set	_UNIX_getresourceusage, %TMP0
	b	callout_to_C
	nop


! _m_exit: terminate
!
! Call from: Scheme
! Input:     Nothing
! Output:    Nothing
! Destroys:  Everything :-)
!
! Terminate the program by calling exit(); Unix closes files and all that.
! We assume there is a Scheme wrapper around this to clean up buffers etc.
!
! Possibly this should not go directly to _exit; hardly a problem right now.

_m_exit:
	set	_exit, %TMP0
	b	callout_to_C
	set	0, %TMP1


! _m_dumpheap: dump the heap to a file.
!
! Dump the heap to a file. There should be a wrapper to validate arguments.
!
! Call from: Scheme
! Input    : RESULT = string: file name
!            ARGREG2 = procedure: startup procedure
! Output   : RESULT = boolean: success of operation
! Destroys : Temporaries

_m_dumpheap:
	set	_UNIX_dumpheap, %TMP0
	b	callout_to_C
	nop

! eof
