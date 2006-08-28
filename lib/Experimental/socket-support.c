/* Minimal support for Unix-style error codes */

extern int errno;
extern int h_errno;

int get_errno( void ) { return errno; }
int get_h_errno( void ) { return h_errno; }

/* eof */
