#include <sys/socket.h>
#include <stddef.h>

main()
{
  struct sockaddr x;
  printf( "sizeof(struct sockaddr) = %d\n", sizeof( x ) );
  printf( "offsetof( x, sa_family ) = %d\n",
	 offsetof( struct sockaddr, sa_family ) );
  printf( "offsetof( x, sa_data ) = %d\n",
	 offsetof( struct sockaddr, sa_data ) );
}
