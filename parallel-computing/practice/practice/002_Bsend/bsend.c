#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 65539

int main( int argc, char *argv[] )
{
	MPI_Comm comm = MPI_COMM_WORLD;
	int dest = 1, src = 0, tag = 1, size;
	int s1, s2, s3;
	char *buf;
	char msg1[7], msg3[17];
	double msg2[2];
	char rmsg1[64], rmsg3[64];
	double rmsg2[64];
	int errs = 0, rank;
	int bufsize, bsize;

	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );

	//calculate the buffer size first
	//
	

	bufsize = 3 * MPI_BSEND_OVERHEAD + s1 + s2 + s3;
	buf = (char *)malloc( bufsize );
	
	//register the user buffer to the system
	//
	MPI_Buffer_attach( buf, bufsize );

	strncpy( msg1, "012345", 7 );
	strncpy( msg3, "0123401234012341", 17 );
	msg2[0] = 1.23; msg2[1] = 3.21;

	if (rank == src) {
		//proc 0 Bsend different messages
		//
		//These message sizes are chosen to expose any alignment problems
		
		
		printf("Proc:%d sent 3 messages\n", rank);
	}

	if (rank == dest) {
		//proc 1 receive different messages
		//

		printf("Proc:%d received 3 messages\n", rank);
		if (strcmp( rmsg1, msg1 ) != 0) {
			errs++;
			fprintf( stderr, "message 1 (%s) should be %s\n", rmsg1, msg1 );
			fflush(stderr);
		}
		if (rmsg2[0] != msg2[0] || rmsg2[1] != msg2[1]) {
			errs++;
			fprintf( stderr, 
                        "message 2 incorrect, values are (%f,%f) but should be (%f,%f)\n",
			rmsg2[0], rmsg2[1], msg2[0], msg2[1] );fflush(stderr);
		}
		if (strcmp( rmsg3, msg3 ) != 0) {
			errs++;
			fprintf( stderr, "message 3 (%s) should be %s\n", rmsg3, msg3 );
			fflush(stderr);
		}
	}

	/* We can't guarantee that messages arrive until the detach */
	//de-register the buffer
	//
	MPI_Buffer_detach( &buf, &bufsize);

	free(buf);

	MPI_Finalize();
	return 0;
}