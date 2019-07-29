#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 65539

int main( int argc, char *argv[] )
{
	MPI_Comm comm = MPI_COMM_WORLD;
	int other, tag = 1, size, s1;
	char *buf;
	int msg[LEN], rmsg[LEN];
	int rank;
	int bufsize;

	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );

	if(rank == 0) other = 1;
	if(rank == 1) other = 0;

	//all processor execute ssend here
	//
	

	//all processor execute recv here
	//
	

	MPI_Finalize();
	return 0;
}