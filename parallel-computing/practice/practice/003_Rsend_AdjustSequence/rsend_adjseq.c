#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>

#include "mpi.h"

#define LEN 66666

int main( int argc, char *argv[] )
{
	MPI_Comm comm = MPI_COMM_WORLD;
	int other, tag = 1, size;
	int msg[LEN], rmsg[LEN];
	int rank;

	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );

	if(rank == 0) other = 1;
	if(rank == 1) other = 0;

	if(rank == 1) {
		//proc 1 execute Rsend here
		//
		
	}
	//add a barrier here will cause error
	//
	

	if(rank == 0) {
		//proc 0 execute recv here
		//
		
	}

	MPI_Finalize();
	return 0;
}