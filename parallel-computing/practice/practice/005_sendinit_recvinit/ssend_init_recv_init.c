//ssend_init_recv_init.c
#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>
 
#define DATALEN 5
int main(int argc, char *argv[])
{
	MPI_Request r;
	MPI_Status s;
	char pstr[DATALEN * (sizeof(int) + 8) + 100];
	int buf[DATALEN];
	int tag = 27;
	int dest = 0;
	int rank, size, i, k;
 
	MPI_Init( &argc, &argv );
	MPI_Comm_size( MPI_COMM_WORLD, &size );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );

	//all processors initiate sending and stand by
	//
	

	//Use that request
	if (rank == 0) {
		//create request objects and status objects here
		//
		
		
		int **rbuf;
		rbuf = (int**)malloc(sizeof(int *) * size);
		for(i = 0; i < size; i++) {
			rbuf[i] = malloc(sizeof(int) * DATALEN);
		}

		//initiate receiver and stand by
		//


		for(i = 0; i < DATALEN; i++) {
			buf[i] = rank * DATALEN + i;
		}

		//proc 0 really statrt sending 
		//


		//proc 0 really start all receiving
		//


		//proc 0 wait for all receive finished
		//


		//wait for send finished
		//


		for(i = 0; i < size; i++) {
			sprintf(pstr, "proc: %d received %d data from proc %d {", rank, DATALEN, rstatus[i].MPI_SOURCE);
			for(k = 0; k < DATALEN - 1; k++) {
				sprintf(pstr, "%srbuf[%d][%d]=%d,", pstr, i, k, rbuf[i][k]);
			}
			sprintf(pstr, "%srbufp[%d][%d]=%d}\n", pstr, i, k, rbuf[i][k]);
			fprintf(stdout, "%s", pstr);
		}

		for(i = 0; i < size; i++) {
			MPI_Request_free(&rr[i]);
		}

		free(rr);
		free(rstatus);
		for(i = 0; i < size; i++) {
			free(rbuf[i]);
		}
		free(rbuf);
	} else {
		for(i = 0; i < DATALEN; i++) {
			buf[i] = rank * DATALEN + i;
		}
		//proc other than 0 really start sending


		//proc other than 0 wait for sending finished
		//

	}
	
	MPI_Request_free( &r );
    
	MPI_Finalize();
	return 0;
}
