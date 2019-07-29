#include "mpi.h"
#include <stdio.h>
#define SIZE 4

int main(int argc, char **argv)
{
	int numtasks, rank, sendcount, recvcount, source;
	float sendbuf[SIZE][SIZE] = {
	  {1.0, 2.0, 3.0, 4.0},
	  {5.0, 6.0, 7.0, 8.0},
	  {9.0, 10.0, 11.0, 12.0},
	  {13.0, 14.0, 15.0, 16.0}  };
	float recvbuf[SIZE];
	char pf[SIZE*SIZE*8+512];
	int i;

	//for reduce_scatter
	int rcounts[SIZE];

	MPI_Init(&argc,&argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

	if (numtasks == SIZE) {
		source = 1;
		sendcount = SIZE;
		recvcount = SIZE;

		
		//add scatter code here
		//
		
		
		//add reduce_scatter code here
		//
		

		//add Scan code here
		//
		
	} else {
		printf("Must specify %d processors. Terminating.\n",SIZE);
	}

	sprintf(pf, "Proc:%d received:\n", rank);
	for(i = 0; i < SIZE - 1; i++) {
		sprintf(pf,"%srecvbuf[%d]=%f,",pf, i, recvbuf[i]);
	}
	sprintf(pf,"%srecvbuf[%d]=%f\n",pf,i, recvbuf[i]);
	printf("%s", pf);

	MPI_Finalize();
}
