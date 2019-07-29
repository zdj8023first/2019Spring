#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "mpi.h"

int main(int argc, char **argv)
{
	int rank, size;
	int chunk = 2;
	char outstr[500];
	
	int i, j;

	int *sb;
	int *rb;

	int status, gstatus;
	
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	sb = (int *)malloc(size *chunk * sizeof(int));
	if(!sb) {
		fprintf(stdout, "can not allocate send buffer");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}
	rb = (int *)malloc(size * chunk * sizeof(int));
	if(!rb) {
		fprintf(stdout, "can not allocate receive buffer");
		free(sb);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}
	
	sprintf(outstr, "proc: %d ", rank);
	for(i = 0; i < size; i++) {
		sprintf(outstr, "%s \nsend to: %d id=%d: ", outstr, i,i);
		for(j = 0; j < chunk; j++) {
			sb[i * chunk + j] = rank + i * chunk + j;
			sprintf(outstr, "%s data[%d]=%d", outstr, j, sb[i * chunk + j]);
			rb[i * chunk + j] = -111;
		}
	}
	fprintf(stdout, "%s\n", outstr);
	
	//add alltoall code here
	//
	

	sprintf(outstr, "proc: %d\n", rank);
	for(i = 0; i < size; i++) {
		sprintf(outstr, "%s receive from %d id=%d: ", outstr, i, i);
		for(j = 0; j < chunk; j++) {
			sprintf(outstr, "%s data[%d]=%d", outstr, j, rb[i * chunk + j]);
		}
		sprintf(outstr, "%s\n", outstr);
	}
	fprintf(stdout, "%s\n", outstr);

	free(sb);
	free(rb);
	
	MPI_Finalize();
	return 0;
}
