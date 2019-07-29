//////////
//
//
#include <stdio.h>
#include "mpi.h"

#define LEN 20

int main(int argc, char **argv) {
	int rank, size;
	int bstart, sharelen;
	MPI_Offset fstart;
	int  i, a[LEN], b[LEN];
	int rd, num;
	
	MPI_File fh0;
	
	MPI_Status status;
	FILE *fw;
	char fwn[512];
	
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	
	if(LEN % size != 0) {
		printf("LEN=%d, should be divided by size=%d\n", LEN, size);
		MPI_Finalize();
		return 0;
	}	

	for ( i=0;i<LEN;i++) {
		a[i] = rank ; 
		b[i] = -111;
	}
	sharelen = LEN / size;
	bstart = rank * sharelen;
	fstart = bstart * sizeof(int);
	printf("Proc:%d sharedlen=%d, bstart=%d, fstart=%ld\n", rank, sharelen, bstart, fstart);

	//add shared write code here///////////////
	//


	//add individual read code here///////////////
	//


	//add shared read code here///////////////
	//


	sprintf(fwn, "W:\\temp\\inteltraining\\examples\\projects\\Debug\\fwshared.%d", rank);
	fw = fopen(fwn, "w");
	printf("Proc:%d opend %s for write\n", rank, fwn);fflush(stdout);
	for(i = 0; i < LEN; i++) {
		printf("b[%d] = %d\n", i, b[i]);fflush(stdout);
		fprintf(fw, "b[%d] = %d\n", i, b[i]);
	}
	fclose(fw);

	MPI_Finalize();
	return 0;
}