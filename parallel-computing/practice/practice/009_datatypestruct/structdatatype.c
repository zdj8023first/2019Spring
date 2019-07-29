#include "mpi.h"
#include <stdio.h>
#define NELM 25

int main(int argc, char **argv)
{
	int numtasks, rank, source=0, dest, tag=1, i;

	typedef struct {
		float x, y, z;
		float velocity;
		int  n, type;
	} Particle;
	Particle     p[NELM], particles[NELM];
	MPI_Datatype particletype, oldtypes[2]; 
	int          blockcounts[2];

	/**
	 * MPI_Aint type used to be consistent with syntax of
	 * MPI_Type_extent routine
	 */
	MPI_Aint    offsets[2], extent;
	MPI_Status stat;
	char outstr[600];

	MPI_Init(&argc,&argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
 
	/*add 0'th element description here */
	//

	/*add 1'th element description here */
	//


	/*add definition and commitment code here*/
	//
	

	/* Initialize the particle array and then send it to each task */
	if (rank == 0) {
		//proc 0 sends to all other processes
		//

	} else {
		//other process all receive it from process 0
		//
		

		printf("rank= %d %3.2f %3.2f %3.2f %3.2f %d %d\n", rank,p[3].x, p[3].y,p[3].z,p[3].velocity,p[3].n,p[3].type);
	}
	
	MPI_Type_free(&particletype);
	MPI_Finalize();
}
