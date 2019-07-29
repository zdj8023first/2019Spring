#include "mpi.h"
#include <stdio.h>
#define SIZE 4

int main(int argc, char **argv)
{
	int numtasks, rank, source=0, dest, tag=1, i, j;
	float a[SIZE][SIZE] =
	{
		{1.0, 1.1, 1.2, 1.3},
		{2.0, 2.1, 2.2, 2.3},
		{3.0, 3.1, 3.2, 3.3},
		{4.0, 4.1, 4.2, 4.3},
	};
	float aa[SIZE*SIZE] = {1.0,1.1,1.2,1.3,2.0,2.1,2.2,2.3,3.0,3.1,3.2,3.3,4.0,4.1,4.2,4.3};
	float b[SIZE][SIZE];
	char outstr[600];

	//for index data type
	int blklen[2], dspl[2];
	MPI_Datatype idxdtype[SIZE];

	MPI_Status stat;
	MPI_Datatype rowtype, coltype;

	MPI_Init(&argc,&argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

	//add definition and commitment code of contiguous data type here
	//

	//add definition and commitment code of vector data type here
	//
	
	//add definition and commitment code of indexed data type here
	//

	if (numtasks == SIZE) {
		if (rank == 0) {
			sprintf(outstr, "rank:%d has \n{\n", rank);
			for(i=0; i < SIZE; i++) {
				sprintf(outstr, "%s\t{", outstr);
				for(j = 0; j < SIZE; j++) {
					if(j < SIZE-1) {
						sprintf(outstr, "%s%f, ", outstr, a[i][j]);
					} else {
						sprintf(outstr, "%s%f},\n", outstr, a[i][j]);
					}
				}
			}
			fprintf(stdout, "%s}\n", outstr);fflush(stdout);
			for (i = 1; i < numtasks; i++) {
				//add code for sending contigouse data type here
				//
				
				
				//add code for sending vector data type here
				//

				
				//add code for sending indexed data type here
				//

			}
		} else {
			for(i = 0; i < SIZE; i++) {
				for(j = 0; j < SIZE; j++) {
					b[i][j] = 0.99999;
				}
			}
			//add code for receiving contigouse data type here
			//
		
			
			//add code for receiving vector data type here
			//

			
			//add code for receiving indexed data type here
			//

			sprintf(outstr, "rank:%d received:  {\n", rank);
			for(i = 0; i < SIZE; i++) {
				sprintf(outstr, "%s\t{", outstr);
				for(j = 0; j < SIZE; j++) {
					if(j < SIZE - 1) {
						sprintf(outstr, "%s%f, ", outstr, b[i][j]);
					} else {
						sprintf(outstr, "%s%f}\n", outstr, b[i][j]);
					}
				}
			}
			fprintf(stdout, "\n%s}\n", outstr);fflush(stdout);
		}
	} else {
		printf("Must specify %d processors. Terminating.\n",SIZE);
		MPI_Type_free(&rowtype);
		MPI_Finalize();
		return 0;
	}

	MPI_Type_free(&rowtype);
	//for vec data type
	//MPI_Type_free(&coltype);
	//for index datatype
	for(i = 0; i < SIZE; i++) {
		//MPI_Type_free(&idxdtype[i]);
	}
	MPI_Finalize();
	return 0;
}

