/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
double f(double);

double f(double a)
{
    return (4.0 / (1.0 + a*a));
}

int main(int argc,char *argv[])
{
    int    n, w, myid, numprocs, i;
    double PI25DT = 3.141592653589793238462643;
    double mypi, pi, h, sum, x;
    double startwtime = 0.0, endwtime;
    int    namelen;
    char   processor_name[MPI_MAX_PROCESSOR_NAME];
	//----3, 4-----
	double *rpis;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    MPI_Get_processor_name(processor_name,&namelen);

	//----3, 4-----
	rpis = (double*)malloc(sizeof(double)*numprocs);


    fprintf(stdout,"Process %d of %d is on %s\n", myid, numprocs, processor_name);
    fflush(stdout);

    n = 10000;	
    
	startwtime = MPI_Wtime();

	//add broad cast here
	//
	w = -111;
	printf("before::: proc:%d, w=%d\n", myid, w);fflush(stdout);
	if(myid == 0) {
		MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
	} else {
		MPI_Bcast(&w, 1, MPI_INT, 0, MPI_COMM_WORLD);
		n = w;
	}
    

    h   = 1.0 / (double) n;
    sum = 0.0;
    
    for (i = myid + 1; i <= n; i += numprocs)
    {
	x = h * ((double)i - 0.5);
	sum += f(x);
    }
    mypi = h * sum;

	//----1----- add reduce code here
	//
	/*
	MPI_Reduce(&mypi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    if (myid == 0) {
			endwtime = MPI_Wtime();
			printf("pi is approximately %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
			printf("wall clock time = %f\n", endwtime-startwtime);	       
			fflush(stdout);
    }*/

	//----2----- add Allreduce code here
	//
	MPI_Allreduce(&mypi, &pi, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
	endwtime = MPI_Wtime();


	//----3----- add Gather code here
	//

	
	//----4----- add Allgather code here
	//
    

/*
	pi = 0.0;
	for(i = 0; i < numprocs; i++) {
		pi += rpis[i];
	}*/
	endwtime = MPI_Wtime();
	printf("Proc:%d pi is approximately %.16f, Error is %.16f\n", myid, pi, fabs(pi - PI25DT));
	printf("Proc:%d wall clock time = %f\n", myid, endwtime-startwtime);	       
	fflush(stdout);
	free(rpis);


    MPI_Finalize();
    return 0;
}
