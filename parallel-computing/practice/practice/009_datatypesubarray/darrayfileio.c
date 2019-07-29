#include "mpi.h"
#include <stdio.h>

#define DIM 3

#define ELM_NUM 4
#define ELM_NUM_X 4
#define ELM_NUM_Y 4
#define ELM_NUM_Z 4

#define PROC_NUM 2
#define PROC_NUM_X 2
#define PROC_NUM_Y 2
#define PROC_NUM_Z 2

#define SUB_LEN (ELM_NUM/PROC_NUM)
#define SUB_LEN_X (ELM_NUM_X/PROC_NUM_X)
#define SUB_LEN_Y (ELM_NUM_Y/PROC_NUM_Y)
#define SUB_LEN_Z (ELM_NUM_Z/PROC_NUM_Z)
 
void proc_grid3d(int rank, int *coord)  {
	int mod, div;
	div = rank / (PROC_NUM*PROC_NUM);
	mod = rank % (PROC_NUM*PROC_NUM);
	coord[0] = div;
	coord[1] = mod / PROC_NUM;
	coord[2] = mod % PROC_NUM;
}
void get_start_point(int rank, int *starts, int *coord) {
	int i;
	proc_grid3d(rank, coord);
	for(i = 0; i < DIM; i++) {
		starts[i] = coord[i] * (ELM_NUM/PROC_NUM);
	}
	printf("Proc: %d, coord=%d,%d,%d;starts=%d,%d,%d\n", rank, coord[0], coord[1], coord[2], starts[0],starts[1],starts[2]);
	fflush(stdout);
}
int main(int argc, char *argv[])
{
	int rank, size, count, buflen;
	MPI_Status status;
	MPI_Status stats[2];
	MPI_Datatype subarray, darray;
	int gsizes[DIM], distribs[DIM], dargs[DIM], psizes[DIM];
	int starts[DIM], coords[DIM];
	//	local_array_size	//num_local_x,y,z
	int larysz, 			lxnum, lynum, lznum; 
	int xprocs, yprocs, zprocs, xrank, yrank, zrank; 
	int i,j,k,m,n;
	MPI_File fh;
	MPI_Offset pos, bpos;
	int *larys, *laryr;

	int ***arrays;
	int ***arrayr;
	char *outb;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size > PROC_NUM_X*PROC_NUM_Y*PROC_NUM_Z) {
		printf("This example run with at most %d processors\n", PROC_NUM_X*PROC_NUM_Y*PROC_NUM_Z);
		fflush(stdout);
		MPI_Finalize();
		return 0;
	}

	outb = (char*)malloc(ELM_NUM*ELM_NUM*ELM_NUM*40+24);
	arrays = (int ***)malloc(ELM_NUM*sizeof(int**));
	arrayr = (int ***)malloc(ELM_NUM*sizeof(int**));
	for(i = 0; i < ELM_NUM; i++) {
		arrays[i] = (int **)malloc(ELM_NUM*sizeof(int*));
		arrayr[i] = (int **)malloc(ELM_NUM*sizeof(int*));
	}

	for(i = 0; i < ELM_NUM; i++) {
		for(j = 0; j < ELM_NUM; j++) {
			arrays[i][j] = (int *)malloc(ELM_NUM*sizeof(int));
			arrayr[i][j] = (int *)malloc(ELM_NUM*sizeof(int));
		}
	}

	for(i = 0; i < ELM_NUM; i++) {
		for(j = 0; j < ELM_NUM; j++) {
			for(k = 0; k < ELM_NUM; k++) {
				arrays[i][j][k] = i*100+j*10+k;
				arrayr[i][j][k] = -111;
			}
		}
	}

	psizes[0] = PROC_NUM_X;
	psizes[1] = PROC_NUM_Y;
	psizes[2] = PROC_NUM_Z;

	gsizes[0] = ELM_NUM_X;
	gsizes[1] = ELM_NUM_Y;
	gsizes[2] = ELM_NUM_Z;

	distribs[0] = MPI_DISTRIBUTE_BLOCK;
	distribs[1] = MPI_DISTRIBUTE_BLOCK;
	distribs[2] = MPI_DISTRIBUTE_BLOCK;
	
	dargs[0] = MPI_DISTRIBUTE_DFLT_DARG;
	dargs[1] = MPI_DISTRIBUTE_DFLT_DARG;
	dargs[2] = MPI_DISTRIBUTE_DFLT_DARG;

	xprocs = PROC_NUM_X;
	yprocs = PROC_NUM_Y;
	zprocs =PROC_NUM_Z;

	proc_grid3d(rank, coords);
	//num_local_rows = (gsizes[0] + xprocs - 1) / xprocs; 
	lxnum = (gsizes[0] + xprocs - 1) / xprocs;
	if(coords[0] == xprocs-1) {
		lxnum = gsizes[0] - (xprocs-1) * lxnum;
	}
	lynum = (gsizes[1] + yprocs - 1) / yprocs;
	if(coords[1] == yprocs-1) {
		lynum = gsizes[1] - (yprocs-1) * lynum;
	}
	lznum = (gsizes[2] + zprocs - 1) / zprocs; 
	if(coords[2] == zprocs-1) {
		lznum = gsizes[2] - (zprocs-1) * lznum;
	}
	larysz = lxnum*lynum*lznum;
	larys = (int *)malloc(larysz * sizeof(int) ); 
	laryr = (int *)malloc(larysz * sizeof(int) ); 
	for(i = 0; i < larysz; i++) {
		larys[i] = rank+1;
		laryr[i] = -1*(rank+1);
	}
	

	/* Create a darray datatype */
	printf("Proc:%d before create type\n", rank);fflush(stdout);
	MPI_Type_create_darray(size, rank, DIM, gsizes, distribs, dargs, psizes, MPI_ORDER_C, MPI_INT, &darray);
	printf("Proc:%d after create type\n", rank);fflush(stdout);
	
	MPI_Type_commit(&darray);

	MPI_File_open(MPI_COMM_WORLD, "temp", MPI_MODE_RDWR | MPI_MODE_CREATE, MPI_INFO_NULL, &fh);
	
	MPI_File_set_view(fh, 0, MPI_INT, darray, "native", MPI_INFO_NULL);
	//MPI_File_set_view(fh, 0, MPI_INT, MPI_INT, "native", MPI_INFO_NULL);
	
	MPI_File_write_all(fh, larys, larysz, MPI_INT, &stats[0]);
	//MPI_File_write_all(fh, arrays, 1, darray, &stats[0]);

	MPI_Get_count(&stats[0], MPI_INT, &count);
	MPI_File_get_position(fh, &pos);
	MPI_File_get_byte_offset(fh, pos, &bpos);
	printf("Proc: %d, write %d number, position=%ld, byteoffset=%ld\n", rank, count, pos, bpos);
	fflush(stdout);
	
	for(i = 0; i < ELM_NUM; i++) {
		for(j = 0; j < ELM_NUM; j++) {
			for(k = 0; k < ELM_NUM; k++) {
				arrayr[i][j][k] = -111;
			}
		}
	}

	MPI_File_seek(fh, 0, MPI_SEEK_SET);
	MPI_File_read_all(fh, laryr, larysz, MPI_INT, &stats[1]);
	//MPI_File_read_all(fh, arrayr, 1, darray, &stats[1]);

	MPI_Get_count(&stats[1], MPI_INT, &count);
	MPI_File_get_position(fh, &pos);
	MPI_File_get_byte_offset(fh, pos, &bpos);
	sprintf(outb, "Proc: %d, read %d number, position=%ld, byteoffset=%ld\n\t", rank, count, pos, bpos);
	///*
	for(i = 0; i < larysz; i++) {
		sprintf(outb, "%s[%d]=%d;",outb,i, laryr[i]);
	}
	//*/
	/*
	for(i = 0; i < ELM_NUM; i++) {
		for(j = 0; j < ELM_NUM; j++) {
			for(k = 0; k < ELM_NUM; k++) {
				sprintf(outb, "%s[%d,%d,%d]=%d;",outb,i,j,k, arrayr[i][j][k]);
			}
		}
	}
	*/
	printf("%s\n", outb);fflush(stdout);
	MPI_File_close(&fh);
///*
	if(rank == 0) {
		//MPI_Send(larys, 1, darray, 1, 123, MPI_COMM_WORLD);
		MPI_Send(arrays, 1, darray, 1, 123, MPI_COMM_WORLD);
	} else if(rank == 1) { 
		sprintf(outb, "rank:%d, before:\n\t", rank);
		for(i = 0; i < ELM_NUM; i++) {
			for(j = 0; j < ELM_NUM; j++) {
				for(k = 0; k < ELM_NUM; k++) {
					sprintf(outb, "%s[%d,%d,%d]=%d;",outb,i,j,k, arrayr[i][j][k]);
				}
			}
		}
		printf("%s\n", outb);fflush(stdout);

		//MPI_Recv(arrayr, 1, darray, 0, 123, MPI_COMM_WORLD, &status);
		MPI_Recv(laryr, larysz, MPI_INT, 0, 123, MPI_COMM_WORLD, &status);
		
		sprintf(outb, "rank:%d, after:\n\t", rank);
		for(i = 0; i < larysz; i++) {
			sprintf(outb, "%s[%d]=%d;",outb,i, laryr[i]);
		}
//		for(i = 0; i < ELM_NUM; i++) {
//			for(j = 0; j < ELM_NUM; j++) {
//				for(k = 0; k < ELM_NUM; k++) {
//					sprintf(outb, "%s[%d,%d,%d]=%d;",outb,i,j,k, arrayr[i][j][k]);
//				}
//			}
//		}
		printf("%s\n", outb);fflush(stdout);
	}
//*/
	free(outb);
	for(i = 0; i < ELM_NUM; i++) {
		for(j = 0; j < ELM_NUM; j++) {
			free(arrays[i][j]);
			free(arrayr[i][j]);
		}
	}
	for(i = 0; i < ELM_NUM; i++) {
		free(arrays[i]);
		free(arrayr[i]);
	}
	free(arrays);
	free(arrayr);
	free(larys);
	free(laryr);
	MPI_Type_free(&darray);
	MPI_Finalize();
	return 0;
}
