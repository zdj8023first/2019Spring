#include <iostream>
#include <cstring> 
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <algorithm>
#include <mpi.h>

#include <sys/time.h>

//#include <Windows.h>
//#include <time.h>


using namespace std;

void verify(char *a, char*b, int size) {
	for (int i = 0; i<size; ++i) {
		if (a[i] != b[i]) {
			printf(" Number %d is failed\n", i);
		}
	}
	printf("verify success\n");
}

char* Creat_arry(char *array, int asize_p) {
	array = (char*)malloc(sizeof(char) * (asize_p + 1));
	srand((unsigned)time(NULL));
	for (int i = 0; i< asize_p; ++i)
		array[i] = (rand() % 10) + 1;
	array[asize_p] = '\0';
	return array;
}


int main(int argc, char** argv) {

	char *a = NULL, *b = NULL;
	int rank, size;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	//数据大小
	int asize;
	if (argc < 2) {
		if (rank == 0)
			printf("error input\n");
		return 0;
	}
	else
		asize = atoi(argv[1]);


	int offset = rank*(asize / size);
	int asize_p;

	if (rank == size - 1)
		asize_p = asize - (asize / size)*(size - 1);

	else
		asize_p = asize / size;


	//初始化所有线程

	char* array = NULL;
	array = Creat_arry(array, asize_p);

	printf("Number %d:", rank);
	for (int i = 0; i< asize_p; ++i)
		printf("%d ", array[i]);
	printf("\n");

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_File fh;
	MPI_Status status;
	MPI_File_open(MPI_COMM_WORLD, "./result.txt", MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
	MPI_File_write_at(fh, offset, array, asize_p, MPI_CHAR, &status);
	MPI_File_close(&fh);

	MPI_Barrier(MPI_COMM_WORLD);

	if (rank == 0) {
		a = (char*)malloc((asize + 1) * sizeof(char));
		b = (char*)malloc((asize + 1) * sizeof(char));
	}

	if (rank == 0) {
		memcpy(a, array, asize_p);
		for (int i = 1; i<size - 1; ++i)
			MPI_Recv(&(a[i*(asize / size)]), asize / size, MPI_CHAR, i, 0, MPI_COMM_WORLD, &status);
		if (size>1)
			MPI_Recv(&(a[(size - 1)*(asize / size)]), asize - (asize / size)*(size - 1), MPI_CHAR, size - 1, 0, MPI_COMM_WORLD, &status);
		free(array);
	}
	else {
		MPI_Send(array, asize_p, MPI_CHAR, 0, 0, MPI_COMM_WORLD);
		free(array);
	}

	MPI_Barrier(MPI_COMM_WORLD);

	if (rank == 0) {
		char c = 0;
		FILE *fp = NULL;
		//fp = fopen("./result.txt", "r");
		fp = fopen("./result.txt", "r");
		for (int i = 0; i< asize; i++)
			if ((c = fgetc(fp)) != EOF)
				b[i] = c;
			else
				printf("error read\n");

		a[asize] = '\0';
		b[asize] = '\0';
		printf("a:");
		for (int i = 0; i< asize; ++i)
			printf("%d ", a[i]);
		printf("\n");
		printf("b:");
		for (int i = 0; i< asize; ++i)
			printf("%d ", b[i]);
		printf("\n");
		verify(a, b, asize);


	}
	free(a);
	free(b);

	MPI_Finalize();

}

