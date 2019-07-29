#include <stdio.h>
#include <math.h>
#include <mpi.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

main(int argc, char** argv){
	
		char *A=NULL, *B=NULL;
		int rank,size;
		MPI_Init( &argc, &argv );
		MPI_Comm_rank( MPI_COMM_WORLD, &rank );
		MPI_Comm_size( MPI_COMM_WORLD, &size ); 
		
		int array_size, arrayPro_size; 
		if (argc < 2){
				if(rank==0)
						printf("Invalid Input!\n");
				return 0;
		}    
		else
				array_size = atoi(argv[1]);

		char* array=NULL;
		
		if(rank!=size-1){
				arrayPro_size = array_size/size;
		}
		else{
				arrayPro_size = array_size-(array_size/size)*rank;
		}

		int offset = rank*(array_size/size);
		array=(char*)malloc((arrayPro_size+1)*sizeof(char));

		int i =0;
		srand((unsigned)time(NULL));
		for(i=0;i< arrayPro_size;i++)
				array[i]=(rand()%100)+1;
		array[arrayPro_size]='\0';
		
#ifdef PRINT
		printf("%d:",rank);
		for(i=0;i< arrayPro_size;i++)
				printf("%d ",array[i]);
		printf("\n");
#endif
		MPI_Barrier(MPI_COMM_WORLD);
		MPI_File fh;
		MPI_Status status;
		MPI_File_open(MPI_COMM_WORLD,"./data.txt",MPI_MODE_CREATE|MPI_MODE_WRONLY,MPI_INFO_NULL,&fh);
		MPI_File_write_at(fh,offset,array,arrayPro_size,MPI_CHAR,&status);
		MPI_File_close(&fh);
		MPI_Barrier(MPI_COMM_WORLD);
		if(rank == 0){
				A=(char*)malloc((array_size+1)*sizeof(char));
				B=(char*)malloc((array_size+1)*sizeof(char));
		}
		if(rank==0){
				memcpy(A,array,arrayPro_size);
				for(i=1;i<size-1;i++){
						MPI_Recv(&(A[i*(array_size/size)]), array_size/size, MPI_CHAR,i, 0, MPI_COMM_WORLD, &status);
				}
				if(size>1) 
						MPI_Recv(&(A[(size-1)*(array_size/size)]), array_size-(array_size/size)*(size-1) , MPI_CHAR,size-1, 0, MPI_COMM_WORLD, &status);
				free(array);
		}else{
				MPI_Send(array, arrayPro_size, MPI_CHAR, 0, 0, MPI_COMM_WORLD);
				free(array);
		}
		MPI_Barrier(MPI_COMM_WORLD);

		if(rank==0){
				char c=0;
				FILE *fp = NULL;
				fp = fopen("./data.txt", "r");
				for(i=0;i< array_size;i++)
						if((c=fgetc(fp))!=EOF){
							B[i]=c;
						}
						else{
							printf("Read File EOF!\n");
						}

				A[array_size]='\0';
				B[array_size]='\0';

#ifdef PRINT
				printf("A:");
				for(i=0;i< array_size;i++)
						printf("%d ",A[i]);
				printf("\n");
				printf("B:");
				for(i=0;i< array_size;i++)
						printf("%d ",B[i]);
				printf("\n");
#endif
				for(i=0;i<array_size;i++){
						if(A[i]!=B[i]){
								printf("Wrong NUMBER at %d\n",i);
								return 0;
						}
				}
				free(A);
				free(B);
				printf("Valid!\n");
		}
		MPI_Finalize();
		return 0;
}
