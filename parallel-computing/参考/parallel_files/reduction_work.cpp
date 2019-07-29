#include <mpi.h>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <string>
#include <sys/time.h>

#define epsilon 1.e-8

using namespace std;

int main(int argc, char* argv[])
{
	int M, N;
	string T, P, Db;
	//double elapsedTime,elapsedTime2;
	//timeval start,end,end2;
	int rank = 0;
	int commonsize = 0;
	MPI_Status status;
	MPI_Request handle;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &commonsize);
	M = atoi(argv[1]);
	N = atoi(argv[2]);

	if (argc > 3)
	{
		T = argv[3];
		if (argc > 4)
		{
			P = argv[4];
			if (argc > 5)
			{
				Db = argv[5];
			}
		}
	}

	double **U_t;
	double alpha, beta, gamma, **Alphas, **Betas, **Gammas;

	int acum = 0;
	//int temp1, temp2;


	U_t = new double*[N];
	Alphas = new double*[N];
	Betas = new double*[N];
	Gammas = new double*[N];

	for (int i = 0; i<N; i++)
	{
		U_t[i] = new double[N];
		Alphas[i] = new double[N];
		Betas[i] = new double[N];
		Gammas[i] = new double[N];
	}


	//Read from file matrix, if not available, app quit
	//Already transposed
	ifstream matrixfile("matrix");
	if (!(matrixfile.is_open()))
	{
		cout << "Error: file not found" << endl;
		return 0;
	}
	for (int i = 0; i < M; i++)
	{
		for (int j = 0; j < N; j++)
		{
			matrixfile >> U_t[i][j];
		}
	}
	matrixfile.close();

	/* Reductions */

	//gettimeofday(&start, NULL);
	//double conv;
	double r[3];
	double r1;
	double r2;
	double r3;
	int flag = 0;
	for (int i = 0; i<M; i++)
	{
		//convergence
		for (int j = 0; j<M; j++)
		{
			alpha = 0.0;
			beta = 0.0;
			gamma = 0.0;
			for (int k = 0; k<N; k++)
			{
				r[0] = U_t[i][k] * U_t[i][k];
				r[1] = U_t[j][k] * U_t[j][k];
				r[2] = U_t[i][k] * U_t[j][k];
				r1 = U_t[i][k] * U_t[i][k];
				r2 = U_t[j][k] * U_t[j][k];
				r3 = U_t[i][k] * U_t[j][k];

				if (rank != 0)
				{
					MPI_Send(r, 3, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
				}
				else
				{
					for (int source = 1; source < commonsize; source++)
					{
						MPI_Recv(r, 3, MPI_DOUBLE, source, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
						alpha = alpha + r[0];
						beta = beta + r[1];
						gamma = gamma + r[2];
					}
				}
			}
			if (rank == 0)
			{
				cout << i << ":" << j << endl;
				Alphas[i][j] = alpha;
				Betas[i][j] = beta;
				Gammas[i][j] = gamma;
			}
		}
	}

	//Output time and iterations 
	if (rank == 0)
	{
		if (T == "-t" || P == "-t")
		{
			//elapsedTime = (end.tv_sec - start.tv_sec) * 1000.0;
			//elapsedTime += (end.tv_usec - start.tv_usec) / 1000.0;
			//cout<<"Time: "<<elapsedTime<<" ms."<<endl<<endl;
		}
		// Output the matrixes for debug
		if (T == "-p" || P == "-p")
		{
			cout << "Alphas" << endl << endl;
			for (int i = 0; i<M; i++)
			{
				for (int j = 0; j<N; j++)
				{
					cout << Alphas[i][j] << "  ";
				}
				cout << endl;
			}
			cout << endl << "Betas" << endl << endl;
			for (int i = 0; i<M; i++)
			{
				for (int j = 0; j<N; j++)
				{
					cout << Betas[i][j] << "  ";
				}
				cout << endl;
			}
			cout << endl << "Gammas" << endl << endl;
			for (int i = 0; i<M; i++)
			{
				for (int j = 0; j<N; j++)
				{
					cout << Gammas[i][j] << "  ";
				}
				cout << endl;
			}
		}
		//Generate files for debug purpouse
		if (Db == "-d" || T == "-d" || P == "-d")
		{
			ofstream Af;
			//file for Matrix A
			Af.open("AlphasAPI.mat");
			/* Af<<"# Created from debug\n# name: A\n# type: matrix\n# rows: "<<M<<"\n# columns: "<<N<<"\n";*/
			Af << M << "  " << N;
			for (int i = 0; i<M; i++)
			{
				for (int j = 0; j<N; j++)
				{
					Af << " " << Alphas[i][j];
				}
				Af << "\n";
			}
			Af.close();

			ofstream Uf;
			//File for Matrix U
			Uf.open("BetasAPI.mat");
			/* Uf<<"# Created from debug\n# name: Ugpu\n# type: matrix\n# rows: "<<M<<"\n# columns: "<<N<<"\n";*/
			for (int i = 0; i<M; i++)
			{
				for (int j = 0; j<N; j++)
				{
					Uf << " " << Betas[i][j];
				}
				Uf << "\n";
			}
			Uf.close();

			ofstream Vf;
			//File for Matrix V
			Vf.open("GammasAPI.mat");
			/* Vf<<"# Created from debug\n# name: Vgpu\n# type: matrix\n# rows: "<<M<<"\n# columns: "<<N<<"\n";*/
			for (int i = 0; i<M; i++)
			{
				for (int j = 0; j<N; j++)
				{
					Vf << " " << Gammas[i][j];
				}
				Vf << "\n";
			}
			Vf.close();
			ofstream Sf;
		}

	}

	for (int i = 0; i<N; i++)
	{
		delete[] Alphas[i];
		delete[] U_t[i];
		delete[] Betas[i];
		delete[] Gammas[i];
	}
	delete[] Alphas;
	delete[] Betas;
	delete[] Gammas;
	delete[] U_t;
	MPI_Finalize();
	return 0;
}
