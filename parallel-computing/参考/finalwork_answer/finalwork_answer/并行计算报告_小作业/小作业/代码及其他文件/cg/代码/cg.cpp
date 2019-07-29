#include "stdio.h"
#include "math.h"
#include "time.h"
#include <iostream>
#include <omp.h>
#include <stdlib.h>
// 设置并行内核数
#define number_threads 2
using namespace std;

void Matrix_Multiplication(int nrows, double *ax, double *sra, int *clm, int *fnz, double *x);
double dotproduct(int nrows, double *z, double *r);
double get_max_value(int nrows, double *r);
void Parallel_CG(int nrows, double *sra, int*clm, int *fnz, double *x, double *b, int &iter, double tol);


int main(){

	
	double start = omp_get_wtime();//获取起始时间  
	double start_t,finish_t;

	int nrows = 5000;		//行数
	int iter = 120000;    //最大迭代次数
	
	double *sra = new double[3*nrows-2];
	sra[0] = 1.0;
	int i = 1;
	int boundary = 3*nrows-2;
	#pragma omp parallel for num_threads(number_threads)
	// sra=[1 2  2 2 3  3 3 4  4 4 5 ……]
	for(i = 1; i < boundary; ++i){
		if(i%3 == 1)
			sra[i] = (int)(i/3) + 2;
		else if(i%3 == 2)
			sra[i] = (int)(i/3) + 2;
		else
			sra[i] = (int)(i/3) + 1;
	}
	
	int *clm = new int[3*nrows-2];
	clm[0] = 0;
	clm[1] = 1;
	#pragma omp parallel for num_threads(number_threads)
	// clm=[0 1  0 1 2  1 2 3  2 3 4  3 4 5  4 5 6 ……]
	for(i = 2; i < 3*nrows-4; ++i){
		if(i%3 == 2)
			clm[i] = (int)(i/3);
		else if(i%3 == 0)
			clm[i] = (int)(i/3);
		else
			clm[i] = (int)(i/3)+1;
	}
	clm[3*nrows-4] = nrows-2;
	clm[3*nrows-3] = nrows-1;

	int *fnz = new int[nrows+1];
	fnz[0] = 0;
	fnz[1] = 2;
	// fnz = [0, 2, 5, 8, 11, 14, 17, 20, 23, 26, 28……]
	for(i = 2; i < nrows; ++i)
		fnz[i] = fnz[i-1] + 3;
	fnz[nrows] = 3*nrows-2;
	

	double *b = new double[nrows];

	#pragma omp parallel for num_threads(number_threads)
	// b=[0 0 0 0 0 0 0 0 0……]
	for(int i = 0; i < nrows; ++i)
		b[i] = 0;

	printf("The exact solution of x is:\n");
	double *x = new double[nrows];
	#pragma omp parallel for num_threads(number_threads)
	for(int i = 0; i < nrows; ++i){
		x[i] = 1.0/(i+3.14);

	}

	printf("Solving the equation...\n\n");

	Matrix_Multiplication(nrows,b, sra, clm, fnz, x);

	#pragma omp parallel for num_threads(number_threads)
	for(int i = 0; i < nrows; ++i)
		x[i] = 0.0;

	double tol = 0.000001;

	start_t = clock();
	Parallel_CG(nrows,sra,clm,fnz,x,b,iter,tol);
	finish_t = clock();
	double execute_time = (finish_t - start_t)/CLOCKS_PER_SEC;
	execute_time = execute_time*1000;
	for(int i = 0; i < 10; ++i)
		printf("x[%d] = %f\n",i,x[i]);
	printf("Total iteration times: %d\n", iter);
	printf("The execute time is %f ms.\n",execute_time);
	double end = omp_get_wtime( );  
	double all_use_time = 1000*(end -start);
	printf("all_use_time %f ms.\n",all_use_time);
	//system("pause");
	//getchar();
	return 0;
}

// 矩阵相乘运算
void Matrix_Multiplication(int nrows, double *ax, double *sra, int *clm, int *fnz, double *x){
	#pragma omp parallel for num_threads(number_threads)
	for(int i = 0; i < nrows; ++i)
		for(int j = fnz[i]; j < fnz[i+1]; ++j)
			ax[i] += (sra[j] * x[clm[j]]);
}

// 用于rT*r转置矩阵乘以本身计算
double Transpose_matrix_Mul(int nrows, double *z, double *r){
	double temp = 0.0;
	int i = 0;
	#pragma omp parallel for num_threads(number_threads) reduction(+:temp)
	for(i= 0; i < nrows; ++i)
		temp += (z[i] * r[i]);
	return temp;
}

// 获取向量最大值
double get_max_value(int nrows, double *r){
	double temp = abs(r[0]);
	#pragma omp parallel for num_threads(number_threads)
	for(int i = 1; i < nrows; ++i)
		if(temp < abs(r[i]))
			temp = abs(r[i]);
	return temp;
}

// 稀疏矩阵求解函数
void Parallel_CG(int nrows, double *sra, int*clm, int *fnz, double *x, double *b, int &iter, double tol){
	int maxiter = iter;
	double *r = new double[nrows];
	double *Ap = new double[nrows];
	double *newr = new double[nrows];

	#pragma omp parallel for num_threads(number_threads)
	for(int i = 0; i < nrows; ++i){
		r[i] = 0.0;
		Ap[i] = 0.0;
		newr[i] = 0.0;
	}

	Matrix_Multiplication(nrows,Ap,sra,clm,fnz,x);

	#pragma omp parallel for num_threads(number_threads)
	for(int i = 0; i < nrows; ++i)
		r[i] = b[i] - Ap[i];
	double *p = new double[nrows];

	#pragma omp parallel for num_threads(number_threads)
	for(int i = 0; i < nrows; ++i)
		p[i] = r[i];

	double pAp = 0.0;
	double rr = 0.0;
	double alpha = 0.0;
	double newrnewr = 0.0;
	double beta = 0.0;

	//循环语句中有break跳出操作，不能实现并行化
	for(iter = 0; iter < maxiter; ++iter){
		#pragma omp parallel for num_threads(number_threads)
		for(int i = 0; i < nrows; ++i)
			{
				Ap[i] = 0.0;
			}
		Matrix_Multiplication(nrows,Ap,sra,clm,fnz,p);
		pAp = Transpose_matrix_Mul(nrows,p,Ap);
		rr = Transpose_matrix_Mul(nrows,r,r);
		alpha = rr/pAp;
		#pragma omp parallel for num_threads(number_threads)
		for(int i = 0; i < nrows; ++i){
			x[i] += (alpha * p[i]);
			newr[i] = r[i] - alpha * Ap[i];
		}
		if(!(get_max_value(nrows,r) > tol))
			break;
		// (r,r)
		newrnewr = Transpose_matrix_Mul(nrows,newr,newr);
		beta = newrnewr/rr;
		#pragma omp parallel for num_threads(number_threads)
		for(int i = 0; i < nrows; ++i){
			p[i] = newr[i] + beta * p[i];
			r[i] = newr[i];
		}
	}
}



