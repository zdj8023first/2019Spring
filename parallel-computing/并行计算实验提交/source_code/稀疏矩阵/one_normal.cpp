//
// Created by zdj on 19-6-17.
//

#include <iostream>
#include <stdlib.h>
#include "stdio.h"
#include "math.h"
#include "time.h"

using namespace std;



double Mu_Dot(int row, double *a, double *b) {
    double t = 0.0;
//#pragma omp parallel for num_threads(Nthreads) reduction(+:t)
    for (int i = 0; i < row; ++i)
        t += (a[i] * b[i]);
    return t;
}

void Mu_Matrix(int row, double *ax, double *sr, int *cm, int *fn, double *x) {
//#pragma omp parallel for num_threads(Nthreads)
    for (int i = 0; i < row; ++i)
        for (int j = fn[i]; j < fn[i + 1]; ++j)
            ax[i] += (sr[j] * x[cm[j]]);
}


double Normalized(int row, double *r) {
    double t = abs(r[0]);
    for (int i = 1; i < row; ++i)
        if (t < abs(r[i]))
            t = abs(r[i]);
    return t;
}

int main() {
    double stratime = clock();

    int row = 10000;
    int iter = 200000;

    double *sr = new double[3 * row - 2];
    sr[0] = 1.0;
    int i = 1;
    int boundary = 3 * row - 2;
    for (i = 1; i < boundary; ++i) {
        if (i % 3 == 1)
            sr[i] = (int)(i / 3) + 2;
        else if (i % 3 == 2)
            sr[i] = (int)(i / 3) + 2;
        else
            sr[i] = (int)(i / 3) + 1;
    }

    int *cm = new int[3 * row - 2];
    cm[0] = 0;
    cm[1] = 1;

    for (i = 2; i < 3 * row - 4; ++i) {
        if (i % 3 == 2)
            cm[i] = (int)(i / 3);
        else if (i % 3 == 0)
            cm[i] = (int)(i / 3);
        else
            cm[i] = (int)(i / 3) + 1;
    }
    cm[3 * row - 4] = row - 2;
    cm[3 * row - 3] = row - 1;

    int *fn = new int[row + 1];
    fn[0] = 0;
    fn[1] = 2;
    for (i = 2; i < row; ++i)
        fn[i] = fn[i - 1] + 3;
    fn[row] = 3 * row - 2;





    double *b = new double[row];


    for (int i = 0; i < row; ++i)
        b[i] = 0;


    double *x = new double[row];
    for (int i = 0; i < row; ++i) {
        x[i] = 1.0 / (i + 3.14);
        printf("x[%d] = %f\n", i, x[i]);
    }



    // calculate b
    Mu_Matrix(row, b, sr, cm, fn, x);

    // set initial x
    for (int i = 0; i < row; ++i)
        x[i] = 0.0;
    double tol = 0.000001;


    int maxiter = iter;
    double *r = new double[row];
    double *Ap = new double[row];
    double *newr = new double[row];
    double pAp = 0.0;
    double rr = 0.0;
    double alpha = 0.0;
    double up = 0.0;
    double beta = 0.0;

    for (int i = 0; i < row; ++i) {
        r[i] = 0.0;
        Ap[i] = 0.0;
        newr[i] = 0.0;
    }
    Mu_Matrix(row, Ap, sr, cm, fn, x);

    for (int i = 0; i < row; ++i)
        r[i] = b[i] - Ap[i];
    double *p = new double[row];

    for (int i = 0; i < row; ++i)
        p[i] = r[i];



    for (iter = 0; iter < maxiter; ++iter) {
        for (int i = 0; i < row; ++i)
        {
            Ap[i] = 0.0;
        }
        //Ap[i] = 0.0;
        // calculate Ap
        Mu_Matrix(row, Ap, sr, cm, fn, p);
        // pk.Apk
        pAp = Mu_Dot(row, p, Ap);
        // rk.rk
        rr = Mu_Dot(row, r, r);
        //alphak = rk.rk/pk.Apk
        alpha = rr / pAp;
        for (int i = 0; i < row; ++i) {
            x[i] += (alpha * p[i]);
            newr[i] = r[i] - alpha * Ap[i];
        }
        if (!(Normalized(row, r) > tol))
            break;
        up = Mu_Dot(row, newr, newr);
        beta = up / rr;
        for (int i = 0; i < row; ++i) {
            // p = r + beta * p
            p[i] = newr[i] + beta * p[i];
            r[i] = newr[i];
        }
    }
    //




    for (int i = 0; i < 20; ++i)
        printf("x[%d] = %f\n", i, x[i]);
    double endtime = clock();
    cout << "program is running as serial"<<endl;
    cout << "program time:" << (endtime - stratime)/ CLOCKS_PER_SEC << "s\n";

    return 0;
}
