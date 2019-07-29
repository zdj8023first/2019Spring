#include <stdlib.h>
#include <GL/glut.h>
#include <omp.h>
#include <time.h>
#include <stdlib.h>
#include <iostream>


using namespace std;
#define number_threads 8
/* Defaut data via command line */
/* Can enter other values via command line arguments */

#define CENTERX -0.5
#define CENTERY 0.5
#define HEIGHT 0.5
#define WIDTH 0.5
#define max_new_ITER 100
/* drand48 */
#define drand48_m 0x100000000LL  
#define drand48_c 0xB16  
#define drand48_a 0x5DEECE66DLL  
  
static unsigned long long drand48_seed = 1; 

/* N x M array to be generated */

#define N 500
#define M 500

float height = HEIGHT; /* size of window in complex plane */
float width = WIDTH;
float cx = CENTERX; /* center of window in complex plane */
float cy = CENTERY; 
int max_new = max_new_ITER; /* number of interations per point */

int n=N;
int m=M;

/* Use unsigned bytes for image */

GLubyte image[N][M];

/* Complex data type and complex add, mult, and magnitude functions */
/* Probably not worth overhead */

typedef float complex[2];

void add(complex a, complex b, complex p)
{
    p[0]=a[0]+b[0];
    p[1]=a[1]+b[1];
}

void mult(complex a, complex b, complex p)
{
    p[0]=a[0]*b[0]-a[1]*b[1];
    p[1]=a[0]*b[1]+a[1]*b[0];
}

float mag2(complex a)
{
    return(a[0]*a[0]+a[1]*a[1]);
}

void form(float a, float b, complex p)
{
    p[0]=a;
    p[1]=b;
}

void display()
{
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawPixels(n,m,GL_COLOR_INDEX, GL_UNSIGNED_BYTE, image);
	glFlush();
}


void myReshape(int w, int h)
{
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    if (w <= h)
    gluOrtho2D(0.0, 0.0, (GLfloat) n, (GLfloat) m* (GLfloat) h / (GLfloat) w);
    else
    gluOrtho2D(0.0, 0.0, (GLfloat) n * (GLfloat) w / (GLfloat) h,(GLfloat) m);
    glMatrixMode(GL_MODELVIEW);
    display();
}
double drand48(void)
{
	drand48_seed = (drand48_a * drand48_seed + drand48_c) & 0xFFFFFFFFFFFFLL;  
    unsigned int x = drand48_seed >> 16;  
    return  ((double)x / (double)drand48_m);  

}
void myinit()
{
    float redmap[256], greenmap[256],bluemap[256];
    int i;

	glClearColor (1.0f, 1.0f, 1.0f, 0.0f);
    gluOrtho2D(0.0, 0.0, (GLfloat) n, (GLfloat) m);

	/* Define pseudocolor maps, ramps for red and blue,
	random for green */

	#pragma omp parallel for num_threads(number_threads)
    for(i=0;i<256;i++) 
    {
         redmap[i]=i/255.;
         greenmap[i]=drand48();
         bluemap[i]=1.0-i/255.;
    }
		glPixelMapfv(GL_PIXEL_MAP_I_TO_R, 256, redmap);
		glPixelMapfv(GL_PIXEL_MAP_I_TO_G, 256, greenmap);
		glPixelMapfv(GL_PIXEL_MAP_I_TO_B, 256, bluemap); 
}


int main(int argc, char *argv[])
{
	double start = omp_get_wtime( );
    if(argc>1) cx = atof(argv[1]); /* center x */
    if(argc>2) cy = atof(argv[2]);  /* center y */
    if(argc>3) height=width=atof(argv[3]); /* rectangle height and width */
    if(argc>4) max_new=atoi(argv[4]); /* maximum iterations */
	#pragma omp parallel for num_threads(number_threads) 	
    for (int i=0; i<n; i++) 
		#pragma omp parallel for num_threads(number_threads)
		for(int j=0; j<m; j++) 
    {

	/* starting point */
		complex c0, c, d;
		float x= i *(width/(n-1)) + cx -width/2;
		float y= j *(height/(m-1)) + cy -height/2;

		form(0,0,c);
		form(x,y,c0);
		
			float v;
			for(int k=0; k<max_new; k++)
			{
				mult(c,c,d);
				add(d,c0,c);
				v=mag2(c);
				if(v>4.0) break;  //assume not in set if mag > 4 
			}
		/* assign gray level to point based on its magnitude */
			if(v>1.0) v=1.0; /* clamp if > 1 */
			image[i][j]=255*v;
    }

    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB );
    glutInitWindowSize(N, M);
    glutCreateWindow("mandlebrot");
    myinit();
    glutReshapeFunc(myReshape);
    glutDisplayFunc(display);
	double end = omp_get_wtime( ); 
	cout<<"use_time"<<1000*(end -start)<<"ms \n"; 
    glutMainLoop();
	return 0;

}

