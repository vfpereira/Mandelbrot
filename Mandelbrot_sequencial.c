#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <GL/glut.h>
#include <sys/timeb.h>
#include <pthread.h>    /* POSIX Threads */

//gcc mandelbrot.c -o mandelbrot -lm -lGL -lglut -lGLU


struct Pixel
{
	double width;
	double height;
	int cor;
} ;

typedef struct str_thdata
{
    int thread_no;
} thdata;

// função que realiza a iteração para definir o ponto mandelbrot e sua respectiva cor
struct Pixel IteracaoMandelbrot(double complex ponto,int numeroDeIteracoes)
{
	struct Pixel pixel;
	int iterador;
	double complex pontoIterado;
	pontoIterado = ponto;
	for(iterador = numeroDeIteracoes;iterador>=0;iterador--)
	{
		if (cabs(pontoIterado)>2 )
		{
			pixel.width = creal(ponto);
			pixel.height = cimag(ponto);
			pixel.cor = iterador;
			return pixel;
		}		
		pontoIterado = cpow(pontoIterado,2)+ponto;
		
	}
}

//função que realiza o calculo de  mandelbrot para todos os numeros complexos do plano e calcula os tempos
struct Pixel** verificaPlanoComplexo(int x,int y ,double complex planoComplexo[x][y],int numeroDeIteracoes)
{	
	int i,j,z;	
	struct timeb start, end;
	long seconds;
	int militm;
	
	ftime(&start);
	
	struct Pixel **pixeis= (struct Pixel **)malloc(x*sizeof(struct Pixel ));
	for(i = 0; i < x; i++)
		pixeis[i] = malloc(y*sizeof(struct Pixel));

	ftime(&end);
	seconds = (long)(end.time - start.time);
	militm = end.millitm - start.millitm;
	if (0 > militm) {militm += 1000;seconds--; }
	printf("tempo de alocacao de memoria %ld.%03d seconds\n", seconds, militm);

	ftime(&start);
	
	// laço que realiza calculo do mandelbrot para todos os numeros complexos
	for (j=0;j<y;j++)
		for (i=0;i<x;i++)		
			pixeis[i][j]=IteracaoMandelbrot(planoComplexo[i][j],numeroDeIteracoes);	
	ftime(&end);

	seconds = (long)(end.time - start.time);
	militm = end.millitm - start.millitm;
	if (0 > militm) {militm += 1000;seconds--; }
	printf("tempo do calculo %ld.%03d seconds\n", seconds, militm);
	return pixeis;
}

int main(int argc,char** argv)
{
	double tela,imageHeigth,imageWidth,numeroDeIteracoes;
	tela = 512;
	imageHeigth = 4;
	imageWidth = 4;
	numeroDeIteracoes = 50;	

	double passo = imageHeigth/tela;
	double valorx,valory;
	double teste ;
	int x,y,i,j;

	x = floor((imageHeigth/passo)+1);
	y = floor((imageWidth/passo)+1);
	
	double complex planoComplexo[x][y];  
	struct Pixel** pixeis; 	
	
	valory=-imageHeigth/2;
	
	// laço que varre todos os pontos do plano, cria e armazena o numero complexo na matriz plano complexo
	for (j=0;j<y;j++)
	{
		
		valorx=-imageWidth/2;
		for (i=0;i<x;i++)
		{
			planoComplexo[i][j]=valorx+valory*I;
			valorx=valorx+passo;	
		}
		valory=valory+passo;
	}
	
	pthread_t threads[4]  /* thread variables */
    	thdata data[4]         /* structs to be passed to threads */
	
	for (i=1;i<4;i++)
		data[i] = i;
	
	pixeis=verificaPlanoComplexo(x,y,planoComplexo,numeroDeIteracoes);
	
	//funçoes opengl para desenhar o fractal de mandelbrot
	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
	glutInitWindowSize (512,512);
	glutInitWindowPosition(100,100);
	glutCreateWindow("Mandelbrot");
	gluOrtho2D(-2.0, 2.0, -2.0, 2.0);
	glClear (GL_COLOR_BUFFER_BIT);
	glColor3f(1.0f, 1.0f, 1.0f);
	glBegin(GL_QUADS);
	
	int numeroCores = 30.0;
	double cor;
	
	for (j=0;j<y;j++)
	{
		for (i=0;i<x;i++)
		{
			cor= (3.0/numeroCores) * ((double)(pixeis[i][j].cor%numeroCores));
			if (cor>2)
				glColor3f(1.0f, 1.0f, 3.0-cor);	
			else if (cor<=2 && cor>1)
				glColor3f(1.0f, 2.0-cor, 0);	
			else    glColor3f(cor, 0, 0);	
			glVertex3f(pixeis[i][j].width-passo,pixeis[i][j].height+passo, 0.0);
			glVertex3f(pixeis[i][j].width+passo,pixeis[i][j].height+passo, 0.0);
         		glVertex3f(pixeis[i][j].width+passo,pixeis[i][j].height-passo, 0.0);
         		glVertex3f(pixeis[i][j].width-passo,pixeis[i][j].height-passo, 0.0);  
			         
		}
	}
	glEnd();
	glFlush();
	glutMainLoop();
	return 0;
}
