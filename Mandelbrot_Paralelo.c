#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <GL/glut.h>
#include <sys/timeb.h>
#include <pthread.h>    /* POSIX Threads */

//gcc Mandelbrot_Paralelo.c -o mandelbrot_paralelo_c -lm -lGL -lglut -lGLU -pthread
//./mandelbrot_paralelo_c  1 2048

struct Pixel
{
	double width;
	double height;
	int cor;
} ;

typedef struct str_thdata
{
    int thread_no;
    double complex *planoComplexoParticionado;
    int size;
     
} thdata;

// função que realiza a iteração para definir o ponto mandelbrot e sua respectiva cor
struct Pixel IteracaoPontoMandelbrot(double complex ponto,int numeroDeIteracoes)
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


void Mandelbrot (void *ptr)
{
    thdata *data;            
    data = (thdata *) ptr;  /* type cast to a pointer to thdata */
    int i;
    struct Pixel *pixeis= malloc(data->size*sizeof(struct Pixel ));
    for (i=0;i< data->size;i++)
	{
	pixeis[i]=IteracaoPontoMandelbrot(data->planoComplexoParticionado[i],50);
    }
    pthread_exit((void*)pixeis); /* exit */	
}

int main(int argc,char** argv)
{
	double imageHeigth,imageWidth,numeroDeIteracoes;
	int numThreads,tela;
	if(argc == 3)
	{
		tela = atoi(argv[2]);
		numThreads = atoi(argv[1]);
		
	}
	else 
	{
		tela = 512;
		numThreads = 2;
	}
	
	imageHeigth = 4;
	imageWidth = 4;
	numeroDeIteracoes = 50;	

	double passo = imageHeigth/tela;
	double valorx,valory;
	double teste ;
	int x,y,i,j,z,indice;
	struct timeb start, end;

	z=0;
	x = floor((imageHeigth/passo)+1);
	y = floor((imageWidth/passo)+1);
	pthread_t threads[numThreads];  /* thread variables */
    	thdata data[numThreads];         /* structs to be passed to threads */
	
	double complex* planoComplexo =malloc(x*y*sizeof(double complex ));
	double complex* planoComplexoReconstruido[numThreads];
	struct Pixel* pixeis; 	
	struct Pixel* pixeisReconstruidos=malloc(x*y*sizeof(struct Pixel ));
	long seconds;
	int militm;

	valory=-imageHeigth/2;
	
	ftime(&start);
	// laço que varre todos os pontos do plano, cria e armazena o numero complexo na matriz plano complexo
	
	for (j=0;j<y;j++)
	{
		
		valorx=-imageWidth/2;
		for (i=0;i<x;i++)
		{
			planoComplexo[z]=valorx+valory*I;
			valorx=valorx+passo;		
			//printf("%f + i%f\n", creal(planoComplexo[z]), cimag(planoComplexo[z]));
			z++;
		}
		valory=valory+passo;
	}
	

	for (i=0;i<numThreads;i++)
	{
		data[i].thread_no = i;
		data[i].planoComplexoParticionado = malloc(((z/numThreads)+1)*sizeof(double complex));
		indice = 0;	
	
		for (j=i;j<z;j=j+numThreads)// i = numero da thread,z = numero de pontos,j+numThreads=deslocamento
		{	
			data[i].planoComplexoParticionado[indice] =planoComplexo[j];
			indice++;
		} 	

		data[i].size = indice; 
		pthread_create (&threads[i], NULL, (void *) &Mandelbrot, (void *) &data[i]);
	}
	indice = 0;	

	for (i=0;i<numThreads;i++)	
	{
		pthread_join(threads[i], (void**)&pixeis);
		for (j=0;j<data[i].size;j++)		
		{
			pixeisReconstruidos[indice]=pixeis[j];
			indice++;
		}
	}

	ftime(&end);

	seconds = (long)(end.time - start.time);
	militm = end.millitm - start.millitm;
	if (0 > militm) {militm += 1000;seconds--; }
	
	int totalThreads =numThreads;
	int numPontos = tela*tela;
	printf("%d, %d ,Linha RoundRobin ,  %ld.%03d s\n",totalThreads,numPontos, seconds, militm);
	//pixeis=verificaPlanoComplexo(x,y,planoComplexo,numeroDeIteracoes);
	
	//funçoes opengl para desenhar o fractal de mandelbrot
	
	/*glutInit(&argc,argv);
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
	
	int total= 0;
	for (j=0;j<y;j++)
	{
		for (i=0;i<x;i++)
		{
			cor= (3.0/numeroCores) * ((double)(pixeisReconstruidos[total].cor%numeroCores));
			if (cor>2)
				glColor3f(1.0f, 1.0f, 3.0-cor);	
			else if (cor<=2 && cor>1)
				glColor3f(1.0f, 2.0-cor, 0);	
			else    glColor3f(cor, 0, 0);	
			glVertex3f(pixeisReconstruidos[total].width-passo,pixeisReconstruidos[total].height+passo, 0.0);
			glVertex3f(pixeisReconstruidos[total].width+passo,pixeisReconstruidos[total].height+passo, 0.0);
         		glVertex3f(pixeisReconstruidos[total].width+passo,pixeisReconstruidos[total].height-passo, 0.0);
         		glVertex3f(pixeisReconstruidos[total].width-passo,pixeisReconstruidos[total].height-passo, 0.0);  
			total++;        
		}
	}
	glEnd();
	glFlush();
	glutMainLoop();
	*/
	return 0;
}
