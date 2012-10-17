
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/uio.h>
#include <string.h>
#include <pthread.h>
/* prototypes */
int parseint(const char *arg, int *num);
void* multiply_values(void* index);
void* display_result(void* ptr);

/* shared variable array to keep M-N results*/
int c[4];
int arrA[4];
int arrB[4];
int doneCount = 0 ;
/*locks to access result array c */
pthread_mutex_t locks[] = {PTHREAD_MUTEX_INITIALIZER,PTHREAD_MUTEX_INITIALIZER,PTHREAD_MUTEX_INITIALIZER,PTHREAD_MUTEX_INITIALIZER};
/* main */
int main(int argc, char *argv[])
{
    /* numbers */
    int i, j;
    int m0index[2];
    int m1index[2];
    int m2index[2];
    int m3index[2];
    int* index[4] = {m0index,m1index,m2index,m3index};
    int* pArr ;
    /* threads */
    pthread_t m0,m1,m2,m3,display;
    pthread_t* thrdarr[] = {&m0,&m1,&m2,&m3};
    /* locks */
    /* check command line arguments */
    if (argc != 9) {
        fprintf(stderr, "Invalid number of input arguments.\n");
        return 1;
    }

    /* parse arguments */
    for (i=0; i<4; i++)
        if (!parseint(argv[i+1], &arrA[i])) {
            fprintf(stderr, "Invalid type of input: %s\n", argv[i+1]);
            return 1;
        }
    for (i=0; i<4; i++)
        if (!parseint(argv[i+5], &arrB[i])) {
            fprintf(stderr, "Invalid type of input: %s\n", argv[i+5]);
            return 1;
        }
    for(i=0;i<4;i++){
      fprintf(stdout,"ArrA[%d]:%d , ArrB[%d]:%d \n",i,arrA[i],i,arrB[i]);
    }
    for(i=0;i<4;i++){
       pArr = index[i];
       pArr[0] = i / 2 ;
       pArr[1] = i % 2 ; 
       /*fprintf(stdout, "pArr[0]%d pArr[1] %d\n",pArr[0],pArr[1]); */
       pthread_create(thrdarr[i],NULL,(void*) &multiply_values,(void*) pArr);
    }
    pthread_create(&display,NULL,(void*) &display_result,NULL);
    for(i=0;i<4;i++){
      pthread_join(*thrdarr[i],NULL);
    }
    pthread_join(display,NULL);

    /* exit */
    printf("%d: All Done.\n", getpid());
    return 0;
}



/* parse an argument as integer, return 1 if successful or 0 if the input is incorrect */
int parseint(const char *arg, int *num)
{
    char *endptr;
    *num = strtol(arg, &endptr, 10);

    /* error converting the number */
    if (endptr != NULL && *endptr != '\0')
        return 0;

    /* success */
    else
        return 1;
}


/* multiply/add 4 values then send the result through the pipe */
void* multiply_values(void* targ)
{
    
    int* index = (int*) targ ;
    int row = index[0];
    int col = index[1] ;

    fprintf(stdout, "Inside thread with tid=%lu row:%d col:%d\n",(unsigned long)pthread_self(),row,col);
    /*
    * C(i,j) = sum( A(i,*)xB(*,j)  
    */
    int r0 = arrA[2*row+0] ; /* row major calculations */
    int r1 = arrA[2*row+1] ; 
    int c0 = arrB[0+col] ; /* row major calculations */
    int c1 = arrB[2*1+col] ;
    int result = r0*c0+r1*c1;
    int resultindex = 2*row+col ;
    fprintf(stdout, "Result (r0,%d) (r1,%d) (c0,%d) (c1,%d) result:%d\n",r0,r1,c0,c1,result);
    pthread_mutex_lock( &locks[resultindex] );
    c[2*row+col] = result ; 
    /*fprintf(stdout, "Writing c[%d] = result:%d\n",resultindex,result);*/
    pthread_mutex_unlock( &locks[resultindex] );
    fprintf(stdout,"Exiting thread %lu\n",(unsigned long)pthread_self());
    doneCount++;
    pthread_exit(0);
}


/* wait for partial results and display the final result */
void* display_result(void* ptr)
{

    /* print the results */
    fprintf(stdout, "Inside display thread with tid=%lu \n",(unsigned long)pthread_self());
    printf("\n");
    int i = 0,col,row,index ; 
    while( doneCount < 4 );
    for( ; i < 4 ; i++ ){
       row = i / 2 ;
       col = i % 2 ; 
       index = 2*row+col ;
      pthread_mutex_lock( &locks[index] );
      int result = c[2*row+col] ; 
      pthread_mutex_unlock( &locks[index] );
      fprintf(stdout, "c[%d][%d] = result:%d\n",row,col,result);
    }
    printf("\n");
    fprintf(stdout,"Exiting thread %lu\n",(unsigned long)pthread_self());
    pthread_exit(0);
}
