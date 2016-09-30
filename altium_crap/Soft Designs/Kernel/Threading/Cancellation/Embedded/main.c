/*****************************************************************************
|*  SwPlatform Cancellation Example
|*
|* Devices:
|*  - LEDs (WB_PRTIO)
|*  - Terminal
|*
|* Services used:
|*  - MULTITHREADING
|*
|* Description:
|*  Threads can be cancelled with pthread_cancel().
|*  In this example, three threads are created and then cancelled from the
|*  main() thread. Upon creation, these threads set their cancellation mode
|*  and proceed to install cleanup handlers.
|*  Every thread can be cancelled at any moment. Cleanup handlers are then
|*  useful to make sure that the threads free resources prior to their cancellation.
|*
|*  The program shows:
|*  - how these handlers are called upon cancellation.
|*  - how you can create threads with their stack allocated
|*    statically. This is a usual practice in real-time systems since it
|*    favours thread creation speed.
|*  - how to create threads with a priority other than default.
\*****************************************************************************/

#include <timing.h>
#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>
#include <devices.h>
#include <stdio.h>


#define NTHREADS                                                        3

char                    thread_stack[NTHREADS][PTHREAD_STACK_MIN*2];
pthread_t               thread_id[NTHREADS];

void *pdeferredloop( void *arg);
void *pasynchronous( void *arg);
void *pdeferrednano( void *arg);
void *(*pentries[NTHREADS])(void *) = { pdeferredloop, pasynchronous, pdeferrednano };

volatile bool cancel_inhandler[NTHREADS] = { false, false, false };
volatile bool thread_ready[NTHREADS] = { false, false, false };

static pthread_mutex_t print_mutex = PTHREAD_MUTEX_INITIALIZER;
inline void print_lock(void)	{ pthread_mutex_lock(&print_mutex); }
inline void print_unlock(void)	{ pthread_mutex_unlock(&print_mutex);	}

void cancel_handler( void * arg )
{
    cancel_inhandler[(int)arg] = true;
    return;
}

void function(void)
{

    pthread_cleanup_push( cancel_handler, (void*)1 );
    for(;;)
    {
        sched_yield();
        pthread_testcancel();
    }
    pthread_cleanup_pop( 0 );
}


void *pdeferredloop( void *arg)
{
    int retval = 1;
    pthread_setcanceltype( PTHREAD_CANCEL_DEFERRED, NULL );
    pthread_cleanup_push( cancel_handler, (void*)0 );
    thread_ready[0] = true;
    print_lock();
    printf("thread1: DEFERRED loop testing cancellation ..\n");
    print_unlock();
    function();
    pthread_cleanup_pop( 0 );
    return( (void *)retval );
}


void *pasynchronous( void *arg)
{
    int retval = 1;
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );
    pthread_cleanup_push( cancel_handler, (void*)3 );
    thread_ready[1] = true;
    print_lock();
    printf("thread2: waiting ASYNCHRONOUS cancellation ..\n");
    print_unlock();
    for(;;) sched_yield();
    pthread_cleanup_pop( 0 );
    return( (void *)retval );
}


void *pdeferrednano( void *arg)
{
    int retval = 1;
    struct timespec spec = { 10,0 };
    pthread_setcanceltype( PTHREAD_CANCEL_DEFERRED, NULL );
    thread_ready[2] = true;
    print_lock();
    printf("thread3: DEFERRED cancellation while waiting in 'nanosleep'..\n");
    print_unlock();
    nanosleep( &spec,NULL );
    return( (void *)retval );
}

int main(int argc, char* argv[])
{
    int i, j;
    void *retval[NTHREADS];
    (void)argc;(void)argv;

    printf("-------------------------------------\n");

    // Create test threads
    printf("Creating %d threads ... \n", NTHREADS);
    for( i = 0; i < NTHREADS; i++ )
    {
        /* build i-th 'thread' */
        struct sched_param schedparam;
        schedparam.sched_priority = POSIX_THREADS_MAIN_PRIORITY; // all same prio
        pthread_attr_t attr;
        pthread_attr_init( &attr );
        pthread_attr_setschedparam( &attr, &schedparam );
        pthread_attr_setstackaddr( &attr, (void *)&thread_stack[i][0] );
        pthread_attr_setstacksize( &attr, sizeof(thread_stack[i]) );
        if ( pthread_create( &thread_id[i],
                              &attr,
                              pentries[i],
                              (void *)(i)) != 0 ){
            return -1;
        }
    }

    // Let the threads get going  - no concurrency problems
    for ( i = 0; i < NTHREADS ; i++ ) {
        while ( thread_ready[i] == false )
            sched_yield();
    }

    // Now wait a bit to be sure that the other threads have reached
    // their cancellation points.
    for ( j = 0; j < 20 ; j++ )
        sched_yield();

    // Let's cancel them
    print_lock();
    printf("Cancelling threads ... \n");
    print_unlock();
    for( i = 0; i < NTHREADS; i++ )
    {
        if ( pthread_cancel( thread_id[i] ) != 0 )
            return -1;

    }

    // And join results
    for( i = 0; i < NTHREADS; i++ )
    {
        if ( pthread_join( thread_id[i], &retval[i] ) != 0 )
            return -1;
    }
    printf("All threads joined ... \n");

    // check retvals
    for( i = 0; i < NTHREADS; i++ )
    {
        if ( retval[i] != PTHREAD_CANCELED )
            return -1;
    }
    printf("All threads successfully cancelled ... \n");
    printf("-------------------------------------\n");
    return 0;
}


