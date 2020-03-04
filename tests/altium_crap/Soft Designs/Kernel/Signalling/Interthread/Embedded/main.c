/*****************************************************************************
|*  SwPlatform Signal Threads Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - Signalling
|*  - Multithreading
|*
|* Description:
|*  This example shows some signalling functionality between threads.
|*  In particular how:
|*  - threads can mask/catch signals
|*  - how to send a signal to a one only thread
|*  - how to send a signal to the open process
|*  - how nanosleep is interrupted by a signal
|*  - how a thread waits on signals (other options are sigwait,..)
|*
\*****************************************************************************/

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>

static int handler0 = 0;

void handlercode0( int sig )
{
    handler0 = sig;
    return;
}

void * thread0( void * arg )
{
    struct timespec ts = { 1, 0 };
    while ( !handler0  )
    {
        printf("thread0: nanosleep 1 sc.\n");
        if ( nanosleep(&ts,NULL) == -1  )
        {
            printf("thread0: nanosleep interrupted by signal\n");
        }
    }
    printf("thread0: exits\n");
    return NULL;
}



static int SIGUSR1_called = 0;
static int SIGUSR2_called = 0;
static int exit_status    = 2;

void handlercode1(int signo)
{
    if (signo == SIGUSR1) {
        SIGUSR1_called = 1;
        if (SIGUSR2_called == 1) {
            exit_status = 1;
        }
    }
    else if (signo == SIGUSR2) {
        SIGUSR2_called = 1;
        if (SIGUSR1_called == 1)
            exit_status = 0;
        else
            exit_status = 1;
    }
}

void* thread1(void* arg)
{
    sigset_t tempmask, originalmask;
    struct timespec ts = { 1, 0};
    struct sigaction act;

    printf("thread1: nanosleep 1 sc.\n");

    act.sa_handler = handlercode1;
    act.sa_flags=0;
    sigemptyset(&act.sa_mask);

    sigemptyset(&tempmask);
    sigaddset(&tempmask, SIGUSR2);

    sigaction(SIGUSR1,  &act, 0);
    sigaction(SIGUSR2,  &act, 0);

    sigemptyset(&originalmask);
    sigaddset(&originalmask, SIGUSR1);
    printf("thread1: sigprocmask: SIGUSR1 is original mask  0x%016llx\n", originalmask);
    sigprocmask(SIG_SETMASK, &originalmask, NULL);

    printf("thread1: sigsuspend() SIGUSR2 is temporary mask 0x%016llx\n", tempmask);
    if (sigsuspend(&tempmask) != -1)
    {
        exit(-1);
    }

    if ( exit_status != 0 )
    {
        exit(-1);
    }

    printf("thread1 catches SIGUSR1 within sigsuspend\n");
    printf("thread1 catches SIGUSR2 after sigsuspend\n");
    printf("thread1: exits\n");

     pthread_exit((void*)exit_status);

    return (void*)0;
}



int main(int argc, char* argv[])
{
    int ret;
    struct sched_param schedparam;
    pthread_attr_t attr;
    pthread_t id0,id1;
    struct sigaction sa;
    sigset_t    set;
    (void)argc;(void)argv;

    printf("-----------------------------------------\n");

    /* Set the signal handler */
    sa.sa_flags = 0;
    sa.sa_handler = handlercode0;
    ret = sigemptyset( &sa.sa_mask );
    sigaction( SIGUSR2, &sa, 0 );

    schedparam.sched_priority = POSIX_THREADS_MAIN_PRIORITY + 1;
    pthread_attr_init( &attr );
    pthread_attr_setschedparam( &attr, &schedparam );
    pthread_attr_setinheritsched( &attr, PTHREAD_EXPLICIT_SCHED );
    printf("main: Create thread 0\n");
    pthread_create( &id0, &attr, thread0, NULL );
    printf("main: send SIGUSR2 to thread0 with pthread_kill()\n");
    pthread_kill( id0, SIGUSR2 );

    printf("-----------------------------------------\n");
    printf("main: Create thread 1\n");
    pthread_create( &id1, &attr, thread1, NULL );

    sigfillset(&set);
    sigprocmask(SIG_SETMASK, &set, NULL);

    printf("main: sends SIGUSR2 signal with kill()\n");
    kill (0, SIGUSR2);
    if (SIGUSR2_called == 1) {
        return -1;
    }
    printf("main: sends SIGUSR1 signal with kill()\n");
    kill (0, SIGUSR1);

    printf("main exits\n");
    printf("-----------------------------------------\n");

    return 0;
}
