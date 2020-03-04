/*****************************************************************************
|*  SwPlatform Signal Timers Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - Signalling
|*  - Multithreading
|*
|* Description:
|*  The code shows how to use posix timers to throw signals to the open
|*  process and how threads can wait on them with sigwait().
|*  The main() thread sets a periodic timer that signals SIGALRM periodically.
|*  A second thread loops in a sigwait call waiting for this signal.
|*  After REPS iterations the thread exists the loop and program ends. 
|*
\*****************************************************************************/

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <pthread.h>
#include <sched.h>

#define PERIODSCS           3
#define REPS                5

void* thread(void* arg)
{
    sigset_t set;
    int      sig,n=0;

    sigemptyset (&set);
    sigaddset   (&set, SIGALRM);
    sigprocmask (SIG_BLOCK, &set, NULL);

    printf("thread: sigwaits for SIGALRM %d times.\n",REPS);
    while( n++ < REPS )
    {
        sigwait(&set, &sig);
        printf("thread catches SIGALRM\n");
    }
    printf("thread: exits\n");

    return NULL;
}


int main(int argc, char* argv[])
{
    struct sigevent ev;
    timer_t tid;
    pthread_t id;
    struct itimerspec its;
    (void)argc;(void)argv;

    printf("-----------------------------------------\n");

    ev.sigev_notify         = SIGEV_SIGNAL;
    ev.sigev_signo          = SIGALRM;
    its.it_interval.tv_sec  = PERIODSCS;
    its.it_interval.tv_nsec = 0;
    its.it_value.tv_sec     = PERIODSCS;
    its.it_value.tv_nsec    = 0;

    printf("timer sends SIGALRM every %d scs.\n",PERIODSCS);
    timer_create(CLOCK_REALTIME, &ev, &tid);
    timer_settime(tid, 0, &its, NULL);
    printf("main: creates and joins thread\n");
    pthread_create(&id,NULL,thread,NULL);
    pthread_join( id, NULL );

    timer_delete(tid);
    printf("main: exits\n");
    printf("-----------------------------------------\n");

    return 0;
}




