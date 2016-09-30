/*****************************************************************************
|*  SwPlatform Priority Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - MULTITHREADING
|*
|* Description:
|*  Threads can be created with different priorities.
|*  In this example, many threads will be created on the heap with
|*  different priorities and they will all proceed to run in
|*  priority-based order.
\*****************************************************************************/

#include <timing.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>
#include <pthread.h>
#include <devices.h>
#include <stdio.h>


void *thread( void *arg)
{
    printf("thread %d priority %d \n",pthread_self(),(int)arg);
    delay_ms(250);
    return NULL;
}


int main(int argc, char* argv[])
{
    int prio,i;
    struct sched_param schedparam;
    pthread_attr_t attr;
    pthread_t      thread_id[SCHED_PRIORITY_MAX];

    printf("-------------------------------------\n");

    // Create priority threads
    pthread_attr_init( &attr );
    for ( i=0,prio = SCHED_PRIORITY_MIN; prio < SCHED_PRIORITY_MAX;i++  )
    {
        schedparam.sched_priority = prio;
        pthread_attr_setschedparam( &attr, &schedparam );
        pthread_attr_setinheritsched( &attr, PTHREAD_EXPLICIT_SCHED );
        if ( pthread_create( &thread_id[i],
                              &attr,
                              thread,
                              (void*)prio) != 0 ){
            break;  // no more memory
        }
        prio++;
    }
    printf("Created %d priority threads ... \n", prio-SCHED_PRIORITY_MIN);

    // join thread with lowest priority
    printf("Joining with lowest priority (%d) thread  ... \n", SCHED_PRIORITY_MIN);
    if ( pthread_join( thread_id[0], NULL ) != 0 )
            return -1;
    printf("main() thread exits ... END !! ...\n");
    printf("-------------------------------------\n");
    return 0;
}


