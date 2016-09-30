/*****************************************************************************
|*  SwPlatform Software Timers Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - timers/timing services
|*  - interrupts
|*
|* Description:
|*   This example shows how you can use interrupt services to associate threads
|*   to wait for specific interrupt events. The thread associates an isr onto an
|*   interrupt source and then waits for interrupt notification.
|*
|*   In this example we use a 1KHz direct interrupt source for interrupt
|*   generation. We use pthread_interrupt_timedwait() to make n threads
|*   go on wait for periods of 1,2,..,n scs and then compare results
|*   with standard clock_ms().
|*
|*   *Posix interrupts are used at driver level to make io requester threads
|*   sleep to only wake up after a certain i/o interrupt event has taken place.
|*   In many cases this is a most desirable situation since you don't want
|*   to spend cpu time in uneccessary nanosleep() or yield() operations.
\*****************************************************************************/

#include <timing.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <interrupts.h>
#include <pthread.h>


#define  NTHREADS           4
#define  INTNUMBER          1
#define  INTFREQ_HZ         (1000L)

static volatile int counter=0;

pthread_mutex_t mutexsync=PTHREAD_MUTEX_INITIALIZER;

uint32_t interrupt_handler ( uint32_t number, void * context )
{
    if ( number != INTNUMBER ) return INTERRUPT_NOT_HANDLED;

    interrupt_acknowledge(number);

    if ( --counter == 0 )
    {
        // notify associated thread to wake up
        return INTERRUPT_NOTIFY;
    }
        // interrupt handled : no wakeup
    return INTERRUPT_HANDLED;
}

void* thread(void* arg)
{
    uint64_t    mark,delay;
    pthread_mutex_lock(&mutexsync);
    pthread_interrupt_associate   (INTNUMBER,interrupt_handler,NULL);
    counter = INTFREQ_HZ*((int)(arg)+1);
    mark  = clock_ms();
    interrupt_acknowledge         (INTNUMBER);
    interrupt_enable              (INTNUMBER);
    pthread_interrupt_timedwait   (NULL);
    delay = clock_ms();
    pthread_interrupt_disassociate(INTNUMBER,interrupt_handler,NULL);
    interrupt_disable             (INTNUMBER);
    pthread_mutex_unlock(&mutexsync);
    return (int)(delay-mark);
}


int main(int argc, char* argv[])
{
     int i;
     int ret;
     pthread_t      ids[NTHREADS];
     void*          retval[NTHREADS];

    (void)argc;(void)argv;

    interrupt_configure     (INTNUMBER, EDGE_RISING);
    interrupt_register      (INTNUMBER,NULL,interrupt_handler);
    interrupt_set_posix     (INTNUMBER, true);
    interrupt_acknowledge   (INTNUMBER);

    // Create threads
    printf("-------------------------------------\n");
    printf("Creating %d threads\n",NTHREADS);
    printf("Threads wait (+-) 1..%d scs. for interrupt notification\n", NTHREADS);
    for( i = 0; i < NTHREADS; i++ )
    {
        ret = pthread_create( &ids[i], NULL, thread, (void *)(i));
        if ( ret != 0 ) {
            return -1;
        }
    }

    // join with threads
    printf("main joins all %d threads ... \n", NTHREADS);
    for( i = 0; i < NTHREADS; i++ )
    {
        ret = pthread_join( ids[i], &retval[i] );
        if ( ret != 0 ){
            return -1;
        }
        printf("thread %d has waited %d mscs.\n",i,(int)retval[i]);
    }

    printf("main() thread exits ... END !\n");
    printf("-------------------------------------\n");

    return 0;
}


