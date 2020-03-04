// Logger thread

#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <mqueue.h>
#include <devctl.h>

#include "tetris.h"

LoggerThread::LoggerThread() : ThreadBase()
{
    // nothing to do here
}

void LoggerThread::Setup()
{
    struct sched_param schedparam;

    // base setup function
    ThreadBase::Setup();

    // initialize pthread attributes
    pthread_attr_init(& _attr);
    pthread_attr_setinheritsched(& _attr, PTHREAD_EXPLICIT_SCHED);

    // initialize scheduling priority
    schedparam.sched_priority = LOGGER_THREAD_PRIORITY;
    pthread_attr_setschedparam(& _attr, & schedparam);

    // initialize thread stack
    pthread_attr_setstackaddr(& _attr, (void *) & _stack[0]);
    pthread_attr_setstacksize(& _attr, sizeof(_stack));
}

void * LoggerThread::Execute(void * arg)
{
    TetrisGame   * theGame = (TetrisGame *) arg;
    DisplayVGA   * display = theGame->GetDisplay();
    mqd_t          mq = theGame->GetReceiveQueue();
    unsigned int   priority;
    char           buf[MSG_MAXSIZE];
    volatile int   forever = 1;

    while (forever)
    {
        // read next log
        mq_receive(mq, buf, MSG_MAXSIZE, & priority);

        if (priority == MSG_EXIT)
        {
            // display_end_game
            display->DisplayEndGame();

            // and cancel other threads
            theGame->GetInputThread().Cancel();
            theGame->GetTetrisThread().Cancel();

            pthread_exit(NULL);
        }
    }

    return NULL;
}
