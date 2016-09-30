// Update info thread

#include <stdio.h>
#include <time.h>

#include "tetris.h"

UpdateInfoThread::UpdateInfoThread() : ThreadBase()
{
    // nothing to do here
}

void UpdateInfoThread::Setup()
{
    struct sched_param schedparam;

    // base setup function
    ThreadBase::Setup();

    // initialize pthread attributes
    pthread_attr_init(& _attr);
    pthread_attr_setinheritsched(& _attr, PTHREAD_EXPLICIT_SCHED);

    // initialize scheduling priority
    schedparam.sched_priority = UPDATE_INFO_THREAD_PRIORITY;
    pthread_attr_setschedparam(& _attr, & schedparam);

    // initialize thread stack
    pthread_attr_setstackaddr(& _attr, (void *) & _stack[0]);
    pthread_attr_setstacksize(& _attr, sizeof(_stack));
}

void * UpdateInfoThread::Execute(void * arg)
{
    TetrisGame      * theGame = (TetrisGame *) arg;
    DisplayVGA      * display = theGame->GetDisplay();
    volatile int      forever = 1;
    struct timespec   ts = {
        1, 0
    };

    while (forever)
    {
        display->DisplayInfo();
        nanosleep(& ts, NULL);
        display->DisplayInfo();
    }

    return NULL;
}

const pthread_attr_t * UpdateInfoThread::Attr() const
{
    return & _attr;
}
