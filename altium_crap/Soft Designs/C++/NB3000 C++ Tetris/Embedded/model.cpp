/*****************************************************************************
|*  SwPlatform Tetris Example
|*
|* Devices:
|*  - TFT screen (VGA32_TFT)
|*  - UART8 serial device (WB_UART8 )
|*
|* Services used:
|*  - Posix_devio (serial, keyboard)
|*  - Multithreading
|*  - Graphics
|*  - Signalling
|*  - Message Queues
|*
|* Description:
|*      This serial-graphics Tetris example is a showcase for many of the services
|*      offered to designers of multithreading applications.
|*
\*****************************************************************************/

#include <stdio.h>
#include <signal.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <mqueue.h>

#include "tetris.h"
#include "pieces.h"

TetrisThread::TetrisThread() : ThreadBase()
{
    _initialized = false;
}

void TetrisThread::Setup()
{
    struct sched_param schedparam;

    // base setup function
    ThreadBase::Setup();

    // initialize pthread attributes
    pthread_attr_init(& _attr);
    pthread_attr_setinheritsched(& _attr, PTHREAD_EXPLICIT_SCHED);

    // initialize scheduling priority
    schedparam.sched_priority = TETRIS_THREAD_PRIORITY;
    pthread_attr_setschedparam(& _attr, & schedparam);

    // initialize thread stack
    pthread_attr_setstackaddr(& _attr, (void *) & _stack[0]);
    pthread_attr_setstacksize(& _attr, sizeof(_stack));
}

void * TetrisThread::Execute(void * arg)
{
    TetrisGame * theGame = (TetrisGame *) arg;

    if (!_initialized)
    {
        theGame->Initialize();
        _initialized = true;
    }

    theGame->Play();

    return NULL;
}
