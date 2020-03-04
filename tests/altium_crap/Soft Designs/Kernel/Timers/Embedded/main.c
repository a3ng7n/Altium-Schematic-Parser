/*****************************************************************************
|*  SwPlatform Software Timers Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - timers/timing services
|*
|* Description:
|*   This example shows how you can use periodic simple software timers.
|*   We use them here to measure time and then we compare results with
|*   standard delay/clock services.
|*
|*
\*****************************************************************************/

#include <timing.h>
#include <timers.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>


#define RUNTIMESCS 5

static bool stop    = false;
static bool start   = false;

void handler(void* context)
{
    if (start && !stop)
    {
        (*(int*)context)++;
    }
    
}


int main(int argc, char* argv[])
{
    uint64_t        mark,delay;
    int             miliseconds  = 0;
    void*           timer;

    printf("-------------------------------------\n");
    printf("Install software timer and delay %d scs\n",RUNTIMESCS);
    timer         = timer_register_handler((void*)&miliseconds, 1000L,  handler);
    if (timer == NULL ){
       return -1;
    }

    mark          = clock_ms();
    start         = true;
    delay_ms( RUNTIMESCS*1000L );
    delay         = clock_ms();
    stop          = true;
    if (timer_deregister_handler(timer) != true){
       return -1;
    }

    printf("time -delay()/clock_ms()- (ms) : %d \n", (int)(delay-mark));
    printf("time -software timers   - (ms) : %d \n", miliseconds);
    printf("-------------------------------------\n");

    return 0;
}


