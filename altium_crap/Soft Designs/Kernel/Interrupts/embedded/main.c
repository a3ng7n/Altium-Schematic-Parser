/*****************************************************************************
|*  SwPlatform Interrupts Example
|*
|* Devices:
|*  - Terminal
|*
|* Services used:
|*  - timers/timing services
|*  - interrupts
|*
|* Description:
|*   This example shows how you can use interrupt services in your
|*   application code to register native handlers interrupt lines.
|*
|*   An external internal source (INT1) has been hooked up to a timer
|*   source that generates an interrupt every 10 usecs ( 100KHz ) at schematic level.
|*   The application uses interrupt services to install a native handler on this
|*   high-frequency interrupt in order to process the evolution of time.
|*   The results are later compared to those of clock_us/delay_us
\*****************************************************************************/

#include <timing.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <interrupts.h>

#define INTNUMBER    1

static      bool stop                   = false;
static      bool start                  = false;

__INTERRUPT_NATIVE void interrupt_handler(void)
{
    int * ptr = (int*)interrupt_native_context(interrupt_get_current());
    if (start && !stop)
    {
        (*ptr)++;
    }
    interrupt_acknowledge(INTNUMBER);
}


int main(int argc, char* argv[])
{
    uint64_t        mark,delay;
    volatile int    tenuscs         = 0;
    printf("-------------------------------------\n");
    printf("Install native interrupt handler and delay 5 scs\n");

    interrupt_register_native( INTNUMBER, (void*)&tenuscs, interrupt_handler );
    interrupt_configure( INTNUMBER, EDGE_FALLING );

    mark          = clock_us();
    start         = true;
    interrupt_acknowledge(INTNUMBER);
    interrupt_enable( INTNUMBER );
    delay_us( 5*1000L*1000L );
    stop          = true;
    delay         = clock_us();

    interrupt_disable( INTNUMBER );

    printf("time -delay()/clock_us()- (us) : %d \n", (int)(delay-mark));
    printf("time -   interrupt      - (us) : %d \n", 10*tenuscs);
    printf("-------------------------------------\n");

    return 0;
}


