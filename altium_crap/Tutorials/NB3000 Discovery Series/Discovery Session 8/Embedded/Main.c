#include <stdio.h>
#include <drv_led.h>
#include <timers.h>
#include <devices.h>
#include <stddef.h>
#include <stdint.h>
#include <drv_ir38k.h>

#include "led_info.h"
#include "devices.h"

#define PRE_SCAN_VALUE  100
#define SCAN_ARRAY_SIZE 7
#define MAX_SPEED       250
#define MIN_SPEED       50
#define SPD_INCR        4

timer_handler_t TimerTick(void * context);
void UpdateKnightRiderLEDs(void);
void init(void);

led_t * ptrLEDs;
ir38k_t * ir38k;
unsigned char ScanArray[LEDS_NUM_LED_IDS];
unsigned char inst_delay = 255;
volatile unsigned char Tick = 0;
uint16_t address;
uint16_t command, prev_command;
int      rec_cmd = 1;
int      ret;
int      ercnt = 0;
uint8_t  speed = 200;

void main( void )
{
    init();

    // Initialize NB Starterkit infrared
    ir38k = ir38k_open(DRV_IR38K_1);

    led_turn_on(ptrLEDs, 0);
    printf("Press the up or down buttons on the remote control...\n");

    while (1)
    {
        prev_command = command;
        ret = ir38k_rxnecdata(ir38k, & address, & command);
        if (ir38k_getbiterror(ir38k) > ercnt)
        {
            ercnt = ir38k_getbiterror(ir38k);
        }
        if (ercnt > 5)
        {
            ercnt = 0;
            ir38k_rstbiterror(ir38k); // reset the error counter
        }
        switch (ret)
        {
            case 1: // repeat data
            case 0: // normal data
                switch (prev_command)
                {
                    case 63750: // Up Arrow on NB3000 remote
                         if (speed < MAX_SPEED) { speed += SPD_INCR; }
                         printf("\rCommand: UP   Speed: %d", speed);
                         break;
                    case 47940: // Dn Arrow on NB3000 remote
                         if (speed > MIN_SPEED) { speed -= SPD_INCR; }
                         printf("\rCommand: DOWN Speed: %d", speed);
                         break;
                    default:
                         break;
                }
                break;
            case - 1: // nothing received
                break;
            case - 2: // Buffer overrun
                printf("Receive buffer overrun.\n");
                break;
            default:
                ; // do nothing
        }
        printf("Address = %d, Command = %d \n", address, command);
    }
}

void init(void)
{
    ptrLEDs = led_open(LEDS);
    timer_register_handler(0, 1000L, TimerTick);
}

timer_handler_t TimerTick(void * context)
{
    Tick--; //indicate to mainline that the timer tick has occurred.
    if (!Tick)
    {
        UpdateKnightRiderLEDs();
        Tick = 255 - speed;
    }
    return NULL;
}

void UpdateKnightRiderLEDs(void)
{
    static unsigned char ScanIndex = 0;
    static unsigned char LEDScanIndexModifier = 1;

    for (unsigned char i = 0; i < LEDS_NUM_LED_IDS; i++)
    {
        if (ScanArray[i] == PRE_SCAN_VALUE)
            ScanArray[i] = 255;
        else
            ScanArray[i] = ScanArray[i] >> 1;
    }

    ScanArray[ScanIndex] = PRE_SCAN_VALUE;
    ScanIndex += LEDScanIndexModifier;

    if ((ScanIndex == (LEDS_NUM_LED_IDS - 1)) || (ScanIndex == 0))
        LEDScanIndexModifier *= - 1;

    for (unsigned char i = 0; i < LEDS_NUM_LED_IDS; i++)
    {
        led_set_intensity(ptrLEDs, i, ScanArray[i]);
    }
}
