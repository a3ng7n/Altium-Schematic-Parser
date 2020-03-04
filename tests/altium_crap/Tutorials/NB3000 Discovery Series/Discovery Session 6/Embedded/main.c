#include <stddef.h>
#include <drv_led.h>
#include "led_info.h"
#include <timers.h>
#include <devices.h>

#include <drv_instrument.h>
#include "instruments.h"

#define PRE_SCAN_VALUE  100
#define SCAN_ARRAY_SIZE 7

timer_handler_t TimerTick (void * context);
void UpdateKnightRiderLEDs (void);
void init(void);

led_t * ptrLEDs;
instrument_t * ctrl_spd;
unsigned char ScanArray[LEDS_NUM_LED_IDS];
unsigned char inst_delay = 255;
volatile unsigned char Tick = 0;

void main(void)
{
    init();
    led_turn_on(ptrLEDs, 0);
    ctrl_spd = instrument_open(CUSTOM_INSTRUMENT_1);
    while (1)
    {
        if (Tick)
        {
            if (inst_delay++ == 0)
            {
                /*
                 * Read Custom Instrument Slider Value and subtract it from
                 * delay time (higher slider = less delay = faster chasing).
                 */
                inst_delay = instrument_get_value( ctrl_spd,
                                                   CUSTOM_INSTRUMENT_1_DELAY);
                UpdateKnightRiderLEDs();
            }
            Tick = 0;
        }
    }
}

void init(void)
{
    // Connect to LED hardware
    ptrLEDs = led_open(LEDS);
    // Set timer callback function
    timer_register_handler(0, 1000L, (void*)TimerTick);
}

timer_handler_t TimerTick(void * context)
{
    Tick = 1; //indicate to mainline that the timer tick has occurred.
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
