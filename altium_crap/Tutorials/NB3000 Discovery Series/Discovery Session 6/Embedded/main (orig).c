#include <drv_led.h>
#include <timers.h>
#include <devices.h>
#include <stddef.h>
#include "led_info.h"

#define PRE_SCAN_VALUE  100
#define SCAN_ARRAY_SIZE 7

timer_handler_t TimerTick (void* context);
void UpdateKnightRiderLEDs (void);
void init(void);

led_t* ptrLEDs;
unsigned char ScanArray[LEDS_NUM_LED_IDS];
volatile unsigned char Tick = 0;

 void main(void)
 {
   init();

    led_turn_on(ptrLEDs, 0);

   while(1)
      {
         if (Tick)
            {
               UpdateKnightRiderLEDs();
               Tick = 0;  // Clear timer flag.
            }
      }
 }

// Initialize LED driver and TSK3000A timer.
void init(void)
{
   ptrLEDs = led_open(LEDS);
   timer_register_handler(0, 20000L, TimerTick);
}

// Callback function for Timer Interrupt - sets flag.
timer_handler_t TimerTick (void* context)
{
   Tick = 1;        //indicate to mainline that the timer tick has occurred.
   return NULL;
}

// Function to shift LED brightness pattern.
void UpdateKnightRiderLEDs (void)
{
   static unsigned char ScanIndex = 0;
   static unsigned char LEDScanIndexModifier = 1;

   // Loop to set eight LEDs in linear brightness pattern
   for (unsigned char i = 0; i < LEDS_NUM_LED_IDS; i++)
   {
      if (ScanArray[i] == PRE_SCAN_VALUE)
         ScanArray[i] = 255;
      else
         ScanArray[i] = ScanArray[i] >> 1;
    }

   ScanArray[ScanIndex] = PRE_SCAN_VALUE;
   ScanIndex += LEDScanIndexModifier;

   if ((ScanIndex == (LEDS_NUM_LED_IDS-1)) || (ScanIndex == 0))
         LEDScanIndexModifier *= -1;

    for (unsigned char i = 0; i < LEDS_NUM_LED_IDS; i++)
    {
        led_set_intensity(ptrLEDs, i, ScanArray[i]);
    }
}

