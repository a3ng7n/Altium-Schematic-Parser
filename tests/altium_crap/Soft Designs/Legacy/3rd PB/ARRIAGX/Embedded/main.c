#include "devices.h"
#include <drv_ioport.h>
#include <timing.h>

#define C_Keydebounce           10        //key debouncing 10ms
#define YES                     0xAA
#define NO                      0
#define Key_Idle                0x01

ioport_t * gpio_port;


unsigned char LED_Update = 250;


unsigned char KeyScan(void)
{
     static unsigned long long t_tick = 0;
     static unsigned char copy_port_b, last_valid;
     if (elapsed_time_ms(t_tick) > C_Keydebounce)
     {
         t_tick = clock_ms();
         if (copy_port_b == ioport_get_value(gpio_port,1))
         {
             if (copy_port_b != last_valid)
             {
                last_valid = copy_port_b;
                return YES;
             }
         }
         else
         {
             copy_port_b = ioport_get_value(gpio_port,1);
             last_valid = Key_Idle;
        }
     }
     return NO;
}


void LED(unsigned char fresh_rate)
{
     static unsigned long long t_led = 0;
     static unsigned char copy_led = 0;

     if (elapsed_time_ms(t_led) > fresh_rate)
     {
         t_led = clock_ms();
         copy_led++;
     }

     ioport_set_value(gpio_port,0,copy_led);
}

void main(void)
{
    gpio_port = ioport_open(DRV_IOPORT_1);

    while(1)
    {
        if (KeyScan() == YES)
        {
           if (LED_Update > 50)
           {
              LED_Update -= 40;
           }
           else
           {
               LED_Update = 250;
           }
        }
        LED(LED_Update);
    }
}
