#include "devices.h"
#include <drv_ioport.h>

ioport_t * port_a;
unsigned int p_value = 0;

void delay(unsigned int temp)
{
   unsigned int i;
   for (i = 0; i < temp; i++);
}

void main(void)
{

      port_a = ioport_open(DRV_IOPORT_1);
      
      while(1)
      {
           ioport_set_value(port_a,0,p_value++);
           delay(100000);
      }

}
