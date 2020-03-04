#include <stdio.h>
#include <drv_ir38k.h>
#include "devices.h"
#include "generic_devices.h"

// Handle for IR receiver
ir38k_t * ir38k;

int main(int argc, char * argv[])
{
    uint16_t   address;
    uint16_t   command;
    int        rec_cmd = 1;
    int        ret;
    int        ercnt = 0;

    // Initialize NB Starterkit infrared
    ir38k = ir38k_open(DRV_IR38K_1);

    printf("Point your NEC Remote Control to the IR receiver and\n");
    printf("push some buttons. This example will print the received\n");
    printf("datastream.\n\n");
    while (1)
    {
         ret = ir38k_rxnecdata(ir38k, &address, &command);
         if (ir38k_getbiterror(ir38k) > ercnt)
         {
            ercnt = ir38k_getbiterror(ir38k);
            printf ("Total decoding error count increased and is now at %d\n", ercnt);
         }
         if(ercnt > 5)
         {
             puts("\nToo many decoding errors ->");
             puts("Use a remote control using the NEC transmission format");
             puts("and/or check the clock frequency!\n");
             ercnt = 0;
             ir38k_rstbiterror(ir38k); // reset the error counter
         }
         switch (ret)
         {
         case 1: // repeat data
              printf("%d. Last data repeated\n", rec_cmd);
              rec_cmd++;
              break;
         case 0: // normal data
             printf("%d. Normal data, addr=%d cmd=%d\n", rec_cmd, address, command);
             rec_cmd++;
             break;
         case -1:  // nothing received
             break;
         case -2:  // Buffer overrun
             printf("Receive buffer overrun.\n");
             break;
         default: ; // do nothing
         }
    }
}

