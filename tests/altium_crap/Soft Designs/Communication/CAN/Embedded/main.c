/*****************************************************************************
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Can driver demonstration program.
|*
\*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <devices.h>
#include <drv_can.h>

can_t * canrx, * cantx;


/* main() ********************************************************************/
void main (int argc, char *argv[])
{
    canid_t canid = 1;
    uint8_t rxbuf[32], txbuf[32] = "123\000456\000678\000ABC\000DEF";
    int     i = 1;

    printf("\nCAN driver example\n");
    cantx = can_open(DRV_CAN_1);
    canrx = can_open(DRV_CAN_2);
    // make sure receive buffer is large enough to receive complete transmitted message
    can_init_rxmo(canrx, canid, 0, rxbuf, 32);
    // only sent 4 bytes in this message (32 at once does also work; in this
    // case messages are split into 8 bytes can messages)
    can_init_txmo(cantx, canid, 0, txbuf, 4);
    can_sent_txmo(cantx, canid);
    do
    {
        if ((canid = can_receive_rxmo(canrx)) < (canid_t)0xFFFF)
        {
            printf("received %s\n", rxbuf);
            // release message object; another CAN message can be received
            // in this message object
            can_release_rxmo(canrx, canid);
            if (i < 5)
            {
                can_setdata_txmo(cantx, canid, &txbuf[4*i], 4);
                can_sent_txmo(cantx, canid);
                i++;
            }
            else
            {
                break;
            }
        }
    } while (1);
    can_stop(cantx); can_stop(canrx);
}

