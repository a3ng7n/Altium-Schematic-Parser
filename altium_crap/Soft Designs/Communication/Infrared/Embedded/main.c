/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:         IR decoder example
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:        Provide example for use of the WB_IRRC core in
|*                      combination with the standard Altium Remote Control or a
|*                      Philips RC5 Remote Control.
 */

#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <stdio.h>

#include "devices.h"
#include "drv_irrc.h"                                                             // used to drive the ir hardware

irrc_t             *irrc;

/*******************************************************************************
 *
 *  FUNCTION:           main
 *
 *  DESCRIPTION:        Print received commands on the terminal.
 */
int main(int argc, char * argv[])
{
    uint32_t plength;
    uint32_t slength;
    uint16_t address;
    uint16_t command;
    int      rec_cmd=0;
    int      ret;

    // Initialize infrared
    irrc = irrc_open(DRV_IRRC_1);

    switch (irrc_getcodec(irrc))
    {
    case IRRC_CODEC_RAW :
        printf("Use your Remote Control to send data to NB2!\n");
        printf("NB2 will print received raw datastream\n");
// To use irrc_rxrawdata you need to do interrupted rx with a large buffer size
// else you will get overflows (printf takes a long time)
        printf("Transmitting pulse sequence\n");
        irrc_txrawdata(irrc, 160, 80);
        irrc_txrawdata(irrc, 320, 40);
        irrc_txrawdata(irrc, 160, 80);
        while (1)
        {
            ret = irrc_rxrawdata(irrc, 0, &plength, &slength);
            switch (ret)
            {
            case -2 : // overflow
                printf("Overflow: Exit\n");
                return -1;
            case -1 : // nothing received
                break;
            default :
                 printf("0 for %d, 1 for %d |", slength, plength);
                 break;
            }
        }
    case IRRC_CODEC_NEC :
        printf("NB2 transmits 1 command and repeats it 3 times.\n");
        printf("You can also use your NEC Remote Control to send data to NB2!\n");
        printf("NB2 will print received datastream\n");
        irrc_txnecdata(irrc, 4, 65280, 62985);
        while (1)
        {
            ret = irrc_rxnecdata(irrc, &address, &command);
            switch (ret)
            {
            case -2 : // overflow
                printf("Overflow: Exit\n");
                return -1;
            case -1 : // nothing received
                break;
            default :
                 rec_cmd++;
                 if (ret)
                     printf("%d, data repeated\n", rec_cmd);
                 else
                     printf("%d, addr=%d cmd=%d\n", rec_cmd, address, command);
                 break;
            }
        }
    case IRRC_CODEC_RC5 :
        printf("Use your RC5 Remote Control to send data to NB2!\n");
        printf("NB2 will print received datastream\n");
        while (1)
        {
            ret = irrc_rxrc5data(irrc, &address, &command);
            switch (ret)
            {
            case -2 : // overflow
                printf("Overflow: Exit\n");
                return -1;
            case -1 : // nothing received
                break;
            default :
                 rec_cmd++;
                 if (ret)
                     printf("%d, data repeated\n", rec_cmd);
                 else
                     printf("%d, addr=%d cmd=%d\n", rec_cmd, address, command);
                 break;
            }
        }
    default:
        break;
    }
}
