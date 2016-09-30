/*************************************************************************
**
**  VERSION CONTROL:    $Revision:   1.6  $
**          $Date:   Mar 20 2003 15:20:46  $
**
**  IN PACKAGE:     Embedded TCPIP
**
**  COPYRIGHT:      Copyright (c) 2002 Altium
**
**  DESCRIPTION:    hardware dependent functions for 8051fpga
**
**************************************************************************/

#include <string.h>

// tcpipset.h should include sys_8051fpga.h and sys.h
#include "tcpipset.h"

#include "tealib\timer0.h"

//**************************************************************************

#ifdef TCPIPDEBUG
// convert romstrings to ramstrings (returns pointer to internal buffer, overwrites buffer on next call)
#ifdef ROMSTR
char *srom2ram(ROMMEMSPEC char *srom)
{
    static char sbuf[ROMSTR_MAXLEN];
    int i;

    for (i = 0; (*srom) && (i < ROMSTR_MAXLEN); ++i)
    {
        sbuf[i] = *srom++;
    }
    sbuf[i] = '\0';

    return sbuf;
}
#endif // ROMSTR
#endif


/***************************************************************************
 * FUNCTION:    ntohdw
 *
 * (net-to-host-Uint32) convert net-order Uint32 to host-order
 */
Uint32 ntohdw(Uint32 dw)
{
    Uint32 tmp;

    ((Uint8 *) & tmp)[0] = ((Uint8 *) & dw)[3];
    ((Uint8 *) & tmp)[1] = ((Uint8 *) & dw)[2];
    ((Uint8 *) & tmp)[2] = ((Uint8 *) & dw)[1];
    ((Uint8 *) & tmp)[3] = ((Uint8 *) & dw)[0];

    return tmp;
}



// wait for specified nr of milliseconds
void sys_sleep(Uint16 ms)
{
    tmr0_delay(ms);
}


void sys_init(void)
{
    tmr0_init();
}


//**************************************************************************
