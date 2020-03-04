/*************************************************************************
**
**  VERSION CONTROL:	@(#)eth_driver.h	1.1	03/04/15
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	Ethernet device driver lowlevel API
**
**************************************************************************/

#ifndef _ETH_DRIVER_H_
#define _ETH_DRIVER_H_

#include "tcpipset.h"

//*************************************************************************

extern void eth_drv_init(void);
extern Uint8 eth_drv_send(Uint16 data_length);
extern Uint16 eth_drv_recv(void);

//*************************************************************************

#endif
