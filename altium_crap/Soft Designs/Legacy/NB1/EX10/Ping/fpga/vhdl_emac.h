/*************************************************************************
**
**  VERSION CONTROL:    @(#)eth_c166_smsc91.h   1.1 03/04/15
**
**  IN PACKAGE:     Embedded TCPIP
**
**  COPYRIGHT:      Copyright (c) 2002 Altium
**
**  DESCRIPTION:    C166 ethernet SMsC LAN91Cxxx device driver
**
**************************************************************************/

#ifndef _VHDL_EMAC_H_
#define _VHDL_EMAC_H_

//*************************************************************************

#define EMAC_BUF 0x0000
#define EMAC_MESS_LEN (*( __xdata volatile unsigned int *)0x5F6)
#define EMAC_MAC_ADDR 0x5FA
#define EMAC_COMMAND (*( __xdata volatile unsigned char *)0x5F8) /* bit0 = start_tx ; bit1 = start_rx */
#define EMAC_STATUS (*( __xdata volatile unsigned char *)0x5F9)  /* bit0 = tx_ready ; bit1 = rx_full */
#define EMAC_INT (*( __xdata volatile unsigned char *)0x5F5)

//*************************************************************************

#endif
