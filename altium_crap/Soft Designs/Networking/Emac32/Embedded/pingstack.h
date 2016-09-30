/*****************************************************************************\
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:        EMAC32 ethernet driver engineering example
|*
|*                      This file implements a very minimalistic ethernet
|*                      stack only capable of relying to an ICMP echo ('ping')
 */

#ifndef _PINGSTACK_H_
#define _PINGSTACK_H_

#include <stdint.h>

extern int eth_process(uint8_t *buf, int size);
extern void eth_init(const uint8_t *mac, const uint8_t *ip);


#endif // _PINGSTACK_H_
