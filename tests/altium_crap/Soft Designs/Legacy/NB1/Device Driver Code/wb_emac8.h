/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   EMAC 8-Bit utility functions
|*
\*****************************************************************************/

#ifndef _WB_EMAC8_H
#define _WB_EMAC8_H

typedef unsigned char t_mac_addr[6];

typedef struct
{
    int data_size;
    t_mac_addr dest;
    t_mac_addr src;
    unsigned short type;
    unsigned char data[];
} t_emac_packet;

extern void           emac8_init     ( unsigned base, const t_mac_addr mac_addr );
extern t_emac_packet* emac8_getpacket( unsigned base, t_emac_packet* packet, int size );
extern void           emac8_putpacket( unsigned base, t_emac_packet* packet );
extern void           emac8_putdata  ( unsigned base, const t_mac_addr dest, const unsigned short type, unsigned int data_size, unsigned char* data );

extern unsigned char  emac8_getstatus( unsigned base, void );

#endif
