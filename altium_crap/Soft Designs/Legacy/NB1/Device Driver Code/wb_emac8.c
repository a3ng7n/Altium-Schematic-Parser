/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   EMAC 8-Bit utility functions
|*
\*****************************************************************************/

//#include "hardware.h"
#include "wb_emac8.h"
#include <string.h>
#include <stdlib.h>

/* EMAC8 hardware definitions */
#define EMAC_BASE(base)  ((volatile unsigned char *) base)
#define EMAC_INT    1
#define EMAC_DATA(base)    EMAC_BASE(base)[1]
#define EMAC_ADDR_L(base)  EMAC_BASE(base)[2]
#define EMAC_ADDR_H(base)  EMAC_BASE(base)[3]

//E_MAC addresses
#define ETH_MESS     0x000
#define ETH_LENGTH   0x5F6
#define ETH_MAC_ADDR 0x5FA
#define ETH_COMMAND  0x5F8
#define ETH_STATUS   0x5F9
#define PHY_ADDR     0x5F0 // (1520) PHY_ADDR
#define REG_ADDR     0x5F1 // (1521) REG_ADDR
#define DATA_LOW     0x5F2 // (1522) DATA_LOW
#define DATA_HIGH    0x5F3 // (1523) DATA_HIGH
#define COMMAND      0x5F4 // (1524) COMMAND
#define ETH_INT      0x5F5

// STATUS register bits
#define EMAC_TXR          0x01
#define EMAC_RXV          0x02
#define EMAC_MDR          0x04

// CMD register bits
#define EMAC_STX          0x01
#define EMAC_SRX          0x02

// IE register bits
#define EMAC_ITX          0x01
#define EMAC_IRX          0x02
#define EMAC_IMD          0x04

#define inline

static t_mac_addr mymac;

static inline void write_emac( unsigned int base, unsigned int address, unsigned char data)
{
    EMAC_ADDR_H(base) = ( address >> 8 );
    EMAC_ADDR_L(base) = ( address & 0xFF );
    EMAC_DATA(base)   = data;
}


static inline unsigned char read_emac( unsigned int base, unsigned int address)
{
    EMAC_ADDR_H(base) = ( address >> 8 );
    EMAC_ADDR_L(base) = ( address & 0xFF );
    return EMAC_DATA(base);
}


/*****************************************************************************\
|*
|*  FUNCTION:       emac8_init
|*
|*  AVAILABILITY:   GLOBAL
|*
|*  PARAMETERS:     mac_addr = own mac address
|*
|*  RETURN VALUE:   None
|*
|*  DESCRIPTION:    Init EMAC8
 */
void emac8_init( unsigned int base, const t_mac_addr mac_addr )
{
    char i;
    int ep = 0;

    // write MAC address into the Individual Address Registers
    ep = ETH_MAC_ADDR;
    for (i = 0; i < sizeof( t_mac_addr ); i++)
    {
       write_emac(base, ep+i, mac_addr[i]);
    }

    // write local mac address
    memcpy(mymac, mac_addr, 6);

    write_emac( base, ETH_COMMAND, EMAC_SRX );            // enable receive
}


/*****************************************************************************\
|*
|*  FUNCTION:       emac8_getstatus
|*
|*  AVAILABILITY:   GLOBAL
|*
|*  PARAMETERS:     None
|*
|*  RETURN VALUE:   0 if not busy
|*
|*  DESCRIPTION:    Retrieves the status of the EMAC8
 */
unsigned char emac8_getstatus( unsigned int base, void )
{
    return read_emac( base, ETH_STATUS );
}


/*****************************************************************************\
|*
|*  FUNCTION:       emac8_getpacket
|*
|*  AVAILABILITY:   GLOBAL
|*
|*  PARAMETERS:     None
|*
|*  RETURN VALUE:   Pointer to structure with ethernet packet, NULL pointer if buffer is empty
|*
|*  DESCRIPTION:    get access to next packet in buffer, if avaialble, free previous packet from buffer
 */
t_emac_packet* emac8_getpacket( unsigned int base, t_emac_packet* packet, int size )
{
    t_emac_packet* retval = NULL;
    char *p_c = (char *)&packet->dest;
    unsigned int emac = base;
    static unsigned int len;

    if ( read_emac( base, ETH_STATUS ) & EMAC_RXV )
    {
       len = read_emac ( base, ETH_LENGTH + 1 ) + ( read_emac( base, ETH_LENGTH ) << 8 );
       if ( len  < size )
       {
          packet->data_size = len - 14;
          while ( len-- )
          {
             *p_c++ = read_emac( base, emac++ );
          }
          retval = packet;
          write_emac( base, ETH_COMMAND, EMAC_SRX );
       }
    }
    return retval;
}


/*****************************************************************************\
|*
|*  FUNCTION:       emac8_putpacket
|*
|*  AVAILABILITY:   GLOBAL
|*
|*  PARAMETERS:     packet      = pointer to Ethernet packet, including data_size, dest, src, type, data
|*                  dest        = destination address
|*                  type        = type/length field in Ethernet frame
|*                  data_size   = size of data field in Ethernet frame
|*                  data        = pointer to payload
|*
|*  RETURN VALUE:   None
|*
|*  DESCRIPTION:    get access to next packet in buffer, if avaialble, free previous packet from buffer
 */
void emac8_putpacket( unsigned int base, t_emac_packet* packet )
{
    int emac = base;
    int len;
    static unsigned char* p_c = 0;
    p_c = (unsigned char*)packet->dest;

    while ( ! (read_emac( base, ETH_STATUS ) & EMAC_TXR ));

    len = packet->data_size + 14;

    // set length
    write_emac( base, ETH_LENGTH, (char)( len >> 8));
    write_emac( base, ETH_LENGTH+1, (char)( len & 0xFF));

    while ( len-- )
    {
       write_emac( base, emac++, *p_c++ );
    }

    // start transmit
    write_emac( base, ETH_COMMAND, EMAC_STX );
}


void emac8_putdata( unsigned int base, const t_mac_addr dest, const unsigned short type, unsigned int data_size, unsigned char* data )
{
    int emac = base;
    int i;

    while ( ! (read_emac( base, ETH_STATUS ) & EMAC_TXR ));

    // set length
    write_emac( base, ETH_LENGTH, (char)(( data_size + 14 ) >> 8 ));
    write_emac( base, ETH_LENGTH+1, (char)(( data_size + 14 ) & 0xFF ));

    // copy dest to EMAC
    for ( i = 0; i < 6; i++ )
    {
       write_emac(base, emac++, *dest++ );
    }

    // copy src to EMAC
    for ( i = 0; i < 6; i++ )
    {
       write_emac(base, emac++, mymac[i] );
    }

    // copy type
    write_emac( base, emac++, (char)( type >> 8 ));
    write_emac( base, emac++, (char)( type & 0xFF ));

    // copy data
    while ( data_size-- > 0 )
    {
       write_emac(base, emac++, *data++);
    }

    // start transmit
    write_emac( base, ETH_COMMAND, EMAC_STX );
}


