/*****************************************************************************\
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:        EMAC32 ethernet driver engineering example
|*
|*                      This file implements a very minimalistic ethernet
|*                      stack only capable of relying to an ICMP echo ('ping')
 */

#include <string.h>
#include <stdio.h>

#include <util_endian.h>

#include "pingstack.h"

//*** some internet constants *************************************************

#define ETH_TYPE_ARP            0x0806
#define ETH_TYPE_IP             0x0800

#define ARP_HARDWARE            0x0001
#define ARP_REQUEST             0x01
#define ARP_REPLY               0x02

#define IP_VERSION_IP4          4

#define IP_PROTOCOL_ICMP        1

#define ICMP_TYPE_ECHOREPLY     0x00
#define ICMP_TYPE_ECHO          0x08


//*** internet header layouts *************************************************

typedef struct __packed__
{
    uint8_t destmac[6];
    uint8_t sourcemac[6];
    uint16_t type;
} eth_t;

typedef struct __packed__
{
    uint16_t hardware;
    uint16_t protocol;
    uint8_t hardwarelength;
    uint8_t protocollength;
    uint16_t operation;
    uint8_t senderhardware[6];
    uint8_t senderprotocol[4];
    uint8_t targethardware[6];
    uint8_t targetprotocol[4];
} arp_t;

typedef struct __packed__
{
    uint8_t version;
    uint8_t service;
    uint16_t packetlength;
    uint16_t ident;
    uint16_t offset;
    uint8_t timetolive;
    uint8_t protocol;
    uint16_t checksum;
    uint8_t sourceip[4];
    uint8_t destip[4];
} ip_t;

typedef struct __packed__
{
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t ident;
    uint16_t sequence;
} icmp_t;


//*** our identity *******************************************************

static const uint8_t *my_mac;
static const uint8_t *my_ip;



/**********************************************************************
|*
|*  FUNCTION    : addcarry16
|*
|*  PARAMETERS  : sum = 32-bit checksum
|*
|*  RETURNS     : converted checksum
|*
|*  DESCRIPTION : Convert a 32-bit checksum to an 16-bit internet checksum
|*               field by recursively adding all bits over 16 and inverting
 */
static uint16_t addcarry16(uint32_t sum)
{
    // add any carry
    while (sum >> 16)
    {
        sum = (sum & 0xffff) + (sum >> 16);
    }

    return ~(uint16_t) (sum & 0xffff);
}


/**********************************************************************
|*
|*  FUNCTION    : checksum
|*
|*  PARAMETERS  : chkdata = start of data
|*                length = length of dsata in bytes
|*
|*  RETURNS     : checksum
|*
|*  DESCRIPTION : 32-bit internet checksum over block of data
 */
static uint32_t checksum(uint8_t *chkdata, int length)
{
    uint32_t sum;
    uint32_t add;

    for (sum = 0; length > 1; length -= 2)
    {
#ifdef __BIG_ENDIAN__
        add = (uint32_t) ((((uint16_t) * chkdata) << 8) + chkdata[1]);
#else
        add = (uint32_t) ((((uint16_t) chkdata[1]) << 8) + *chkdata);
#endif
        sum = sum + add;
        chkdata += 2;
    }

    /* add up any odd Uint8 */
    if (length == 1)
    {
#ifdef __BIG_ENDIAN__
        add = (uint32_t) (((uint16_t) *chkdata) << 8);
#else
        add = (uint32_t) * chkdata;
#endif
        sum = sum + add;
    }

    return sum;
}


/**********************************************************************
|*
|*  FUNCTION    : arp_sendprep
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Finalize received ARP frame before returning
 */
static void arp_sendprep(uint8_t *buf, int size)
{
    arp_t *arp = (arp_t*) buf;

    // use our own mac and ip as the sender
    memcpy(arp->senderhardware, my_mac, 6);
    memcpy(arp->senderprotocol, my_ip, 4);
}


/**********************************************************************
|*
|*  FUNCTION    : arp_process
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : length of frame to return
|*
|*  DESCRIPTION : Process received ARP frame, convert it into its answer
 */
static int arp_process(uint8_t *buf, int size)
{
    arp_t *arp = (arp_t*) buf;

    if (size < sizeof(arp_t))
    {
        // header incomplete
        return -1;
    }

    switch (arp->operation)
    {
    case BIG16(ARP_REQUEST):
        if ((arp->targetprotocol[0] != my_ip[0]) ||
            (arp->targetprotocol[1] != my_ip[1]) ||
            (arp->targetprotocol[2] != my_ip[2]) ||
            (arp->targetprotocol[3] != my_ip[3]))
        {
            // not our IP address
            return -1;
        }

        if ((arp->hardware != BIG16(ARP_HARDWARE)) || (arp->hardwarelength != 0x06) ||
            (arp->protocol != ETH_TYPE_IP) || (arp->protocollength != 0x04))
        {
            // hardware or protocol mismatch
            return -1;
        }

        // convert the frame to its reply
        arp->operation = BIG16(ARP_REPLY);

        // target is now the original sender
        memcpy(arp->targethardware, arp->senderhardware, 6);
        memcpy(arp->targetprotocol, arp->senderprotocol, 4);

        printf("send ARP reply\n");

        break;

    default:
        printf("unsupported ARP operation\n");
        return 0;

    }

    arp_sendprep(buf, size);

    return size;
}


/**********************************************************************
|*
|*  FUNCTION    : icmp_sendprep
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Finalize received ICMP frame before returning
 */
static void icmp_sendprep(uint8_t *buf, int size)
{
    icmp_t *icmp = (icmp_t*) buf;

    icmp->checksum = 0;
    icmp->checksum = addcarry16(checksum(buf, size));
}


/**********************************************************************
|*
|*  FUNCTION    : icmp_process
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : length of frame to return
|*
|*  DESCRIPTION : Process received ICMP frame, convert it into its answer
 */
static int icmp_process(uint8_t *buf, int size)
{
    icmp_t *icmp = (icmp_t*) buf;

    if (size < sizeof(icmp_t))
    {
        // incomplete
        return -1;
    }

    // verify checksum
    if (addcarry16(checksum(buf, size)))
    {
        // checksum error
        return -1;
    }

    switch (icmp->type)
    {
        case ICMP_TYPE_ECHO:
           switch (icmp->code)
           {
               case 0:
                   // ping packet, change it into it's own reply
                   icmp->type = ICMP_TYPE_ECHOREPLY;

                   printf("send ICMP echoreply\n");

                   break;

               default:
                   printf("unsupported ICMP ECHO code\n");
                   return 0;

           }

           break;

       default:
           printf("unsupported ICMP type\n");
           return 0;

    }

    icmp_sendprep(buf, size);

    return size;
}


/**********************************************************************
|*
|*  FUNCTION    : ip_sendprep
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Finalize received IP frame before returning
 */
static void ip_sendprep(uint8_t *buf, int size)
{
    ip_t *ip = (ip_t*) buf;

    ip->packetlength = big16(size & 0xFFFF);

    memcpy(ip->sourceip, my_ip, 4);

    ip->checksum = 0;
    ip->checksum = addcarry16(checksum(buf, sizeof(ip_t)));
}


/**********************************************************************
|*
|*  FUNCTION    : ip_process
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : length of frame to return
|*
|*  DESCRIPTION : Process received IP frame, convert it into its answer
 */
int ip_process(uint8_t *buf, int size)
{
    ip_t *ip = (ip_t*) buf;
    int returndatasize;

    if (size < sizeof(ip_t))
    {
        // header incomplete
        return -1;
    }

    if ((ip->destip[0] != my_ip[0]) ||
        (ip->destip[1] != my_ip[1]) ||
        (ip->destip[2] != my_ip[2]) ||
        (ip->destip[3] != my_ip[3]))
    {
        // not our IP address
        return -1;
    }

    if ((ip->version >> 4) != IP_VERSION_IP4)
    {
        printf("unsupported IP version\n");
        return 0;
    }

    if (addcarry16(checksum(buf, sizeof(ip_t))))
    {
        // ipheader checksum error
        return -1;
    }

    if (size < ip->packetlength)
    {
        // data incomplete (could just mean our buffer was too small)
        return -1;
    }

    switch (ip->protocol)
    {
    case IP_PROTOCOL_ICMP:
        returndatasize = icmp_process(buf + sizeof(ip_t), size - sizeof(ip_t));
        break;

    default:
        printf("unsupported IP protocol\n");
        return 0;

    }

    if (returndatasize < 0) return returndatasize;

    size = sizeof(ip_t) + returndatasize;

    memcpy(ip->destip, ip->sourceip, 4);

    ip_sendprep(buf, size);

    return size;
}



/**********************************************************************
|*
|*  FUNCTION    : eth_sendprep
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : finalize received ETH frame before returning
 */
static void eth_sendprep(uint8_t *buf, int size)
{
    // no need to set sourcemac, driver will do that
}


/**********************************************************************
|*
|*  FUNCTION    : eth_process
|*
|*  PARAMETERS  : buf = start of frame
|*                size = length of frame in bytes
|*
|*  RETURNS     : length of frame to return to sender
|*
|*  DESCRIPTION : Process received ETH frame, convert it into its answer
 */
extern int eth_process(uint8_t *buf, int size)
{
    eth_t *eth = (eth_t*) buf;
    int returndatasize;

    if (size < sizeof(eth_t))
    {
        // incomplete
        return -1;
    }

    switch (eth->type)
    {
    case BIG16(ETH_TYPE_ARP):
        returndatasize = arp_process(buf + sizeof(eth_t), size - sizeof(eth_t));
        break;

    case BIG16(ETH_TYPE_IP):
        returndatasize = ip_process(buf + sizeof(eth_t), size - sizeof(eth_t));
        break;

    default:
        printf("unsupported ETHERNET type\n");
        return 0;

    }

    if (returndatasize < 0) return returndatasize;

    size = sizeof(eth_t) + returndatasize;

    memcpy(eth->destmac, eth->sourcemac, 6);

    eth_sendprep(buf, size);

    return size;
}


/**********************************************************************
|*
|*  FUNCTION    : eth_init
|*
|*  PARAMETERS  : mac = our MAC address
|*                ip = out IP number
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Set up our identity
 */
extern void eth_init(const uint8_t *mac, const uint8_t *ip)
{
    my_mac = mac;
    my_ip = ip;
}

