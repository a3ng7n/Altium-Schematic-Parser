/*************************************************************************
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*
|*  IN PACKAGE:         SMS engine Main
|*
|*  COPYRIGHT:          Copyright (c) 2010, Altium
|*
|*  DESCRIPTION:        SMS module, read incomming messages and
|*                      process them...
|*
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <timing.h>

#include "devices.h"
#include "modem.h"

#include <graphics.h>
#include <textdisplay.h>

#define SMSC_INTERNATIONAL  0x91
#define MDM_PINCODE     "\"4510\""

modem_t    *modem;


void main(int argc, char * argv[])
{
    int     mem_location;
    char    pdu_message[SMS_MSG_SIZE];
    char    new_message[SMS_MSG_SIZE];
    smsg_t  sms_message;

    printf ("Initializing Modem...\n");
    modem = modem_open(MODEM_GSM);
    while (!modem_start(modem));
    modem_echo(modem, false,true);

    printf("Waiting for network...\n");
    while(!modem_check_network(modem));
    modem_sms_init(modem,SMS_STORAGE_SIM);
    printf("\nModem ready to receive messages...\n");
    for(;;)
    {
        mem_location = modem_read_sms(modem, pdu_message, SMS_UNREAD);
        if (mem_location<0) continue;

        modem_delete_sms(modem, mem_location);
        sms_pdu_decode(pdu_message, &sms_message);
        printf("time: %s\nfrom: %s (type %2.2x)\ndata: '%s'\n",
            sms_message.timestamp,
            sms_message.from,
            sms_message.addr_type,
            sms_message.message);

        if (sms_message.addr_type == SMSC_INTERNATIONAL)
        {
            sprintf(new_message,"NB2 Recvd: %s",sms_message.message);
            sms_pdu_encode(new_message, sms_message.from, pdu_message);
            modem_send_sms(modem, pdu_message);
        }
    }
}
