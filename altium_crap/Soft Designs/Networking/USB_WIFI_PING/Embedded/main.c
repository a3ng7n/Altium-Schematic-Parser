#include <string.h>
#include <ctype.h>
#include <pthread.h>
#include <timing.h>

#include "devices.h"

#include <per_ioport.h>

#include <lwip.h>
#include <lwip/netif.h>
#include <lwip/tcpip.h>
#include "lwip/api.h"

#include <usbhost.h>
#include <swp_ifconfig.h>

////////////////////////////////////////////////////////////////////////////////
static usbhost_t  *USBHost;

////////////////////////////////////////////////////////////////////////////////
static void wait_for_and_print_ip_address(lwip_t * lwip)
{
    struct timespec ts = {1,0};
    struct ip_addr addr;

    do{
        printf(".");
        addr = lwip_get_local_addr(lwip);
        nanosleep(&ts, NULL);
    }while(addr.addr == 0);

    printf("\nSet IP address to: ");

    printf ("%hu.%hu.%hu.%hu\n",
            ip4_addr1(&addr),
            ip4_addr2(&addr),
            ip4_addr3(&addr),
            ip4_addr4(&addr));
}

////////////////////////////////////////////////////////////////////////////////
void lwip_setup(int inst)
{
    lwip_t * lwip;
    printf("Starting server... \n");
    printf("LWIP Initialisation: ");
    lwip = lwip_open(inst);
    if (!lwip)
    {
        printf("LWIP open failed.\n");
        return;

    }
    printf("Done.\n");
    printf("Starting LWIP: ");
    if (lwip_start(lwip)!=ERR_OK)
    {
        printf("LWIP start failed.\n");
        return;
    }
    printf("Done.\n");

    printf("Acquiring IP Address: ");
    wait_for_and_print_ip_address(lwip);
}

////////////////////////////////////////////////////////////////////////////////
void init_usb(void)
{

    printf("Init USB subsystem: ");
    if ((USBHost = usbhost_open(USBHOST_1)) == NULL)
    {
        printf("Failed usbhost_open(USBHOST_1)\n");
        abort();
    }
    printf("Done.\n");
}

////////////////////////////////////////////////////////////////////////////////
void main(void)
{
    pthread_t thread;
    pthread_attr_t attr;
    ifconfig_t *ifconfig=NULL;
    uint32_t    linkState=0;
    struct timespec ts = {1,0};

    printf("PING server test\n");

    init_usb();

    printf("Attach to USB network device: ");
    do{
        usbhost_process(USBHost);
        ifconfig = usbhost_network_ifconfig(USBHOST_NETWORK_1);
    } while(!ifconfig);
    printf("Done.\n");

    printf("Init USB network device(Keygen may take up to 30Sec): ");

    // Enable to override SSID
    //ifconfig_ieee80211_set_ssid(ifconfig, "CUSTOM_SSID");

    // Enable to override Pass Phrase
    //ifconfig_ieee80211_set_passphrase(ifconfig, "CUSTOM_PASS_PHRASE");


    // Enable to override Crypt type from the following:
    //  IFCONFIG_IEEE80211_CRYPT_OPEN
    //  IFCONFIG_IEEE80211_CRYPT_WEP
    //  IFCONFIG_IEEE80211_CRYPT_WPAPSK
    //ifconfig_ieee80211_set_crypt(ifconfig, IFCONFIG_IEEE80211_CRYPT_OPEN, false);


    ifconfig_link_start(ifconfig);
    printf("Done.\n");

    printf("Waiting for Wireless Link : ");
    while(!(linkState==IFCONFIG_LINK_S_UP)){
        ifconfig_link_get_state(ifconfig, &linkState);
    }
    printf("CONNECTED!.\n");

    lwip_setup(LWIP_1);

    printf("PING server up and running!\n");

    while(1){
        nanosleep(&ts, NULL);
    }

    /* never reached */
}


