#include <stdio.h>
#include <time.h>
#include "swplatform.h"


////////////////////////////////////////////////////////////////////////////////
void set_unique_mac (void)
{
    spi_t       *spi;
    bool         got_bus;
    int          i;
    ethernet_t  *ethernet;
    uint8_t      mac[6] = {0};

    spi = spi_open ( DRV_SPI_1 );
    got_bus = spi_get_bus ( spi, DEVICE_1WIRE_NB );
    while ( !got_bus )
        got_bus = spi_get_bus ( spi, DEVICE_1WIRE_NB );

    spi_cs_lo ( spi );
    spi_transmit8 ( spi, 0x03 ); // select memories
    spi_transmit8 ( spi, 0x00 ); // select first device - the NanoBoard

    // MAC is a custom internal mac
    mac[0] = 0x02;
    // Discard first 5 bytes of ID
    for ( i=1; i<7; i++ )
    {
        mac[i] = spi_transceive8 ( spi, 0 );
    }

    spi_cs_hi ( spi );

    spi_release_bus ( spi );


    printf("\nUsing MAC: ");
    for(i=0;i<7;i++){
        printf("%02X", mac[i]);
    }
    printf("\n");

    ethernet = ethernet_open ( ETHERNET_1 );
    ethernet_setmac ( ethernet, mac );

}

////////////////////////////////////////////////////////////////////////////////
void main(void)
{
    printf("Starting LWIP...\n");

    // generated initialization code
    swplatform_init_stacks();

    if (!net_device)
    {
        printf("LWIP open failed.\n");
        return;
    }

    set_unique_mac();

    if (lwip_start(net_device)!=ERR_OK)
    {
        printf("LWIP start failed.\n");
        return;
    }

    printf("Waiting for DHCP response..");
    const struct timespec ts = { .tv_sec = 1, .tv_nsec = 0 };
    for(;;)
    {
        struct ip_addr addr = lwip_get_local_addr(net_device);
        if (!ip_addr_isany(&addr))
        {
            break;
        }
        nanosleep(&ts, NULL);
        putchar('.');
    }
    printf("Ok\n");

    lwip_print_addrs(net_device);

    printf("Ready. You can now ping this example.\n");
}


