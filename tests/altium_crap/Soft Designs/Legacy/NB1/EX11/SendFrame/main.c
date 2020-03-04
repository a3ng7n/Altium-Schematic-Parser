/*    -- MD addresses:
    -- 5F0 (1520) PHY_ADDR
    -- 5F1 (1521) REG_ADDR
    -- 5F2 (1522) DATA_LOW
    -- 5F3 (1523) DATA_HIGH
    -- 5F4 (1524) COMMAND

    -- MAC address structure 000h..5FFh (0..1535):
    -- 000h..5EDh (0..1517)     Message:
    --      000h..005h (0..5)        Destination address
    --      006h..00Bh (6..11)       Source address
    --      00Ch..00Dh (12..13)      Length / Type
    --      00Eh..5EDh (14..1517)    Message + CRC (CRC only for receive)
    -- 5EEh..5EFh (1518..1519)  Not Used
    -- 5F0h..5F4h (1520..1524)  MD. see above
    -- 5F5h       (1525)        Interrupt register.
    -- 5F6h..5F7h (1526..1527)  Lenght of TX_Mess, including addresses and length/type, without padding or CRC (Write only)
    -- 5F6h..5F7h (1526..1527)  Lenght of RX_Frame, including addresses, length, padding (Read only)
    -- 5F8h       (1528)        Command (Read/Write) (bit0 = start_tx ; bit1 = start_rx)
    -- 5F9h       (1529)        Status (Read Only)   (bit0 = tx_ready ; bit1 = rx_valid ; bit2 = md_ready)
    -- 5FAh..5FFh (1530..1535)  MAC Address (Write Only)
*/

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

//WB addresses
#define WB_ADDR_H    0xFF
#define WB_ADDR_L    0xFE
#define WB_DATA      0xFD

//PING
#if 1                                                                                        
__rom unsigned char frame[] = { 0x01, 0x23, 0x45, 0x67, 0x89, 0x01,             /* dest */

                                0x00, 0x07, 0xe9, 0x7f, 0x0a, 0x34,             /* src */

                                0x08, 0x00,                                     /* length/type */

                                0x45, 0x00, 0x00, 0x3c, 0x83, 0x06, 0x00, 0x00, /* IP4 header */
                                0x80, 0x01, 0xe2, 0x44, 0xac, 0x11, 0x04, 0xb7,
                                0xc0, 0xa8, 0x64, 0x05,

                                0x08, 0x00, 0x18, 0x5c, 0x04, 0x00,             /* ICMP header */

                                0x31, 0x00, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, /* data */
                                0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e,
                                0x6f, 0x70, 0x71, 0x72, 0x73, 0x42, 0x75, 0x76,
                                0x77, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
                                0x68, 0x68,
};

#else

//CHECKSUM CHECK
__rom unsigned char frame[] = { 0xFF, 0xFF, 0xFF, 0xFF//, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                                0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                                0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                                0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
 };

#endif


__rom unsigned char mac_addr[] = { 0x01, 0x23, 0x45, 0x67, 0x89, 0x01 };


void write_emac( unsigned int address, char data)
{
    * (char *)address = data;
}

void main( void )
{
    char i;
    char j = 0;
    int ep = 0;


    ep = ETH_MAC_ADDR;

    for (i = 0; i < sizeof( mac_addr ); i++)
    {
       write_emac(ep+i, mac_addr[i]);
    }

    ep = 0;
    for (i = 0; i < sizeof( frame ); i++)
    {
       write_emac(ep+i, frame[i]);
    }

    write_emac( ETH_LENGTH, sizeof( frame ) >> 8);
    write_emac( ETH_LENGTH+1, sizeof( frame ) & 0xFF);
    write_emac( ETH_COMMAND, 3);


    while ( 1 )
    {
        P0_7 = 1;
    }

}

