#ifndef npd_h
    #define npd_h

    #define TRUE 1
    #define FALSE 0

//..............................................................................
    #define CLOCK_FREQ 10137600
    #define DELAY5CON  0x0FFF
//..............................................................................

//..............................................................................
//Port addresses
//..............................................................................
#define PORT_BASE   0x4000
#define PORTA       PORT_BASE + 0
#define PORTB       PORT_BASE + 1
#define PORTC       PORT_BASE + 2
#define PORTD       PORT_BASE + 3
//..............................................................................

//..............................................................................
//Serial interface addresses
//..............................................................................
    #define SERIAL_BASE 0x8000
    #define PCON_ID     SERIAL_BASE + 0
    #define S0CON_ID    SERIAL_BASE + 1
    #define S0BUF_ID    SERIAL_BASE + 2
    #define S0RELL_ID   SERIAL_BASE + 3
    #define S0RELH_ID   SERIAL_BASE + 4
    #define ADCON_ID    SERIAL_BASE + 8
//..............................................................................

//..............................................................................
    #define S0RELL_INI  0x0DF
    #define S0RELH_INI  0x03
    #define S0CON_INI   0x50
    #define PCON_INI    0x80
    #define ADCON_INI   0x80
//..............................................................................

//..............................................................................
    #define KCODE
    #define TCODE __rom
//..............................................................................

//..............................................................................
    #define WORD unsigned short
    #define BYTE unsigned char
    #define word unsigned short
    #define byte unsigned char
//..............................................................................

//..............................................................................
    #define SPACE 0x20

    #define ONE_EMOD    0  //1
    #define TWO_EMOD    1  //2
    #define THREE_EMOD  2  //3
#endif
