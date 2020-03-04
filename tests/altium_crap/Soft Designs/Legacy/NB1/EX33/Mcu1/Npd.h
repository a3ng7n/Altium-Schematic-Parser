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

    //Tables of modes
    //Main

    #define NOP_MSM    0
    #define SCROLL_MSM    1
    #define EDIT_MSM    2
    #define HELP_MSM    3
    #define SERE_MSM    4 //send receive mode

    //Key pad character values
    #define SCROLEDIT_MOD 7   //13
    #define HELP_MOD  12  //10
    #define SERE_MOD  14  //11 //send receive mode

    //Inside send receive mode SERE_MOD
    #define SEND_SRMOD    13 //0
    #define RECEIVE_SRMOD  0 //1 //send receive mode
    
    //Inside scroll mode
    #define CLR_SMOD    3   //12
    #define LARROW_SMOD 4   //4
    #define RARROW_SMOD 6   //6
    #define UARROW_SMOD 1   //2
    #define DARROW_SMOD 9   //8
    
    //Inside edit mode
    #define ZERO_EMOD   13 //0
    #define ONE_EMOD    0  //1
    #define TWO_EMOD    1  //2
    #define THREE_EMOD  2  //3
    #define FOUR_EMOD   4  //4
    #define FIVE_EMOD   5  //5
    #define SIX_EMOD    6  //6
    #define SEVEN_EMOD  8  //7
    #define EIGHT_EMOD  9  //8
    #define NINE_EMOD   10 //9
    #define A_EMOD  12 //10
    #define B_EMOD  14 //11
    #define C_EMOD  3  //12
    #define D_EMOD  7  //13
    #define E_EMOD  11 //14
    #define F_EMOD  15 //15
    
    #define MODE0CHAR   0
    #define MODE1CHAR   1
    #define MODE2CHAR   2
    #define MODE3CHAR   3
    #define MODE4CHAR   4
    
    #define SLETTER 0
    #define CLETTER 1
#endif
