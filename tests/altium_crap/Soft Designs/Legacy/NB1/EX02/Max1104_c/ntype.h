#ifndef ntype_h
#define ntype_h

    #define TRUE 1
    #define FALSE 0

    #define TXDATA __xdata
    #define TCODE __rom

    #define ENABLE_INTERRUPTS() EAL = TRUE
    #define DISABLE_INTERRUPTS() EAL = FALSE

//    #define P3_0  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b0)
//    #define P3_1  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b1)
//    #define P3_3  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b3)
//    #define P3_5  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b5)

    #define CLOCK_FREQ 30000000

    #define WORD unsigned short
    #define BYTE unsigned char
    #define word unsigned short
    #define byte unsigned char


#endif
