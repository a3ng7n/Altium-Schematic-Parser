/*********************************************************************
 *
 * UART8 Example, showing how to handle transmission errors
 *
 *********************************************************************/

#include <stdio.h>
#include <swplatform.h>

// Serial communication parameters, change to check what's happening
#define BAUDRATE    115200
#define PARITY      UART8_NO_PARITY
//#define PARITY      UART8_ODD_PARITY
//#define PARITY      UART8_USER_PARITY
//#define PARITY      UART8_EVEN_PARITY
#define DATABITS    8
#define STOPBITS    1

// Pointers to swplatform device driver data
ioport_t * gpio;
uart8_t * uart;

// Local prototypes
static void init( void );
static void transmit( uint16_t val );

// Main, do the tests:

void main( void )
{
    int c;
    init();

    // Send characters that are okay for N8x (no parity, 8 databits, 1 or 2 stopbits)

    transmit( 0xF007 | (0xAA << 4) );   // data LSB to MSB = 0101.0101, followed by 1111. Thus no error on N8x or O8x
    transmit( 0xF007 | (0xAB << 4) );   // data LSB to MSB = 1101.0101, followed by 1111. Thus no error on N8x or E8x
    transmit( 0x8001 );                 // a break condition
    transmit( 0xF007 );                 // a zero

    // Send out characters that are okay for E8x (even parity, 8 databits).
    transmit( 0xE007 | (0xAA << 4) );   // data LSB to MSB = 0101.0101, followed by 0111. Parity = 0 is okay for E8x, framing error for N8x
    transmit( 0xF007 | (0xAB << 4) );   // data LSB to MSB = 1101.0101, followed by 1111. Parity = 1 is okay for E8x

    // Send out characters that are okay for O8x (odd parity, 8 databits)
    transmit( 0xF007 | (0xAA << 4) );   // data LSB to MSB = 0101.0101, followed by 1111. Parity = 1 is okay for O8x
    transmit( 0xE007 | (0xAB << 4) );   // data LSB to MSB = 1101.0101, followed by 0111. Parity = 0 is okay for O8x, framing error on N8x

    // Frame errors for 8-bit data with parity, might generate ghost restart for no parity
    transmit( 0xD007 | (0xAA << 4) );   // data LSB to MSB = 0101.0101, followed by 1011. Parity = 1 is okay for O8x but stopbit = error
    transmit( 0xC007 | (0xAA << 4) );   // data LSB to MSB = 0101.0101, followed by 0011. Parity = 0 is okay for E8x but stopbit = error


    // Check UART8 receiver:
    for (;;)
    {
        if ( c = uart8_getchar( uart ), c == -1 ) continue;
        printf( "Received %02X  ", c & 0xFF );              // Strip off any error indicators and print received value
        if ( c & UART8_BREAK ) puts( "BREAK" );             // BREAK detected, value should be 00
        else if ( c & UART8_FRAMERR ) puts( "FRAMERR" );    // Framing error detected
        else if ( c & UART8_PARERR ) puts( "PARERR" );      // Parity error detected (note: in user parity, this does not indicate an ERROR)
        else putchar( '\n' );
    }
}

/*******************************************************************\
 * Initialize the drivers & hardware
 */

static void init( void )
{
    const char * parstr[] = { "no", "odd", "even", "user" } ;

    puts( "UART Example, compiled " __DATE__ ", " __TIME__ );
    printf( "UART configuration: %d bps, %s parity, %d databits, %d stopbits\n", BAUDRATE, parstr[PARITY], DATABITS, STOPBITS );

    if ( uart = uart8_open( DRV_UART8_1 ), !uart )
    {
        puts( "Unable to open UART driver" );
        abort();
    }
    printf( "UART component version %d\n", uart8_get_version( uart ));

    if ( gpio = ioport_open( DRV_IOPORT_1 ), !gpio )
    {
        puts( "Unable to open I/O port driver" );
        abort();
    }

    // Setup the clock divisor for the shift register. The shift register will clock out 1 bit per CLKDIV cycles
    ioport_set_value( gpio, X_CLKDIV, (50000000 + BAUDRATE/2) / BAUDRATE );
    ioport_set_value( gpio, X_CLKLOAD, -1 );
    ioport_set_value( gpio, X_CLKLOAD, 0 );

    // Initialize the UART. Note that this writes the baudrate register and thus resets the UART internal FIFO's too!
    uart8_set_parameters( uart, BAUDRATE, PARITY, DATABITS, STOPBITS );
    uart8_ignore( uart, 0 );    // Do not ignore any errors

}

/*********************************
 * This function writes a 16-bit value to the
 * shift register and starts sending it (LSB first!)
 *
 * Usage hints:
 *  - The least significant nibble should always be 0111b (3 x quiet on the line, than a startbit)
 *  - Nibbles 1 and 2 contain the payload (1 byte)
 *  - The most significant nibble should describe the trailer (optional parity on LSB, than stopbits and quiet line again)
 *
 * For example: transmit( 0xF557 )
 *          will send 1110.1010.1010.1111 => startbit, value 55, than optionally a parity bit ('1') and stopbits
 */

static void transmit( uint16_t val )
{
    while( ioport_get_value( gpio, X_READY ) == 0 )  ;
    ioport_set_value( gpio, X_VAL, val );
    ioport_set_value( gpio, X_START, -1 );
    while( ioport_get_value( gpio, X_READY ) )  ;
    ioport_set_value( gpio, X_START, 0 );
}


