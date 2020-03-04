#include "swplatform.h"

// from wb_multi_manual.c
extern int test_convert( void );

// The adresses used in the #defines are defined in file generic_devices.h
#define BITCOUNT (*((uint32_t volatile *)WB_SINGLE_BASEADDRESS))
#define DO_SQR   (*((float volatile *)SQRCTRL_BASEADDRESS))

// Interface to swap() implemented in file swap.c
// Due to its calling qualifier the static stack of this function is
// mapped at address 0xFF000000.
__rtl __export __CC(wishbone, 0) uint32_t swap(uint32_t DAT_IN)
{
    // Empty function definition required
}


void main(void)
{
    volatile long    temp = 0;
    volatile long    sum = 0;
    volatile int     i = 0xfff;
    volatile int     ten = 0;
    volatile float   sqr_result_1;
    volatile float   sqr_result_2;
    volatile float   sqr_result_3;
    volatile float   sqr_result_4;
    volatile float   sqr_result_5;

    puts( "NB3000 Code Symbol Example , file " __FILE__ " compiled " __DATE__ ", " __TIME__ "\n\n" );

    // Access C Code Symbol WB_SINGLE
    puts( "1. WB_SINGLE\n" );
    while(i)
    {
        BITCOUNT = i;
        sum += BITCOUNT;
        --i;
    }
    // sum should have value 6*0x1000 here
    printf( "sum = 0x%08lx (should be 0x00006000)\n\n", sum );

    // Access C Code Symbol WB_MULTI
    puts( "2. WB_MULTI\n" );
    i = 0xfff;
    sum = 0;
    while(i)
    {
        temp = swap(i);
        BITCOUNT = temp;
        sum += BITCOUNT;
        --i;
    }
    // sum should again have value 6*0x1000 here
    printf( "sum = 0x%08lx (should be 0x00006000)\n\n", sum );

    // Access C Code Symbol WB_MULTI
    puts( "3. WB_MULTI_MANUAL\n" );
    // ten should be 10
    ten = test_convert();
    printf( "ten = %d (should be 10)\n\n", ten );

    // Access C Code Symbol SQRCTRL
    puts( "3. SQRCTRL\n" );
    DO_SQR = 2.0;
    DO_SQR = 1.001;
    DO_SQR = 3.14;
    DO_SQR = 3.0;
    DO_SQR = 2.718281828459;

    sqr_result_1 = DO_SQR;
    printf( "sqr_result_1 = %f (2.0^2)\n", sqr_result_1 );

    sqr_result_2 = DO_SQR;
    printf( "sqr_result_2 = %f (1.001^2)\n", sqr_result_2 );

    sqr_result_3 = DO_SQR;
    printf( "sqr_result_3 = %f (3.14^2)\n", sqr_result_3 );

    sqr_result_4 = DO_SQR;
    printf( "sqr_result_4 = %f (3.0^2)\n", sqr_result_4 );

    sqr_result_5 = DO_SQR;
    printf( "sqr_result_5 = %f (2.718281828459^2)\n", sqr_result_5 );

    puts( "\nFinished\n" );
}
