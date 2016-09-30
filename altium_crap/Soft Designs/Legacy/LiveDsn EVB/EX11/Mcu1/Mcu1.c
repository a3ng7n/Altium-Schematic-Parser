/* Binary Counter: -- C Programming Language Example */

#include "morse.h"
#include <string.h>


static void delay (int loops, int scale)
{
    for (int j = 0; j<scale; j++){
        for (int i = 0; i<loops; i++){
          __asm ("nop");
        }
    }
}


static void beep (unsigned int length)
{
    unsigned int i;
    unsigned char on=P1;

    for (i=0; i<length; i++){
        P1 = on & 0xfe;
        delay(10,1);
        P1 = on | 1;
        delay(180, 1);
    }
}

static void dash ()
{
    P0 = 0xF0;
    beep (2000);
    P0 = 0x00;

}

static void dot ()
{
    P0 = 0x0F;
    beep (400);
    P0 = 0x00;
}

static void inter_char_delay ()
{
    P0 = 0x00;
    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);
//    delay (100, 10000);

}

static void inter_word_delay ()
{
    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);
//    delay (100, 30000);

}

void main (void)
{
    BYTE i, len;
    WORD code;
    static char __rom message[] = "ALTIUM NEXAR DEMO";
    char __rom *p;

    PSW = 0x00;

    while (1) {
       for (p=message; *p; p++) {
          get_morse_code (*p, &code, &len);
          for (i=len; i>=0; i--) {
             if ( (code & (1 << i)) && (code & (1 << (i-1)))){
                dash ();
                i--;
             } else if (code & (1 << i)) {
                dot ();
             }
             else {
                inter_char_delay ();
             }
          }
       }
       inter_word_delay ();
    }
}


