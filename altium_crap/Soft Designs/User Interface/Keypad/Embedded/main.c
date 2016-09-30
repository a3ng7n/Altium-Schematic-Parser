#include <stdio.h>
#include <drv_keypad.h>
#include "devices.h"

drv_keypad_t* keypad;

void main(void)
{
    unsigned char  ch;
    unsigned char* ascii_lookup;

    printf("Keypad example\n");
    printf("As you use the keypad the keys will be displayed in this terminal.\n");

    keypad       = keypad_open(KEYPAD);
    ascii_lookup = keypad_get_ascii_lookup_table(keypad);

    while (1)
    {
        ch = keypad_get_next_key(keypad);
        printf("%c\n", ascii_lookup[ch]);
    }
}
