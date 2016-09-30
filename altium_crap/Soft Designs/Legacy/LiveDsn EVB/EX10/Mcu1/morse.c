#include "morse.h"

#define SPACE (MAX_LETTERS-1)

static struct morse_code __rom morse_map [MAX_LETTERS] = {
    {'A',  4, 0x000B},
    {'B',  8, 0x00D5},
    {'C',  9, 0x01AD},
    {'D',  6, 0x0035},
    {'E',  1, 0x0001},
    {'F',  8, 0x00AD},
    {'G',  7, 0x006D},
    {'H',  7, 0x0055},
    {'I',  3, 0x0005},
    {'J', 10, 0x02DB},
    {'K',  7, 0x006B},
    {'L',  8, 0x00B5},
    {'M',  5, 0x001B},
    {'N',  4, 0x000B},
    {'O',  8, 0x00DB},
    {'P',  9, 0x016D},
    {'Q', 10, 0x036B},
    {'R',  6, 0x002D},
    {'S',  5, 0x0015},
    {'T',  2, 0x0003},
    {'U',  6, 0x002B},
    {'V',  8, 0x00AB},
    {'W',  7, 0x005B},
    {'X',  9, 0x01AB},
    {'Y', 10, 0x035B},
    {'Z',  9, 0x01B5}, 
    {' ',  2, 0x0000}
};

void get_morse_code (char letter, WORD * code, BYTE * len)
{
    if ((letter>='A')&&(letter<='Z')) {
       *code = morse_map[letter-'A'].code;
       *len  = morse_map[letter-'A'].len;
    } else {
       *code = morse_map[SPACE].code;
       *len  = morse_map[SPACE].len;
    }
}

