#define MAX_LETTERS 27

typedef unsigned short WORD;
typedef char  BYTE;

struct morse_code {
    char ch;
    BYTE len;
    WORD code;
};

extern void get_morse_code (char letter, WORD * code, BYTE * len);
