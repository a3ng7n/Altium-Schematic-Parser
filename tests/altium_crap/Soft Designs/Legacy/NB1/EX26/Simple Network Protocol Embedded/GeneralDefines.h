
#define byte unsigned char

#define TRUE 1
#define FALSE 0

#define Bit0 0x01
#define Bit1 0x02
#define Bit2 0x04
#define Bit3 0x08
#define Bit4 0x10
#define Bit5 0x20
#define Bit6 0x40
#define Bit7 0x80

// The following macro evaluates to TRUE if the specified bit is set
#define CHECK_BIT(VarToCheck, BitName) ((VarToCheck & BitName) > 0)
