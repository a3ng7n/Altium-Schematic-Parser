
// Keypad Codes
#define Key_1        (byte)0x01
#define Key_2        (byte)0x02
#define Key_3        (byte)0x03
#define Key_C        (byte)0x0C
#define Key_4        (byte)0x04
#define Key_5        (byte)0x05
#define Key_6        (byte)0x06
#define Key_D        (byte)0x0D
#define Key_7        (byte)0x07
#define Key_8        (byte)0x08
#define Key_9        (byte)0x09
#define Key_E        (byte)0x0E
#define Key_A        (byte)0x0A
#define Key_0        (byte)0x00
#define Key_B        (byte)0x0B
#define Key_F        (byte)0x0F
#define Key_Up       Key_1
#define Key_Left     Key_4
#define Key_Right    Key_6
#define Key_Down     Key_9
#define Key_INVALID  (byte)0xFF

// Globally accessible Keypad functions
void keypadInit(void);
byte keyScan(void);

