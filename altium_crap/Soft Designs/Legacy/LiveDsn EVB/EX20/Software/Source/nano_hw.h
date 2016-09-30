#ifndef __NANO_HW_H__
#define __NANO_HW_H__

#ifndef __INTNO
#define __INTNO(nr) (((nr)<<3)+3)
#endif

#ifdef BUILD_KEYBOARD
#define BUILD_PS2_B
#endif

#ifdef BUILD_MOUSE
#define BUILD_PS2_A
#endif

// PS/2 outputs    
#define PS2BSTROBE   P0_7
#define PS2BRESET    P0_6
#define PS2ASTROBE   P0_5
#define PS2ARESET    P0_4

// PS/2 inputs
#define PS2BBUSY     P0_7
#define PS2ABUSY     P0_6

/*
 *     PS2B, EXT0, Keyboard Port
 */

// keyboard scancodes
#define EXKEY      0xE0
#define BREAKCODE  0xF0
#define KEY_ESC    0x76
#define KEY_1      0x16
#define KEY_2      0x1E
#define KEY_3      0x26
#define KEY_A      0x1C
#define KEY_B      0x32
#define KEY_E      0x24
#define KEY_K      0x42
#define KEY_M      0x3A
#define KEY_N      0x31
#define KEY_P      0x4D
#define KEY_S      0x1B
#define KEY_X      0x22
#define KEY_Y      0x35
#define KEY_LEFT   0x6B
#define KEY_RIGHT  0x74
#define KEY_UP     0x75
#define KEY_DOWN   0x72
#define KEY_SPACE  0x29

#define KEY_LSHIFT      0x12
#define KEY_RSHIFT      0x59
#define KEY_LCTRL       0x14
#define KEY_LALT        0x11
#define EX_KEY_RALT     KEY_LALT
#define EX_KEY_RCTRL    KEY_LCTRL
#define EX_KEY_INS      0x70
#define EX_KEY_HOME     0x6C
#define EX_KEY_PGUP     0x7D
#define EX_KEY_DEL      0x71
#define EX_KEY_END      0x69
#define EX_KEY_PGDN     0x7A
#define EX_KEY_UP       0x75
#define EX_KEY_LEFT     0x6B
#define EX_KEY_DOWN     0x72
#define EX_KEY_RIGHT    0x74
#define KEY_CAPS        0x58
#define EX_KEY_ENTER    0x5A

#ifdef BUILD_PS2_B
#define ENABLE_KEYBOARD_INT    \
    PX1 = 1;                   \
    EX1 = 1
#define INIT_KEYBOARD_PORT     \
    PS2BSTROBE = 1;            \
    PS2BRESET = 1;             \
    PS2BRESET = 0
extern volatile __xdata unsigned char lastKeyPressed;
extern volatile __xdata unsigned char key[];
#else
#define ENABLE_KEYBOARD_INT
#define INIT_KEYBOARD_PORT
#endif

/*
 *     PS2A, EXT1, Mouse Port
 */
 
#ifdef BUILD_PS2_A
#define ENABLE_MOUSE_INT       \
    PX0 = 1;                   \
    EX0 = 1
#define INIT_MOUSE_PORT        \
    PS2ASTROBE = 1;            \
    PS2ARESET = 1;             \
    PS2ARESET = 0
#define MOUSE_STROBE           \
    PS2ASTROBE = 0;            \
    PS2ASTROBE = 1
#define SEND_MOUSE_CMD(cmd)    \
    mouseAck = 0;              \
    P2 = cmd;                  \
    MOUSE_STROBE;              \
    while (!mouseAck)          \
       ;
    //SEND_MOUSE_CMD(0xFF);   // reset command
#define INIT_MOUSE_COMMS                            \
    SEND_MOUSE_CMD(0xF3);   /* set sample rate */   \
    SEND_MOUSE_CMD(0x0A);   /* 10 samples/sec */    \
    SEND_MOUSE_CMD(0xF4)    /* enable reporting */
#define MOUSE_ACKS_AFTER_RESET    3
#define MOUSE_DATA_BTN   0
#define MOUSE_Y_OV       (1<<7)
#define MOUSE_X_OV       (1<<6)
#define MOUSE_Y_SIGN     (1<<5)
#define MOUSE_X_SIGN     (1<<4)
#define MOUSE_BTN_MID    (1<<2)
#define MOUSE_BTN_RT     (1<<1)
#define MOUSE_BTN_LT     (1<<0)
#define MOUSE_DATA_X     1
#define MOUSE_DATA_Y     2
extern volatile signed char lastMouseData[];
extern volatile __bit mouseAck;
#else
#define ENABLE_MOUSE_INT
#define INIT_MOUSE_PORT
#define INIT_MOUSE_COMMS
#endif

/*
 *    SERIAL PORT
 */

#ifdef BUILD_SERIAL
#define ENABLE_SERIAL_INT      ES = 1
#define INIT_SERIAL_PORT       \
        PCON = 0x80;           \
        SCON = 0x50;           \
        TH1 = 240;             \
        TL1 = 0;               \
        TR1 = 1;
#else
#define ENABLE_SERIAL_INT
#define INIT_SERIAL_PORT
#endif

// video hardware

#define SCREEN_X 64
#define SCREEN_Y 60

// colours

#define COLOUR_BLACK     0
#define COLOUR_RED       1
#define COLOUR_GREEN     2
#define COLOUR_YELLOW    (COLOUR_GREEN+COLOUR_RED)
#define COLOUR_BLUE      4
#define COLOUR_MAGENTA   (COLOUR_BLUE+COLOUR_RED)
#define COLOUR_CYAN      (COLOUR_BLUE+COLOUR_GREEN)
#define COLOUR_BROWN     (COLOUR_BLUE+COLOUR_GREEN+COLOUR_RED)

#define BRIGHT(c)        (8|(c))
#define COLOUR_GRAY      (BRIGHT(COLOUR_BLACK))
#define COLOUR_WHITE     (BRIGHT(COLOUR_BROWN))

#define BLANK_TILE       0x7F

// sprites

#define NUM_SPRITES   8

// must update high byte of y register *first*
#define PUTSPRITE(s,x,y)             \
    reg[((s)<<4)+0] = (x)>>1;        \
    reg[((s)<<4)+2] = (y)>>8;        \
    reg[((s)<<4)+1] = y
    
#define SPRITE_FLIP(s,x,y)           \
    reg[((s)<<4)+3] = (((y)<<1)|(x))
#define SPRITE_FLIP_X(s,x)           \
    SPRITE_FLIP(s,x,0)
#define SPRITE_FLIP_Y(s,y)           \
    SPRITE_FLIP(s,0,y)

#define SPRITE_COLOUR(s,c1,c2,c3)    \
    reg[((s)<<4)+4] = (c2<<4)|c1;    \
    reg[((s)<<4)+5] = c3

#define SPRITE_N(s,n)                \
    reg[((s)<<4)+6] = n

#define COLLISION (P1 & 0x07)
#define IS_COLLISION(i)              \
    ((i = COLLISION) != 0x07)

#define SOUND_DIR_REG    P0_2

#ifdef MULTI_MCU
#define SOUND_DIR(n)
#define PLAY_SOUND(start,end)
#define PLAY_SOUND_EX(start,end,dir)
#else
#define SOUND_DIR(n)     SOUND_DIR_REG = n
#define PLAY_SOUND(start,end)           \
    sndReg[0] = (unsigned char)start;   \
    sndReg[1] = start >> 8;             \
    sndReg[2] = (unsigned char)end;     \
    sndReg[3] = end >> 8;               \
    P0_3 = 0;                           \
    P0_3 = 1
#define PLAY_SOUND_EX(start,end,dir)    \
    SOUND_DIR(dir);                     \
    PLAY_SOUND(start,end)
#endif

#define SOUND_DIR_UP     SOUND_DIR(1)
#define SOUND_DIR_DOWN   SOUND_DIR(0)

#ifdef BUILD_LCD
#define LCD_DATA             P1
#define LCD_BUSY             (P0_5 != 0)
#define INIT_LCD             \
        P3_5 = 1
#define LCD_STROBE           \
        P3_5 = 0;            \
        P3_5 = 1
#define LCD_PUTCHAR(x,y,c)   \
        LCD_DATA = c;        \
        P3 = (x) | (y<<4);   \
        LCD_STROBE
#else
#define INIT_LCD
#define LCD_BUSY             (0)
#define LCD_PUTCHAR(x,y,c)
#endif

extern __xdata char *reg;
extern __xdata char *sprite;
extern __xdata char *tileMap;
extern __xdata char *tile;
extern __xdata char *collide;
#ifndef MULTI_MCU
extern __xdata char *sndReg;
#endif

#endif

