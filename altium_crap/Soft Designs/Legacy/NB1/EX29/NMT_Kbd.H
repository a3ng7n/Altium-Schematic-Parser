//    _   _  __  __  _____     _  __ ____   ____
//   | \ | ||  \/  ||_   _|   | |/ /| __ ) |  _ \
//   |  \| || |\/| |  | |     | ' / |  _ \ | | | |
//   | |\  || |  | |  | |     | . \ | |_) || |_| |
//   |_| \_||_|  |_|  |_|_____|_|\_\|____/ |____/
//                      |_____|
// (c) 2003 Altium
// Started: 17.11.2003 Ch.Weimann
// Keyboard and Test Button Scan Routines for 
// the Altium Nanoboard Tester

#ifndef __NBT_KBD_H__
#define __NBT_KBD_H__  
// #include "hware.h"

extern __rom unsigned char KeyTranslationTable[];

enum {KEY_TEST = 'T'};     // code returned for test key both as scancode and ASCII
enum {KEY_FORMAT_SCANCODE, KEY_FORMAT_ASCII};

extern __bit KbHit;        // indicates that a key was pressed
extern unsigned char LastKey;
//----------------------------------------------------------------------------------
// returns last key that was pressed
// KEY_TEST if the test key was pressed
// if 'Format'==KEY_FORMAT_SCANCODE, a scan code is returned as follows:
//   bits 0..1 indicate the column 
//   bits 2..3 indicate the row
//   bit 4 is always set
// if 'Format'==KEY_FORMAT_ASCII, the ASCII representation of the key on the kbd
//   is returned, eg. '1' for top left, 'F' for bottom right
//----------------------------------------------------------------------------------
unsigned char GetKey(unsigned char Format);


//------------------------------------------------------------------------------
// scans the Keyboard
// returns: 
// 0 if no key was pressed
// scankey of the cuurent keyboard scan:
//   bits 0..1 indicate the column 
//   bits 2..3 indicate the row
// Must be called regularly to scan the KBD
// if a key has been pressed, set global bit KbHit and global LastKey
//------------------------------------------------------------------------------
unsigned char Kbd_DoScan(void);

#endif // __NBT_KBD_H__




