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


#include "hware.h"
#include "nmt_kbd.h"

__rom unsigned char KeyTranslationTable[] = 
  {'1','2','3','C',
   '4','5','6','D',
   '7','8','9','E',
   'A','0','B','F'
  };

__bit KbHit = 0;
unsigned char LastKey;

//----------------------------------------------------------------------------------
// returns last key that was pressed
// KEY_TEST if the test key was pressed
// if 'Format'==KEY_FORMAT_SCANCODE, a scan code is returned as follows:
//   bits 0..1 indicate the column 
//   bits 2..3 indicate the row
//   bit 4 is always set
// if 'Format'==KEY_FORMAT_ASCII, the ASCII representation of the key on the kbd
//   is returned, eg. '1' for top left, 'F' for bottom right
// Uses:
// Globals LastKey and KbHit
//----------------------------------------------------------------------------------
unsigned char GetKey(unsigned char Format)
{ 
  register unsigned char retval=LastKey; // function return value
  if(KEY_TEST != retval)
  {
    switch(Format)
    {
      case KEY_FORMAT_ASCII:
        retval = KeyTranslationTable[retval];   // look ASCII value up in table 
      break;  
      case KEY_FORMAT_SCANCODE:
        retval |= 0x10;                          // set bit 4
      break;
      default:   
      break;
    }
  } // if
  KbHit = 0;
  return (retval);
}


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
unsigned char Kbd_DoScan(void)
{   
    static __bit AllKeysOff = 1;  // flag for key release
    register unsigned char row;
    register unsigned char columndata;
    for(row = 0; row < 4; row ++)
    {
       KEY_PORT = ~(1 << row);    // output low bit to scan row
       columndata = KEY_PORT;  // read in pattern for current row
       columndata ^=0xFF;      // invert  --> active keys have bit set
       if(columndata & 0x80)   // top Bit set  --> Test/Reset is down
       {
          columndata = KEY_TEST;   // top bit set --> Test Key Code
       }
       else
       {
         columndata &= 0x0F;     // mask top bits       
       }
       if(columndata)          // any other keys are down?
       {
          switch(columndata)
          {
                         //  translate bitnumber into column number 
             case 1:     //  this will make the lower two bits of the scancode
             case 2:     //   
               columndata -= 1;
             break;
             case 4:
             columndata = 2;
             break;
             case 8:
             columndata = 3;
             break;
             case KEY_TEST:  
               row = 0;
             break;
             default:   // abort if more than one key is pressed
             return(0);
          }                           // assemble scan code
          columndata |= (row << 2);   // lower two nibbles are column, next two are row
          if(AllKeysOff)              // was it a new key?  TODO: debounce
          {
            LastKey = columndata;       // set global key variable
            KbHit = 1;                  // set KbHit flag
            AllKeysOff = 0;             // clear release flag
          }
          return(columndata);         // TODO: look up in lookup table to translate into key code
       }
    }  // at this point we know that no key is pressed
  AllKeysOff = 1;
  return (0);    
}
