#include <stdio.h>
#include <stdlib.h>

#include "npd.h"
#include "interruptnm.h"

//..............................................................................
volatile BYTE  Var_PORTA   __at(PORTA);
volatile BYTE  Var_PORTB   __at(PORTB);
volatile BYTE  Var_PORTC   __at(PORTC);
volatile BYTE  Var_PORTD   __at(PORTD);
//..............................................................................

//..............................................................................
#define  LCD_Data     Var_PORTB
#define  LCD_Status   Var_PORTB
#define  LCD_Address  Var_PORTC
#define  ControlBits  Var_PORTA  //Bit 0 = LCD Strobe   Bit 1 = KeyPad Reset
#define  KeyPad_Data  Var_PORTA
#define  LED_Data     Var_PORTD
//..............................................................................

//..............................................................................
volatile BYTE PCON_MEMPOS    __at(PCON_ID  );
volatile BYTE S0CON_MEMPOS   __at(S0CON_ID );
volatile BYTE S0BUF_MEMPOS   __at(S0BUF_ID );
volatile BYTE S0RELL_MEMPOS  __at(S0RELL_ID);
volatile BYTE S0RELH_MEMPOS  __at(S0RELH_ID);
volatile BYTE ADCON_MEMPOS   __at(ADCON_ID );
//..............................................................................

//..............................................................................
static BYTE  welcome           [] = "Welcome to      Serial";                   //
static BYTE  smessage          [] = "Sent mode";                                //
static BYTE  rmessage          [] = "Receive mode";                             //
//..............................................................................//

//..............................................................................
char bytemap[8]  = {1,2,4,8,16,32,64,128};
//..............................................................................

//..............................................................................
unsigned short gunblockrec;
BYTE           gledstatus;

volatile BYTE  gtransmitti;
unsigned short grecdatcount;

#define TRSIZE  33
BYTE           m_TextRam[TRSIZE];

BYTE gcurkey;

void ResetKeypad()
{
    ControlBits = 0;
    ControlBits = 2;
    ControlBits = 0;
}

void BusyLCD(void)
{
    BYTE lwait = 1;

    while (lwait == 1)
    {
        lwait = LCD_Status;
        lwait &= bytemap[0];
    }
}

void ClearLCD(BYTE ltc, BYTE csc)
{
    BYTE i;
    for (i=0;i<32;i++)
    {
         BusyLCD();
         LCD_Address        = i;
         LCD_Data           = SPACE;
         ControlBits        = 0;
         ControlBits        = 1;
         ControlBits        = 0;
    }
}

void SentCharOnLCD(BYTE pos,BYTE c)
{
    BusyLCD();
    LCD_Address       = pos;
    LCD_Data          = c;
    ControlBits       = 0;
    ControlBits       = 1;
    ControlBits       = 0;
}

void ClearS0CON(BYTE val)
{
    val &= ~bytemap[0];
    val &= ~bytemap[1];
    S0CON_MEMPOS = val;
}

/*****************************************************************************
|*
|*  Function:           InterruptNm
|*
|*  Description:
|*
|*      This function will be called every non maskable interrupt.
|*
 */
void InterruptNm( void )
{
    BYTE lcursocon;
    BYTE lcursocon0;
    BYTE lcursocon1;
    unsigned short i;

    lcursocon = S0CON_MEMPOS;
    lcursocon0 = lcursocon & bytemap[0];
    lcursocon1 = lcursocon & bytemap[1];

    if (lcursocon1 == 2)
    {
       gtransmitti = 1;
       ClearS0CON(lcursocon);
    }
    else
    {
       if ((lcursocon0 == 1) && (gunblockrec == 1))
       {
		  if (grecdatcount == 0)
      	  {
      	      ClearLCD(0,0);
      	  }
          m_TextRam[grecdatcount] = S0BUF_MEMPOS;
          SentCharOnLCD(grecdatcount,m_TextRam[grecdatcount]);
          grecdatcount++;
          ClearS0CON(lcursocon);
          if (grecdatcount == TRSIZE - 1)
          {
              grecdatcount = 0;
          }
       }
    }
}

void TransmittData(void)
{
    unsigned short i;

    for (i=0;i<grecdatcount;i++)
    {
       S0BUF_MEMPOS = m_TextRam[i];
       while (gtransmitti == 0)
       {
       }
       gtransmitti = 0;
    }
}

void InidDataMemW(void)
{
    unsigned short i;

    for (i=0;i<TRSIZE;i++)
    {
        m_TextRam[i] = '-';
    }
}

void InitialiseSerial(void)
{
    S0RELL_MEMPOS = S0RELL_INI;
    S0RELH_MEMPOS = S0RELH_INI;
    S0CON_MEMPOS  = S0CON_INI;
    PCON_MEMPOS   = PCON_INI;
    ADCON_MEMPOS  = ADCON_INI;
}


void SentMessageOnLCD(BYTE lcdline, TCODE KCODE BYTE * pData )
{
    BYTE pos;
    register BYTE c;

    pos = lcdline*16;
    while ((c=*pData++))
    {
       SentCharOnLCD(pos,c);
       pos++;
    }
}

BYTE KeyPadDebaunce(void)
{
    BYTE lcurkeys;
    BYTE lpressdoo;


    BYTE lmask = 15;
    BYTE lmaskb4 = 16;
    lcurkeys = KeyPad_Data;
    ResetKeypad();

    lpressdoo = lcurkeys & lmaskb4;
    gcurkey = lcurkeys & lmask;
  
    return lpressdoo;
}

void ScanKeyPad(void)
{
    BYTE lpressdoo;

    lpressdoo = KeyPadDebaunce();
    if (lpressdoo == 16)
    {
        switch (gcurkey)
        {
            case ONE_EMOD:
                ClearLCD(0,0);
                SentMessageOnLCD(0,smessage);
                grecdatcount = 32;
                gunblockrec = 0;
                TransmittData();
                gledstatus = 0x01;
            break;
            case TWO_EMOD:
            if (gunblockrec == 0)
            {
                gunblockrec = 1;
                ClearLCD(0,0);
                SentMessageOnLCD(0,rmessage);
                grecdatcount = 0;
                gledstatus = 0x02;
            }
            break;
            case THREE_EMOD:
                ClearLCD(0,0);
                InidDataMemW();
                grecdatcount = 0;
                gunblockrec = 0;
                gledstatus = 0x04;
            break;
        }
    }
    
    LED_Data = gledstatus;
}

void Initialise(void)
{
    ResetKeypad();
    ClearLCD(0,0);
    SentMessageOnLCD(0,welcome);
    gtransmitti = 0;
    grecdatcount = 0;
    gcurkey = 0;
    gledstatus = 0;
    gunblockrec = 0;
    InitialiseSerial();
    InidDataMemW();
    LED_Data = gledstatus;
}

void main(void)
{
    Initialise();

    while (1)
    {
       ScanKeyPad();
    }
}
