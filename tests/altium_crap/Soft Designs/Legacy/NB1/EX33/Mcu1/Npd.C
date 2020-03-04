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
static BYTE  welcome           [] = "Welcome to TeEd";                                 //
static BYTE  pressaforhelp     [] = "Press A for Help";                                //
static BYTE  help_arrows_scroll[] = "Help - Use arrows to scroll:";                    //
static BYTE  help_b            [] = "B -send or receive message";                      //
static BYTE  help_c            [] = "C -clr: all data-SM,left char-EM";                //
static BYTE  help_d            [] = "D -mode: scroll-SM, edit-EM";                     //
static BYTE  help_e            [] = "E -change letters: small/capital";                //
static BYTE  help_f            [] = "F -char table: mode 0 1 2 3 4";                   //
static BYTE  help_end          [] = "End of Help";                                     //
static BYTE  sendrec_msg       [] = "Press: 0 send, 1 act. reception";                 //
//..............................................................................//

//..............................................................................
char bytemap[8]  = {1,2,4,8,16,32,64,128};
//..............................................................................

//..............................................................................
BYTE           gnrscline    = 0;
unsigned short gcurmode     = 0;
unsigned short gscrollcount = 0;

unsigned short gunblockrec;
BYTE           gledstatus;

BYTE           gmodesfedit;
BYTE           guplowcedit;

volatile BYTE  gtransmitti;
unsigned short grecdatcount;
BYTE           grecdatcount32;
BYTE           gchartodisplay;

#define TRSIZE  2048
BYTE           m_TextRam[TRSIZE];

BYTE gferstin;
BYTE gcurkey;

BYTE gfkdprot;
BYTE gfkdrep;
BYTE gfkdrephv;
BYTE gcharfrel;
BYTE grepcon;
BYTE gpressdoo;

int IncGrecdatcount32(unsigned short val)
{
    if (val < 31)
    {
        val++;
    }
        else
    {
        val = 0;
    }
    return val;
}

void ResetKeypad()
{
    ControlBits = 0;
    ControlBits = 2;
    ControlBits = 0;
}

BYTE DelayKB(void)
{
    BYTE lcurkeys = 0;
    BYTE lcurkeysp = 0;
    unsigned short i = 0;

    while ((lcurkeys == 0) && (i < 500))
    {
        lcurkeys = KeyPad_Data;
        lcurkeysp = lcurkeys;
        lcurkeys &= bytemap[4];
        i++;
    }
    return lcurkeysp;
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
       if ((lcursocon0 == 1) && (grecdatcount < TRSIZE - 1))
       {
          if (gunblockrec == 1)
          {
              m_TextRam[grecdatcount] = S0BUF_MEMPOS;
              SentCharOnLCD(grecdatcount32,m_TextRam[grecdatcount]);
              grecdatcount32 = IncGrecdatcount32(grecdatcount32);
              grecdatcount++;
              ClearS0CON(lcursocon);
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
        m_TextRam[i] = SPACE;
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


void ClearLCD(BYTE ltc, BYTE csc)
{
    BYTE i;
    BYTE l = 0;
    BYTE k = 32;
    switch (ltc)
    {
        case 0:
            l = 0;
            k = 32;
        break;

        case 1:
            l = 0;
            k = 15;
        break;

        case 2:
            l = 15;
            k = 32;
        break;

        case 3:
            l = csc;
            k = csc + 1;
        break;
    }

    for (i=l;i<k;i++)
    {
         BusyLCD();
         LCD_Address        = i;
         LCD_Data           = SPACE;
         ControlBits        = 0;
         ControlBits        = 1;
         ControlBits        = 0;
    }
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

void EditModeSentCharOnLCDAndToMem(BYTE c, unsigned short mempos)
{
    m_TextRam[mempos] = c;

    if (grecdatcount < TRSIZE)
    {
        if ((grecdatcount32 == 0) && (grecdatcount != 0))
        {
            ClearLCD(0,0);
        }
        SentCharOnLCD(grecdatcount32,c);
        grecdatcount++;
        grecdatcount32 = IncGrecdatcount32(grecdatcount32);
    }
}

void EditModeClearCharFromLCDAndFromMem(void)
{
    BYTE i;
    unsigned short imem;

    if (grecdatcount > 0)
    {
       grecdatcount--;

        if (grecdatcount32 > 0)
        {
            grecdatcount32--;
            SentCharOnLCD(grecdatcount32,SPACE);
            m_TextRam[grecdatcount] = SPACE;
            if ((grecdatcount32 == 0) && (grecdatcount != 0))
            {
             imem = grecdatcount - 32;
                for (i=0;i<31;i++)
                {
                   BusyLCD();
                   LCD_Address = i;
                   LCD_Data = m_TextRam[imem];
                   ControlBits = 0;
                   ControlBits = 1;
                   ControlBits = 0;
                 imem++;
             }
            }
        }
        else
        {
          grecdatcount32 = 31;
          SentCharOnLCD(grecdatcount32,SPACE);
            m_TextRam[grecdatcount] = SPACE;
        }
    }
}

void ScrollModeSentTextOnLCD(unsigned short memposl, unsigned short memhmchar)
{
    unsigned short i;

    //display data on lcd
    for (i=memposl;i<(memposl + memhmchar);i++)
    {
       SentCharOnLCD((i - memposl), m_TextRam[i]);
    }
    //display spaces on the rest of places
    for (i=memhmchar;i<32;i++)
    {
       SentCharOnLCD(i, SPACE);
    }
}

void SwitchOnScrollModeSentTextOnLCD(void)
{
    if (grecdatcount > 31)
    {
        gscrollcount = grecdatcount - grecdatcount32;
    }
    else
    {
        gscrollcount = 0;
    }
    ScrollModeSentTextOnLCD(gscrollcount,grecdatcount32);
}



void HelpMessages(void)
{
    switch (gnrscline)
    {
        case 0:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_arrows_scroll);
            break;

        case 1:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_b);
            break;

        case 2:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_c);
            break;

        case 3:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_d);
            break;

        case 4:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_e);
            break;

        case 5:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_f);
            break;

        case 6:
            ClearLCD(0,0);
            SentMessageOnLCD(0,help_end);
            break;
    }
}


BYTE KeyPadDebaunce(void)
{
    BYTE lcurkeys;

    BYTE lmask = 15;
    BYTE lmaskb4 = 16;

    lcurkeys = DelayKB();
    ResetKeypad();

        gcharfrel = 1;

        gpressdoo = lcurkeys & lmaskb4;
        if (gpressdoo == 0)
        {
            gfkdprot = 0;
            gfkdrephv = 30;
        }
        gcurkey = lcurkeys & lmask;
        if (gfkdprot == 0)
        {
            return gpressdoo;
        }
        else //for keys repetition
        {
            if (gfkdrep < gfkdrephv)
            {
                gfkdrep++;
            }
            else
            {
                gfkdrep = 0;
                gfkdprot = 0;
                gfkdrephv = 2;
            }
        }
    return 0;
}

void ScanKeyPad(void)
{
    BYTE lpressdoo;
    BYTE lcurkey;
    BYTE gledstatustp;

    lpressdoo = KeyPadDebaunce();
    if ((lpressdoo == 16) || (gferstin == 1))
    {
        gfkdprot = 1;
        gferstin = 0;
        lcurkey = gcurkey;

        switch (gcurmode)
        {
            case NOP_MSM:
                ClearLCD(0,0);
                SentMessageOnLCD(0,welcome);
                SentMessageOnLCD(1,pressaforhelp);
                gcurmode = SCROLL_MSM;
                gledstatus |= bytemap[0%8];
            break;

            case SCROLL_MSM:
             switch (lcurkey)
             {
                case LARROW_SMOD:
                   if (gscrollcount > 0)
                   {
                       gscrollcount--;
                       ScrollModeSentTextOnLCD(gscrollcount, 32);
                   }
                break;

                case RARROW_SMOD:
                   if ((gscrollcount < (TRSIZE - grecdatcount32)) && (gscrollcount < (grecdatcount - grecdatcount32)))
                   {
                       gscrollcount++;
                       ScrollModeSentTextOnLCD(gscrollcount, 32);
                   }
                   else
                   {
                        ScrollModeSentTextOnLCD(gscrollcount, grecdatcount32);
                   }
                break;

                case UARROW_SMOD:
                   if (gscrollcount > 32)
                   {
                       gscrollcount = gscrollcount - 32;
                       ScrollModeSentTextOnLCD(gscrollcount, 32);
                   }
                   else
                   {
                      gscrollcount = 0;
                      ScrollModeSentTextOnLCD(0, 32);
                   }
                break;

                case DARROW_SMOD:
                   if ((gscrollcount < (TRSIZE - 32)) && (gscrollcount < (grecdatcount - 32)))
                   {
                       gscrollcount = gscrollcount + 32;
                       ScrollModeSentTextOnLCD(gscrollcount, 32);
                   }
                   else
                   {
                      ScrollModeSentTextOnLCD(gscrollcount, grecdatcount32);

                   }
                break;

                case CLR_SMOD:
                   gscrollcount = 0;
                   grecdatcount = 0;
                   grecdatcount32 = 0;
                   ClearLCD(0,0);
                   InidDataMemW();
                break;

                case SCROLEDIT_MOD:
                    gcurmode = EDIT_MSM;
                    gledstatus &= ~bytemap[0%8];
                    ClearLCD(0,0);
                    ScrollModeSentTextOnLCD(grecdatcount-grecdatcount32, grecdatcount32);
                break;

                case HELP_MOD:
                    gcurmode = HELP_MSM;
                    HelpMessages();
                break;

                case SERE_MOD:
                    gcurmode = SERE_MSM;
                    ClearLCD(0,0);
                    SentMessageOnLCD(0,sendrec_msg);
                break;
             }
            break;

            case EDIT_MSM:
                switch (lcurkey)
                {
                    case SCROLEDIT_MOD:
                        gcurmode = SCROLL_MSM;
                        gledstatus |= bytemap[0%8];
                        //SwitchOnScrollModeSentTextOnLCD();
                        if (grecdatcount > 31)
                        {
                            gscrollcount = grecdatcount - grecdatcount32;
                        }
                        else
                        {
                            gscrollcount = 0;
                        }
                    break;

                    case F_EMOD:
                        if (gmodesfedit == 4)
                        {
                            gmodesfedit= 0;
                        }
                        else
                        {
                            gmodesfedit++;
                        }
                        gledstatustp = gledstatus & 0x8F;
                        gledstatus = gledstatustp | (gmodesfedit*16);
                    break;

                    case E_EMOD:
                        if (guplowcedit == 1)
                        {
                            guplowcedit = 0;
                            gledstatus &= ~bytemap[7%8];
                        }
                        else
                        {
                            guplowcedit = 1;
                            gledstatus |= bytemap[7%8];
                        }
                    break;

                    case C_EMOD:
                        EditModeClearCharFromLCDAndFromMem();
                    break;

                    case ONE_EMOD:
                        gchartodisplay = '1';
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case TWO_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 50;
                        }
                        else
                        {
                            gchartodisplay = 64 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case THREE_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 51;
                        }
                        else
                        {
                            gchartodisplay = 67 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case FOUR_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 52;
                        }
                        else
                        {
                            gchartodisplay = 70 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case FIVE_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 53;
                        }
                        else
                        {
                            gchartodisplay = 73 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case SIX_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 54;
                        }
                        else
                        {
                            gchartodisplay = 76 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case SEVEN_EMOD:
                        if (gmodesfedit == MODE0CHAR)
                        {
                            gchartodisplay = 55;
                        }
                        else
                        {
                            gchartodisplay = 79 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case EIGHT_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 56;
                        }
                        else
                        {
                            gchartodisplay = 83 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case NINE_EMOD:
                        if (gmodesfedit == MODE0CHAR)
                        {
                            gchartodisplay = 57;
                        }
                        else
                        {
                            gchartodisplay = 86 + gmodesfedit + guplowcedit*32;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case A_EMOD:
                        if ((gmodesfedit == MODE0CHAR) || (gmodesfedit == MODE3CHAR) || (gmodesfedit == MODE4CHAR))
                        {
                            gchartodisplay = 65;
                        }
                        else
                        {
                            gchartodisplay = 41 + gmodesfedit;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case ZERO_EMOD:
                        if (gmodesfedit == MODE1CHAR)
                        {
                            gchartodisplay = 32;
                        }
                        else
                        {
                            gchartodisplay = 48;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;

                    case B_EMOD:
                        switch (gmodesfedit)
                        {
                            case MODE0CHAR:
                            case MODE3CHAR:
                            case MODE4CHAR:
                                gchartodisplay = 66;
                            break;

                            case MODE1CHAR:
                                gchartodisplay = 35;
                            break;

                            case MODE2CHAR:
                                gchartodisplay = 94;
                            break;
                        }
                        EditModeSentCharOnLCDAndToMem(gchartodisplay, grecdatcount);
                    break;
                }
            break;

            case HELP_MSM:
                switch (lcurkey)
                {
                    case HELP_MOD:
                        gcurmode = SCROLL_MSM;
                        SwitchOnScrollModeSentTextOnLCD();
                    break;

                    case SCROLEDIT_MOD:
                        gcurmode = SCROLL_MSM;
                        if (grecdatcount > 31)
                        {
                            ScrollModeSentTextOnLCD(gscrollcount, 32);
                        }
                        else
                        {
                            gscrollcount = 0;
                            ScrollModeSentTextOnLCD(0,grecdatcount);
                        }
                        gledstatus |= bytemap[0%8];
                    break;

                    case UARROW_SMOD:
                       if (gnrscline > 0)
                       {
                           gnrscline --;
                       }
                       HelpMessages();
                    break;

                    case DARROW_SMOD:
                       if (gnrscline < 7)
                       {
                          gnrscline++;
                       }
                       HelpMessages();
                    break;
                }
            break;

            case SERE_MSM:
                switch (lcurkey)
                {
                    case SEND_SRMOD:
                            gledstatus &= ~bytemap[0%8];
                            gledstatus &= ~bytemap[1%8];
                            gledstatus |= bytemap[2%8];
                            LED_Data = gledstatus;
                            TransmittData();
                            gledstatus |= bytemap[0%8];
                            gledstatus &= ~bytemap[2%8];
                            ClearLCD(0,0);
                            gcurmode = SCROLL_MSM;
                            SwitchOnScrollModeSentTextOnLCD();
                            gunblockrec = 0;
                    break;

                    case RECEIVE_SRMOD:
                        if (gunblockrec == 0)
                        {
                            gunblockrec = 1;
                            gledstatus &= ~bytemap[0%8];
                            gledstatus |= bytemap[1%8];
                            ClearLCD(0,0);
                            grecdatcount = 0;
                            grecdatcount32 = 0;
                        }
                        else
                        {
                            gunblockrec = 0;
                            ClearLCD(0,0);
                            gcurmode = SCROLL_MSM;
                            SwitchOnScrollModeSentTextOnLCD();
                            gledstatus |= bytemap[0%8];
                            gledstatus &= ~bytemap[1%8];
                        }
                    break;

                    case SERE_MOD:
                        ClearLCD(0,0);
                        gcurmode = SCROLL_MSM;
                        SwitchOnScrollModeSentTextOnLCD();
                        gledstatus |= bytemap[0%8];
                        gledstatus &= ~bytemap[1%8];
                        gunblockrec = 0;
                    break;

                    case SCROLEDIT_MOD:
                        ClearLCD(0,0);
                        gcurmode = SCROLL_MSM;
                        SwitchOnScrollModeSentTextOnLCD();
                        gledstatus |= bytemap[0%8];
                        gledstatus &= ~bytemap[1%8];
                        gunblockrec = 0;
                    break;

                }

            break;
        }
    }

    LED_Data = gledstatus;
}

void Initialise(void)
{
    gfkdprot = 0;
    gfkdrep = 0;
    gfkdrephv = 30;
    gcharfrel = 0;
    gpressdoo = 0;
    gtransmitti = 0;
    grecdatcount = 0;
    grecdatcount32 = 0;
    gcurmode = 0;
    gmodesfedit = 0;
    gferstin = 1;
    gcurkey = 0;
    gledstatus = 0;
    gunblockrec = 0;
    InitialiseSerial();
    InidDataMemW();
    LED_Data = gledstatus;
}

void main(void)
{

    ResetKeypad();

    Initialise();

    while (1)
    {
       ScanKeyPad();
    }
}
