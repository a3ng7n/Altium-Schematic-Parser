#include <stdio.h>
#include <stdlib.h>

#include "ntype.h"
#include "interrupt0.h"

#define KXDATA
#define KCODE
#define TXDATA __xdata
#define TCODE __rom

KXDATA TXDATA char m_VideoRam[VSIZENC] __at( 0x0000 );

KCODE TCODE byte bytemap[8]  = {1,2,4,8,16,32,64,128};

short m_X;
short m_Y;

unsigned short ltcx, ltcy, rbcx, rbcy;
unsigned short ltcxc, ltcyc, rbcxc, rbcyc;
unsigned short vphwidth = 0;
unsigned short vphheight = 0;
unsigned char  keyval = 0;
unsigned char  fkbyte = 0;

byte refreshen;


/*****************************************************************************
|*
|*  Function:           Interrupt0
|*
|*  Description:
|*
|*      This function will be called every interrupt, and
|*      is used as a timer for scrolling the text.
|*
 */
void Interrupt0( void )
{
    DISABLE_INTERRUPTS();           // Global interrupt disable
     switch ( P1 )
        {
        case SPCE0:
                keyval  = 0;
                fkbyte = 1;
                break;
        case ARROWLEFT:
                if (fkbyte == 1)
                {
                    keyval  = 1;
                    fkbyte = 0;
                }
                break;

        case ARROWRIGHT:
                if (fkbyte == 1)
                {
                    keyval  = 2;
                    fkbyte = 0;
                }
                break;
                
        case ARROWUP:
                if (fkbyte == 1)
                {
                    keyval  = 3;
                    fkbyte = 0;
                }
                break;

        case ARROWDOWN:
                if (fkbyte == 1)
                {
                    keyval  = 4;
                    fkbyte = 0;
                }
                break;

        case SPACE:
                keyval  = 5;
                fkbyte = 0;
                break;
        }
        
        if ((keyval == 1) && (ltcx >= 20))
        {
            ltcx = ltcx - 10;
            rbcx = rbcx - 10;
            refreshen = 1;
        }
        if ((keyval == 2) && (rbcx <= vphwidth - 20 ))
        {
            ltcx = ltcx + 10;
            rbcx = rbcx + 10;
            refreshen = 1;
        }
        if ((keyval == 3) && (ltcy >= 20 ))
        {
            ltcy = ltcy - 10;
            rbcy = rbcy - 10;
            refreshen = 1;
        }
        if ((keyval == 4) && (rbcy <= vphheight - 20 ))
        {
            ltcy = ltcy + 10;
            rbcy = rbcy + 10;
            refreshen = 1;
        }
        if (keyval == 5)
        {
            InitVideo();
            ltcx = vphwidth/2 - 50;
            ltcy = vphheight/2 - 50;
            rbcx = vphwidth/2 + 49;
            rbcy = vphheight/2 + 49;    
            refreshen = 1;
        }
        keyval = 0;
    ENABLE_INTERRUPTS();            // Global interrupt enable
}                           

void ClearDisplay(void)
{
    WORD countc = vphwidth/8;
    WORD countr = V_PHYSICAL_HEIGHT;
    TXDATA char KXDATA * pRam = m_VideoRam;
    
    pRam = 0;
    while (countr > 0)
    {
        while (countc > 0)
        {
            while (P3_1 == 1)
            {                
            }
            P3_5 = 0;
            *pRam = 0xFF;
            P3_5 = 1;
            pRam = pRam + 1;
            countc--;
        }
        countc = vphwidth/8;
        pRam = pRam;
        countr--;
    }   
    
    m_X = 0;
    m_Y = 0;
}

void InitC51Graphics(void)
{
    P3_0 = 0;
    m_X = 0;
    m_Y = 0;

    InitVideo();
    ClearDisplay();
}


void InitVideo(void)
{
    vphwidth = 800;
    vphheight = 600;
}


void SetPixel(short x, short y, BYTE color)
{
    TXDATA char KXDATA * pTarget;
    char pixels;

    pTarget = m_VideoRam + y*(vphwidth/8) + x/8;
    
    while (P3_1 == 1)
    {
    }
    P3_5 = 0;
    pixels = *pTarget;
    P3_5 = 1;
    
    if (color)
        pixels |= bytemap[x%8];
    else
        pixels &= ~bytemap[x%8];
    while (P3_1 == 1)
    {
    }
    P3_5 = 0;
    *pTarget = pixels;
    P3_5 = 1;
}


void MoveTo(short x, short y)
{
    m_X = x;
    m_Y = y;
}


void LineTo(short x2, short y2, byte color)
{
    
    short deltax,deltay,x,y,xinc1,xinc2,yinc1,yinc2,den,num,numadd,numpixels,curpixel;

    deltax = abs(x2 - m_X);        // The difference between the x's
    deltay = abs(y2 - m_Y);        // The difference between the y's
    x = m_X;                       // Start x off at the first pixel
    y = m_Y;                       // Start y off at the first pixel

    if (x2 >= m_X)                 // The x-values are increasing
    {
        xinc1 = 1;
        xinc2 = 1;
    }
    else                          // The x-values are decreasing
    {
        xinc1 = -1;
        xinc2 = -1;
    }

    if (y2 >= m_Y)                 // The y-values are increasing
    {
        yinc1 = 1;
        yinc2 = 1;
    }
    else                          // The y-values are decreasing
    {
        yinc1 = -1;
        yinc2 = -1;
    }

    if (deltax >= deltay)         // There is at least one x-value for every y-value
    {
        xinc1 = 0;                  // Don't change the x when numerator >= denominator
        yinc2 = 0;                  // Don't change the y for every iteration
        den = deltax;
        num = deltax / 2;
        numadd = deltay;
        numpixels = deltax;         // There are more x-values than y-values
    }
    else                          // There is at least one y-value for every x-value
    {
        xinc2 = 0;                  // Don't change the x for every iteration
        yinc1 = 0;                  // Don't change the y when numerator >= denominator
        den = deltay;
        num = deltay / 2;
        numadd = deltax;
        numpixels = deltay;         // There are more y-values than x-values
    }

    for (curpixel = 0; curpixel <= numpixels; curpixel++)
    {
        SetPixel(x, y, color);             // Draw the current pixel
        num += numadd;              // Increase the numerator by the top of the fraction
        if (num >= den)             // Check if numerator >= denominator
        {
        num -= den;               // Calculate the new numerator value
        x += xinc1;               // Change the x as appropriate
        y += yinc1;               // Change the y as appropriate
        }
        x += xinc2;                 // Change the x as appropriate
        y += yinc2;                 // Change the y as appropriate
    }
    m_X = x2;
    m_Y = y2;
}


void Rectangle(short x1, short y1, short x2, short y2, BYTE color)
{
    MoveTo(x1,y1);
    LineTo(x2,y1,color);
    LineTo(x2,y2,color);
    LineTo(x1,y2,color);
    LineTo(x1,y1,color);
}

void Initialise(void)
{
    ET1 = 0;
    EX1 = 0;
    ES = 0;
    
    refreshen = 0;
    ltcx = vphwidth/2 - 50;
    ltcy = vphheight/2 - 50;
    rbcx = vphwidth/2 + 49;
    rbcy = vphheight/2 + 49;
    ltcxc = ltcx;
    ltcyc = ltcy;
    rbcxc = rbcx;
    rbcyc = rbcy;   
    InitInterrupt0();               //Initialize interrupt 0
    ENABLE_INTERRUPTS();            // Global interrupt enable
}

void InitPs2(void)
{
    P1 = 0xFF;
    P3_3 = 0;
    P3_3 = 1;
    P3_3 = 0;
}


void main(void)
{
    InitC51Graphics();
    Initialise();
    InitPs2();
    // border
    Rectangle(5,5,vphwidth-6,vphheight-6,0);
    Rectangle(9,9,vphwidth-10,vphheight-10,0);

    while (1)
    {
        if ( refreshen == 1)
        {
            Rectangle(ltcxc,ltcyc,rbcxc,rbcyc,1);
            refreshen = 0;
            DisableInt0Interrupt();
            Rectangle(ltcx,ltcy,rbcx,rbcy,0);
            ltcxc = ltcx;
            ltcyc = ltcy;
            rbcxc = rbcx;
            rbcyc = rbcy;   
            EnableInt0Interrupt();
        }
    }
}
