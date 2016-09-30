 /****************************************************************
 * FILE:           @(#)vga.c	1.4 04/09/08
 * DESCRIPTION:                   
 *     VGA driver source code.                                  
 *     Access to the VGA hardware is protected with resource    
 *     'vgares'.                                                
 ***************************************************************/
#if defined(DEMO)
#include "demo.h"
#else
#include "tetris.h"
#endif

#if ( defined(TETRIS) && defined(NEXAR) )

#if (TETRISOUT != SERIAL0 && TETRISOUT != SERIAL1)

#include <osek/osek.h>


/* x offset in cell units */
#define X_OFF             10
/* y offset in cell units */
#define Y_OFF             3
/* size of a cell -pixels- */
#define W_CELL            10

/* board is (H_BOARD+2)(W_BOARD+2) */
#define B_X0              X_OFF*W_CELL
#define B_X1              B_X0 + W_CELL*(LASTBOARDROW+1)
#define B_Y0              Y_OFF*W_CELL
#define B_Y1              B_Y0 + W_CELL*(LASTBOARDLINE+1)

#define LINE             1
#define FILL             2
unsigned char chart_type = FILL;

typedef   unsigned char *( *charset )( const char c );

#define VCMD    (*( volatile __sfr unsigned char *) 0xD9 )
#define VXPOS   (*( volatile __sfr unsigned int  *) 0xDA )
#define VXPOSH  (*( volatile __sfr unsigned char *) 0xDA )
#define VXPOSL  (*( volatile __sfr unsigned char *) 0xDB )
#define VYPOS   (*( volatile __sfr unsigned int  *) 0xDC )
#define VYPOSH  (*( volatile __sfr unsigned char *) 0xDC )
#define VYPOSL  (*( volatile __sfr unsigned char *) 0xDD )
#define VCOLOR  (*( volatile __sfr unsigned char *) 0xDE )

inline void vga_plot( unsigned short x, unsigned short y, unsigned char color )
{
    /* wait until VGA-controller is ready to accept new command */
    while ( VCMD != 0 );

    /* Set x-pos, y-pos, and color */
    VXPOS = x;
    VYPOS = y;
    VCOLOR = color;
    /* write one pixel */
    VCMD = 2;
}

static   const unsigned char mask[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
static void vga_init(color bkg);
static unsigned char vga_getbmwidth(   const unsigned char *bm );
static unsigned char vga_getbmheight(   const unsigned char *bm );
static unsigned char vga_bitmap_2c( const unsigned short x, const unsigned short y,   const unsigned char *bm, const unsigned char fg_color, const unsigned char bg_color );
static unsigned char vga_bitmap_16c( const unsigned short x, const unsigned short y,   const unsigned char *bm );
static void vga_fill( unsigned short x1, unsigned short y1, unsigned short x2, unsigned short y2, unsigned char color );
static inline void vga_plot2( unsigned short x, unsigned short y, unsigned char color1, unsigned char color2 );

static unsigned int screen_puts(unsigned int x, unsigned int y,   char * s,
        charset cs, unsigned char fg_color, unsigned char bg_color);
static void screen_puti(unsigned int x, unsigned int y, unsigned int n,
        unsigned char fg_color, unsigned char bg_color);

static __idata unsigned char first;

extern   unsigned char bmp_altium16[];
extern   unsigned char *bios_font(const char c);

/* External definitions */
void ScreenInit(void)
{
      return;
}


void Fill(unsigned char xo,unsigned char yo,unsigned char x1,
          unsigned char y1,color color)
{
     if (first)
     {
         vga_init(black);
         first=0;
     }
     vga_fill(B_X0 + xo*W_CELL, B_Y1 - y1*W_CELL,
              B_X0 + x1*W_CELL, B_Y1 - yo*W_CELL,
              color);
}

#define E_ROW_Y       15
#define E_POS_X       B_X0+W_CELL*(LASTBOARDROW+1)/3
#define E_POS_Y       B_Y0+W_CELL*2*(LASTBOARDLINE+1)/5
void EndGame(void)
{
     Fill((W_BOARD+2)/3,2*(H_BOARD+2)/5,
              2*(W_BOARD+2)/3,3*(H_BOARD+2)/5,black);
    screen_puts(E_POS_X,E_POS_Y+E_ROW_Y,"GAME OVER",bios_font,white,black);
    screen_puts(E_POS_X,E_POS_Y+2*E_ROW_Y,"Press F ",bios_font,white,black);

    DisplayScoreBoard();
}

#define S_POS_X       400
#define S_POS_Y       120
#define S_ROW_Y       15
#define S_LINE_X      70

inline void
WritePoints(void)
{
   screen_puts(S_POS_X,S_POS_Y+S_POINT*S_ROW_Y,"POINTS :",
               bios_font,red,black);
   return;
}

inline void
WriteLines(void)
{
   screen_puts(S_POS_X,S_POS_Y+S_LINE*S_ROW_Y, "LINES  :",
               bios_font,red,black);
   }

inline void
WritePieces(void)
{
   screen_puts(S_POS_X,S_POS_Y+S_PIECE*S_ROW_Y,"PIECES :",
               bios_font,red,black);
}

inline void
WritePhase(void)
{
   screen_puts(S_POS_X,S_POS_Y+S_PHASE*S_ROW_Y,"PHASE  :",
               bios_font,red,black);
}


void ClearScreen(color color)
{
    /* wait until VGA-controller is ready to accept new command */
    while ( VCMD != 0 );

    /* fill whole screen with color black */
    VCOLOR = (color << 4) | color;
    VCMD = 1;

    /* 'init' the rest when first Fill */
    first = 1;
}

void TargetDisplayScoreBoard(unsigned int po, unsigned int li,
               unsigned int pi, unsigned char ph)
{

    screen_puti(S_POS_X+S_LINE_X,S_POS_Y+S_POINT*S_ROW_Y,po,
               red,black);
    screen_puti(S_POS_X+S_LINE_X,S_POS_Y+S_LINE*S_ROW_Y,li,
               red,black);
    screen_puti(S_POS_X+S_LINE_X,S_POS_Y+S_PIECE*S_ROW_Y,pi,
               red,black);
    screen_puti(S_POS_X+S_LINE_X,S_POS_Y+S_PHASE*S_ROW_Y,ph,
               red,black);
    return;
}




static void vga_init( color bkg )
{

    /* display Altium */
#define L_POS_X2         630
    vga_bitmap_16c(L_POS_X2 - vga_getbmwidth(bmp_altium16), 0, bmp_altium16);

    /* display Tetris keys Info */
#define I_POS_X       400
#define I_POS_Y       220
#define I_ROW_Y       10

   screen_puts(I_POS_X,I_POS_Y,          "Right:     ->",bios_font,white,bkg);
   screen_puts(I_POS_X,I_POS_Y+I_ROW_Y,  "Left:      <-",bios_font,white,bkg);
   screen_puts(I_POS_X,I_POS_Y+2*I_ROW_Y,"Rotate:    5",bios_font,white,bkg);
   screen_puts(I_POS_X,I_POS_Y+3*I_ROW_Y,"Down:      8",bios_font,white,bkg);
   screen_puts(I_POS_X,I_POS_Y+4*I_ROW_Y,"Pause:     2",bios_font,white,bkg);
   screen_puts(I_POS_X,I_POS_Y+5*I_ROW_Y,"Continue:  C",bios_font,white,bkg);

   WritePoints();
   WriteLines();
   WritePieces();
   WritePhase();

    return;
}



static void inline vga_plot2( unsigned short x, unsigned short y, unsigned char color1, unsigned char color2 )
{
    /* wait until VGA-controller is ready to accept new command */
    while ( VCMD != 0 );

    /* Set x-pos, y-pos, and color */
    VXPOS = x;
    VYPOS = y;
    VCOLOR = ( color1 << 4 ) | color2;
    /* write two pixels */
    VCMD = 3;
}



static unsigned char vga_bitmap_2c( const unsigned short x, const unsigned short y,   const unsigned char *bm, const unsigned char fg_color, const unsigned char bg_color )
{
    unsigned char i;
    unsigned char j;
    unsigned char width;
    unsigned char color1, color2;

    width = ( bm[0] & 0x07 ) ? ( bm[0] >> 3 ) + 1 : bm[0] >> 3;

    j = 0;

    if ( x % 2 )            /* odd x-pos? -> plot first vert line of bitmap */
    {
        for ( i = 0; i < bm[1]; i++ )   /* height */
        {
            color1 = ( bm[2 + ( i * width ) + ( j >> 3 )] & ( mask[j & 0x07])) ? fg_color : bg_color;
            vga_plot( x + j, y + i, color1 );
        }
        j++;
    }

    while ( j < bm[0] - 1 ) /* plot bitmap-body */
    {
        while ( VCMD != 0 );
        VXPOS = x + j;
        for ( i = 0; i < bm[1]; i++ )   /* height */
        {
            color1 = ( bm[2 + ( i * width ) + ( j >> 3 )] & ( mask[j & 0x07])) ? fg_color : bg_color;
            color2 = ( bm[2 + ( i * width ) + (( j+1 ) >> 3 )] & ( mask[(j+1) & 0x07 ])) ? fg_color : bg_color;

            /* wait until VGA-controller is ready to accept new command */
            while ( VCMD != 0 );

            /* Set x-pos, y-pos, and color */
            VYPOS = y + i;
            VCOLOR = ( color2 << 4 ) | color1;
            /* write two pixels */
            VCMD = 3;

        }
        j += 2;
    }

    if ( j < bm[0] )        /* last x-pos not done? -> plot last vert line of bitmap*/
    {
        for ( i = 0; i < bm[1]; i++ )   /* height */
        {
            color1 = ( bm[2 + ( i * width ) + ( j >> 3 )] & ( mask[j & 0x07])) ? fg_color : bg_color;
            vga_plot( x + j, y + i, color1 );
        }
    }

    return bm[0];
}

static
unsigned char vga_bitmap_16c( const unsigned short x, const unsigned short y,   const unsigned char *bm )
{
    unsigned char i, j;
    unsigned char width;
    unsigned char color1, color2;

    width = ( bm[0] >> 1 ) + ( bm[0] & 0x01 );

    i = 0;

    if ( x % 2 )            /* odd x-pos? -> plot first vert line of bitmap */
    {
        for ( j = 0; j < bm[1]; j++ )   /* height */
        {
            color1 = bm[2 + ( j * width )] >> 4;
            vga_plot( x, y + j, color1 );
        }
        i++;
    }

    while ( i < bm[0] - 1 ) /* plot bitmap-body */
    {
        for ( j = 0; j < bm[1]; j++ )   /* height */
        {
            if ( i % 2 )
            {
                color1 = bm[2 + ( j * width ) + (( i+1 ) >> 1 )] >> 4;
                color2 = bm[2 + ( j * width ) + ( i >> 1 )] & 0x0F;
            }
            else
            {
                color2 = bm[2 + ( j * width ) + ( i >> 1 )] >> 4;
                color1 = bm[2 + ( j * width ) + ( i >> 1 )] & 0x0F;
            }
            vga_plot2( x + i , y + j, color1, color2 );
        }
        i += 2;
    }

    if ( i < bm[0] )        /* last x-pos not done? -> plot last vert line of bitmap*/
    {
        for ( j = 0; j < bm[1]; j++ )   /* height */
        {
            vga_plot( x + i, y + j, bm[2 + ( j * width ) + ( i >> 1 )] >> 4 );
        }
    }

    return bm[0];
}

static
unsigned char vga_getbmwidth(   const unsigned char *bm )
{
    return bm[0];
}

static unsigned char vga_getbmheight(   const unsigned char *bm )
{
    return bm[1];
}
static
void vga_fill( unsigned short x1, unsigned short y1, unsigned short x2, unsigned short y2, unsigned char color )
{
    short x, xstop, ystart, ystop, y;

    /* set color */
    while ( VCMD != 0 );
    VCOLOR = ( color << 4 ) | color;

    if ( x1 > x2 )
    {
        x = x2;
        xstop = x1;
    }
    else
    {
        x = x1;
        xstop = x2;
    }

    if ( y1 > y2 )
    {
        ystart = y2;
        ystop = y1 + 1;
    }
    else
    {
        ystart = y1;
        ystop = y2 + 1;
    }

    if ( x % 2 )            /* first pixel on odd x-pos? -> plot first line */
    {
        /* wait until VGA-controller is ready to accept new data */
        while ( VCMD != 0 );
        /* Set x-pos and y-pos */
        VXPOS = x;
        VYPOS = ystart;

        for ( y = ystart; y < ystop; y++ )
        {
            /* wait until VGA-controller is ready to accept new command */
            while ( VCMD != 0 );
//            VYPOS = y;
            /* write one pixel and increment y */
            VCMD = 0x12;
        }
        x++;
    }

    while ( x < xstop )        /* plot block, two pixels a time */
    {
        /* wait until VGA-controller is ready to accept new data */
        while ( VCMD != 0 );
        /* Set x-pos and y-pos */
        VXPOS = x;
        VYPOS = ystart;

        for ( y = ystart; y < ystop; y++ )
        {
            /* wait until VGA-controller is ready to accept new command */
            while ( VCMD != 0 );
            /* write two pixels */
//            VYPOS = y;
            VCMD = 0x13;
        }
        x += 2;
    }

    if ( x != xstop )          /* all x done, else plot last line */
    {
        /* wait until VGA-controller is ready to accept new data */
        while ( VCMD != 0 );
        /* Set x-pos and y-pos */
        VXPOS = x;
        VYPOS = ystart;

        for ( y = ystart; y < ystop; y++ )
        {
            /* wait until VGA-controller is ready to accept new command */
            while ( VCMD != 0 );
            /* write one pixel and increment y */
//            VYPOS = y;
            VCMD = 0x12;
        }
    }
}



static unsigned int screen_puts(unsigned int x, unsigned int y,   char * s,
        charset cs, unsigned char fg_color, unsigned char bg_color)
{
    while (* s)
    {
        x += vga_bitmap_2c(x, y, cs(* s++), fg_color, bg_color);
    }

    return x;
}

static void screen_puti(unsigned int x, unsigned int y,
        unsigned int n, unsigned char fg_color, unsigned char bg_color)
        {
        unsigned char i;
        unsigned int div=1;

        if (n < 10000)
        {
           for(i=0;i<NO_CIFERS-1;i++) div = 10*div;
           for (i=0;i<NO_CIFERS;i++)
             {
              x += vga_bitmap_2c(x, y, bios_font(('0' + n/div)), fg_color, bg_color);
              n = n%div;
              div = div/10;
             }
        }
        return;
}

#endif /* TETRISOUT */
#endif /* TETRIS + NEXAR */

