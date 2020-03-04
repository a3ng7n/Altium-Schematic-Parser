/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   VGA Controller core device driver
|*
\*****************************************************************************/

//..............................................................................
#include <stdio.h>
#include "wb_vga_defs.h"
#include "wb_vga.h"
//..............................................................................

//..............................................................................
#define VGA_Ctrl_Register      *((volatile unsigned int*) (Video_BaseAddress + 0x00))   // Control     Register
#define VGA_STAT_Register      *((volatile unsigned int*) (Video_BaseAddress + 0x04))   // Status      Register
#define VGA_HTIM_Register      *((volatile unsigned int*) (Video_BaseAddress + 0x08))   // Horizontal  Register
#define VGA_VTIM_Register      *((volatile unsigned int*) (Video_BaseAddress + 0x0C))   // Vertical    Rgister
#define VGA_HVLEN_Register     *((volatile unsigned int*) (Video_BaseAddress + 0x10))   // Horizontal  and Vertical Length
#define VGA_VBARa_Register     *((volatile unsigned int*) (Video_BaseAddress + 0x14))   // Video Memory Base Address Register
#define VGA_DIV_Register       *((volatile unsigned int*) (Video_BaseAddress + 0x18))   // system clock division
#define VGA_LutOffset_Register *((volatile unsigned int*) (Video_BaseAddress + 0x1C))
//..............................................................................

//..............................................................................
// Each color lookup table is 256 long
// Data organization is:
// bit 31..24 - Unused
// bit 23..16 - Red
// bit 15..8  - Green
// bit 7..0   - Blue
#define VGA_LUT0_ADDRESS Video_BaseAddress + 0x800
#define VGA_LUT1_ADDRESS Video_BaseAddress + 0xC00

#define VGA_LUT0               *((volatile unsigned int*) VGA_LUT0_ADDRESS)
#define VGA_LUT1               *((volatile unsigned int*) VGA_LUT1_ADDRESS)
#define VGA_LUT_LENGTH (256 * 4)
//..............................................................................

//..............................................................................
#define ENDIANESS(x) ((((x) & 0xFF) << 24) | (((x) & 0xFF00) << 8) | (((x) & 0xFF0000) >> 8) | (((x) & 0xFF000000) >> 24))
//..............................................................................

//..............................................................................
unsigned int  Video_BaseAddress   = 0;
unsigned int  Video_Width         = 800;
unsigned int  Video_Height        = 600;
unsigned char Video_Mode          = VideoMode_800x600_bpp8;
unsigned char Video_BitsPerPixel  = 8;
unsigned char Video_Resolution    = HighResolution;
unsigned int  Video_DrawAddress   = 0x01020000;
//..............................................................................

//..............................................................................
void vga_set_address_for_drawing(unsigned int address)
{
    Video_DrawAddress = address;
}
//..............................................................................

//..............................................................................
unsigned int vga_get_address_for_drawing ( void )
{
    return Video_DrawAddress;
}
//..............................................................................

//..............................................................................
void vga_set_address_for_display(unsigned int address)
{
    VGA_VBARa_Register = address;
}
//..............................................................................

//..............................................................................
unsigned int vga_get_address_for_display ( void )
{
    return VGA_VBARa_Register;
}
//..............................................................................

//..............................................................................
void vga_open ( unsigned int base )
{
    Video_BaseAddress = base;
}
//..............................................................................

//..............................................................................
void vga_initialize_registers (void)
{
   if (Video_Resolution==LowResolution)
   {
      VGA_HTIM_Register  = ENDIANESS(HTIM_RegisterLowRes);
      VGA_VTIM_Register  = ENDIANESS(VTIM_RegisterLowRes);
      VGA_HVLEN_Register = ENDIANESS(HVLEN_RegisterLowRes);
      VGA_VBARa_Register = ENDIANESS(Video_DrawAddress);
   }
   else if (Video_Resolution==HighResolution)
   {
      VGA_HTIM_Register  = ENDIANESS(HTIM_RegisterHighRes);
      VGA_VTIM_Register  = ENDIANESS(VTIM_RegisterHighRes);
      VGA_HVLEN_Register = ENDIANESS(HVLEN_RegisterHighRes);
      VGA_VBARa_Register = ENDIANESS(Video_DrawAddress);
   }
   else if (Video_Resolution==LowResolutionDiv)
   {
      VGA_HTIM_Register  = ENDIANESS(HTIM_RegisterLowResDiv);
      VGA_VTIM_Register  = ENDIANESS(VTIM_RegisterLowResDiv);
      VGA_HVLEN_Register = ENDIANESS(HVLEN_RegisterLowResDiv);
      VGA_VBARa_Register = ENDIANESS(Video_DrawAddress);
      VGA_DIV_Register   = ENDIANESS(0x1);
   }
}
//..............................................................................

//..............................................................................
void vga_fill_screen(unsigned char color)
{
    unsigned int  i;
    unsigned int* Pointer;
    unsigned int  VideoMemorySize;
    unsigned int  ColorWord = 0;


    switch(Video_BitsPerPixel)
    {
        case 4 : color = (color << 4)  | (color & 0x0F);

        case 8 : ColorWord = (color << 24) | (color << 16) | (color << 8) | color;
                 //ColorWord = 0xFFFFFFFF;
                 break;

        case 2 : switch (color & 0x03)
                 {
                    case 0 : ColorWord = 0x00000000; break;
                    case 1 : ColorWord = 0x55555555; break;
                    case 2 : ColorWord = 0xAAAAAAAA; break;
                    case 3 : ColorWord = 0xFFFFFFFF; break;
                 }
                 break;

        case 1 : if (color & 1)
                     ColorWord = 0xFFFFFFFF;
                 else
                     ColorWord = 0;
                 break;

    }

    VideoMemorySize = (Video_Width * Video_Height) / (8/Video_BitsPerPixel);

    VideoMemorySize++;

    for ( i = 0; i < VideoMemorySize; i+=4 )
    {
       Pointer  = (unsigned int*) (i + Video_DrawAddress) ;
       *Pointer = ColorWord ;
    }
}
//..............................................................................

//..............................................................................
void vga_clear_screen(void)
{
    vga_fill_screen(0);
}
//..............................................................................

//..............................................................................
unsigned char vga_get_video_mode ( void )
{
    return Video_Mode;
}
//..............................................................................

//..............................................................................
void vga_set_video_mode(unsigned char mode)
{
    Video_Mode = mode;
    switch(Video_Mode)
    {
        case VideoMode_800x600_bpp8 :
             Video_Width         = 800;
             Video_Height        = 600;
             Video_BitsPerPixel  = 8;
             Video_Resolution    = HighResolution;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x0381);
             break;

        case VideoMode_800x600_bpp4 :
             Video_Width         = 800;
             Video_Height        = 600;
             Video_BitsPerPixel  = 4;
             Video_Resolution    = HighResolution;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x3A1);
             break;

        case VideoMode_800x600_bpp2 :
             Video_Width         = 800;
             Video_Height        = 600;
             Video_BitsPerPixel  = 2;
             Video_Resolution    = HighResolution;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x3C1);
             break;

        case VideoMode_800x600_bpp1 :
             Video_Width         = 800;
             Video_Height        = 600;
             Video_BitsPerPixel  = 1;
             Video_Resolution    = HighResolution;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x3E1);
             break;

        case VideoMode_640x480_bpp8 :
             Video_Width         = 640;
             Video_Height        = 480;
             Video_BitsPerPixel  = 8;
             Video_Resolution    = LowResolutionDiv;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x1381);
             break;

        case VideoMode_640x480_bpp4 :
             Video_Width         = 640;
             Video_Height        = 480;
             Video_BitsPerPixel  = 4;
             Video_Resolution    = LowResolutionDiv;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x13A1);
             break;

        case VideoMode_640x480_bpp2 :
             Video_Width         = 640;
             Video_Height        = 480;
             Video_BitsPerPixel  = 2;
             Video_Resolution    = LowResolutionDiv;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x13C1);
             break;

        case VideoMode_640x480_bpp1 :
             Video_Width         = 640;
             Video_Height        = 480;
             Video_BitsPerPixel  = 1;
             Video_Resolution    = LowResolutionDiv;
             vga_initialize_registers();
             VGA_Ctrl_Register = ENDIANESS(0x13E1);
             break;
    }
}
//..............................................................................

/**********************************************************************
|*
|*  FUNCTION    : vga_plot
|*
|*  INPUT       : x, y = coordinates of pixel
|*                color  = color of the pixel
|*
|*  OUTPUT      : None
|*
|*  DESCRIPTION : Draw one pixel with given color
 */
void vga_plot (unsigned int x,  unsigned int y,  unsigned char color)
{
    unsigned int   ByteOffset = 0;
    unsigned char* vmem;
    unsigned char  byte;
    unsigned char  pix;
    unsigned char  mask;

    //........................................
    //Compensate for defect in VGA hardware
    x = x + 1;
    //........................................


    switch(Video_BitsPerPixel)
    {
        case 8 : ByteOffset = (y * (Video_Width / 1)) + (x / 1);  break;
        case 4 : ByteOffset = (y * (Video_Width / 2)) + (x / 2);  break;
        case 2 : ByteOffset = (y * (Video_Width / 4)) + (x / 4);  break;
        case 1 : ByteOffset = (y * (Video_Width / 8)) + (x / 8);  break;
    }

    vmem  = (unsigned char*) (Video_DrawAddress + ByteOffset);

    switch(Video_BitsPerPixel)
    {
        case 8 : *vmem = color ;
                 break;

        case 4 : color = color & 0x0f;       //Bits(7..4) are lower pixel (left most)
                 byte  = *vmem;

                 if (x & 1)
                    byte = (byte & 0xf0) | (color     );
                 else
                    byte = (byte & 0x0f) | (color << 4);

                 *vmem = byte;
                 break;

        case 2 : color = color & 0x03;      //Bits(7..6) are lower pixel (left most)
                 byte  = *vmem;

                 pix         =  2 * (3-(x & 3));
                 mask        = (3        ) << pix;
                 color       = (color & 3) << pix;
                 byte        = (byte & (~mask)) | color;
                 *vmem = byte;
                 break;

        case 1 : byte        = *vmem;
                 pix         =  7-(x & 0x07);
                 mask        = (1        ) << pix;
                 color       = (color & 1) << pix;
                 byte        = (byte & (~mask)) | color;
                 *vmem = byte;
                 break;
    }

}
//..............................................................................

//..............................................................................
void vga_fill(unsigned int  x1,
              unsigned int  y1,
              unsigned int  x2,
              unsigned int  y2,
              unsigned char color )
{
    unsigned int ix,iy;

   for (iy = y1; iy <= y2; iy++)
   {
       for (ix = x1; ix <= x2; ix++)
       {
           vga_plot(ix,iy,color);
       }
   }
}
// -----------------------------------------------------------------------------

/**********************************************************************
|*
|*  FUNCTION    : vga_line
|*
|*  INPUT       : x1, y1 = coordinates of startpoint
|*                x2, y2 = coordinates of endpoint
|*                color  = color to be used for drawing
|*
|*  OUTPUT      : None
|*
|*  DESCRIPTION : Draw a straight line between two points using Bresenham's
|*                integer-only algorithm
 */
//..............................................................................

//..............................................................................
void vga_line(unsigned int  x1,
              unsigned int  y1,
              unsigned int  x2,
              unsigned int  y2,
              unsigned char color )
{
    char    steep = 0;
    char    sx, sy;
    char    swap_char;
    int     swap_int;
    int     dx, dy;
    int     d, i;

    if ( x2 > x1 )
    {
        sx = 1;
        dx = x2 - x1;
    }
    else
    {
        sx = -1;
        dx = x1 - x2;
    }

    if ( y2 > y1 )
    {
        sy = 1;
        dy = y2 - y1;
    }
    else
    {
        sy = -1;
        dy = y1 - y2;
    }

    if ( dy > dx )
    {
        steep = 1;
        swap_int = x1; x1 = y1; y1 = swap_int;
        swap_int = dx; dx = dy; dy = swap_int;
        swap_char = sx; sx = sy; sy = swap_char;
    }

    d = (2 * dy) - dx;
    for ( i = 0; i < dx; i++ )
    {
        if ( steep )
        {
            vga_plot( y1, x1, color );
        }
        else
        {
            vga_plot( x1, y1, color );
        }
        while ( d >= 0 )
        {
            y1 = y1 + sy;
            d = d - (2 * dx);
        }
        x1 = x1 + sx;
        d = d + (2 * dy);
    }
    vga_plot( x2, y2, color );
}
//..............................................................................

//..............................................................................
/*
|*
|*  FUNCTION    : vga_cicrle
|*
|*  INPUT       : x0, y0 = coordinates of center point
|*                r      = radius
|*                color  = color to be used for drawing
|*
|*  OUTPUT      : None
|*
|*  DESCRIPTION : Draw a circle using Bresenham's integer-only algorithm
 */
//..............................................................................

//..............................................................................
void vga_circle(unsigned int  x0,
                unsigned int  y0,
                unsigned int  r ,
                unsigned char color )
{
    int   eps;
    int   x;
    int   y;
    x = 0;
    y = r;
    eps = 3 - (r << 1);
    while( x <= y )
    {
        vga_plot( (x0 + x), (y0 + y), color );
        vga_plot( (x0 - x), (y0 + y), color );
        vga_plot( (x0 + x), (y0 - y), color );
        vga_plot( (x0 - x), (y0 - y), color );

        vga_plot( (x0 + y), (y0 + x), color );
        vga_plot( (x0 - y), (y0 + x), color );
        vga_plot( (x0 + y), (y0 - x), color );
        vga_plot( (x0 - y), (y0 - x), color );

        if ( eps < 0 )
        {
            eps += (x << 2) + 6;
        }
        else
        {
            eps += ((x - y) << 2) + 10;
            y--;
        }
        x++;
    }

}
//..............................................................................

//..............................................................................
void vga_test_pattern(void)
{
   unsigned char color = 0;
   unsigned int  x1,y1,x2,y2;

   unsigned int  x,y;
   unsigned int  RectangleSizeX = 50;
   unsigned int  RectangleSizeY = 50;
   unsigned int  StepsX;
   unsigned int  StepsY;

   vga_clear_screen();

   x1 = 0;
   y1 = 0;
   x2 = Video_Width  - 1;
   y2 = Video_Height - 1;

   color = 1;

   vga_line(x1,y1,x2,y1,color);
   vga_line(x2,y1,x2,y2,color);
   vga_line(x2,y2,x1,y2,color);
   vga_line(x1,y2,x1,y1,color);

   vga_plot(x1,y1,color);
   vga_plot(x2,y1,color);
   vga_plot(x2,y2,color);
   vga_plot(x1,y2,color);

   //return;

   //for (color = 0; color < 64; color++)
   //{
   //   vga_fill_screen(color);
   //}

   StepsX = (Video_Width  + (RectangleSizeX-1)) / RectangleSizeX;
   StepsY = (Video_Height + (RectangleSizeY-1)) / RectangleSizeY;

   color = 0;

   for (y = 0; y < StepsY; y++)
   {
       for (x = 0; x < StepsX; x++)
       {
           x1 = x  * RectangleSizeX;
           y1 = y  * RectangleSizeY;
           x2 = x1 + RectangleSizeX;
           y2 = y1 + RectangleSizeY;

           if (x2 >= Video_Width ) x2 = (Video_Width  - 1);
           if (y2 >= Video_Height) y2 = (Video_Height - 1);

           vga_fill(x1,y1,x2,y2,color);
           color++;
       }
   }
}
//..............................................................................

//..............................................................................
unsigned int NanoBoard_StandardColorTable[] =
{
CBlack    ,
CWhite    ,
CGrey1    ,
CGrey2    ,
CGrey3    ,
CGrey4    ,
CGrey5    ,
CGrey6    ,
CGrey7    ,
CGrey8    ,
CGrey9    ,
CGrey10   ,
CGrey11   ,
CGrey12   ,
CGrey13   ,
CGrey14   ,
CGrey15   ,
CGrey16   ,
CGrey17   ,
CGrey18   ,
CGrey19   ,
CGrey20   ,
CGrey21   ,
CGrey22   ,
CGrey23   ,
CGrey24   ,
CGrey25   ,
CGrey26   ,
CRed      ,
CRed1     ,
CRed2     ,
CGreen    ,
CGreen1   ,
CGreen2   ,
CBlue     ,
CBlue1    ,
CBlue2    ,
CYellow   ,
CYellow1  ,
CYellow2  ,
CYellow3  ,
CYellow4  ,
CYellow5  ,
CYellow6  ,
CYellow7  ,
CYellow8  ,
CMagenta  ,
CMagenta1 ,
CMagenta2 ,
CMagenta3 ,
CMagenta4 ,
CMagenta5 ,
CMagenta6 ,
CMagenta7 ,
CMagenta8 ,
CCyan     ,
CCyan1    ,
CCyan2    ,
CCyan3    ,
CCyan4    ,
CCyan5    ,
CCyan6    ,
CCyan7    ,
CCyan8
};
//..............................................................................

//..............................................................................
void         vga_set_palette_entry         ( unsigned int rgb, unsigned int index )
{
    unsigned int* p_palette;
    p_palette        = (unsigned int*) (VGA_LUT0_ADDRESS);
    p_palette[index] = ENDIANESS(rgb);
    p_palette        = (unsigned int*) (VGA_LUT1_ADDRESS);
    p_palette[index] = ENDIANESS(rgb);
}
//..............................................................................

//..............................................................................
unsigned int vga_get_palette_entry         ( unsigned int index )
{
    unsigned int* p_palette;
    p_palette = (unsigned int*) (VGA_LUT0_ADDRESS);
    return ENDIANESS(p_palette[index]);
}
//..............................................................................

//..............................................................................
void vga_set_default_palette(void)
{
    unsigned int i;
    unsigned int* Pointer;
    unsigned int TableValue;

    for ( i=0 ; i < (VGA_LUT_LENGTH) ; i+=4 )
    {
       TableValue = NanoBoard_StandardColorTable[(i / 4) & 0x3F];
       Pointer  = (unsigned int*) (i + VGA_LUT0_ADDRESS) ;
       *Pointer = ENDIANESS(TableValue);

       Pointer  = (unsigned int*) (i + VGA_LUT1_ADDRESS) ;
       *Pointer = ENDIANESS(TableValue);
    }

}
//..............................................................................

//..............................................................................
void vga_draw_bitmask(unsigned int x0, unsigned int y0, unsigned int width, unsigned int height,
                      unsigned char * data, unsigned char back_color, unsigned char fore_color,
                      unsigned int transparent)
{
    unsigned char* CurrentData = data;
    unsigned int BitNumber = 8;
    unsigned int i, j;
    unsigned char Value;

    for(i = 0; i < height; i++)
    {
        for(j = 0; j < width; j++)
        {
            if(BitNumber == 0)
            {
                CurrentData++;
                BitNumber = 8;
            }
            BitNumber--;
            Value = ((*CurrentData) >> BitNumber) & 1;
            if(Value || !transparent)
            {
                vga_plot(x0 + j, y0+ i, Value?fore_color:back_color);
            }
        }
        BitNumber = 0;
    }
}
//..............................................................................

//..............................................................................
int  vga_get_video_height         ( void )
{
    return Video_Height;
}
//..............................................................................

//..............................................................................
int  vga_get_video_width         ( void )
{
    return Video_Width;
}
//..............................................................................

//..............................................................................
int vga_get_bpp                   ( void )
{
    return Video_BitsPerPixel;
}
//..............................................................................

//..............................................................................
void vga_rect ( unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2, unsigned char color )
{
    vga_line(x1  , y1  , x2-1, y1  , color);
    vga_line(x2  , y1  , x2  , y2-1, color);
    vga_line(x2  , y2  , x1+1, y2  , color);
    vga_line(x1  , y2  , x1  , y1+1, color);
}
//..............................................................................

