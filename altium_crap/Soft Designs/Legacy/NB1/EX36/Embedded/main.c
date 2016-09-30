#include <stdio.h>
#include "util_timing.h"
#include "hardware.h"
#include "wb_vga_defs.h"
#include "wb_vga.h"
#include "io_wb_vga.h"

//..............................................................................
int rand_range( int range)
{
    return ((rand() & 0xfff) * range) / 0xfff;
}
//..............................................................................

//..............................................................................
void initialize ( void )
{
    //timing_set_clock_freq_hz (50 * 1000 * 1000);
    timing_set_clock_freq_hz (200 * 1000 * 1000);
    vga_open                    (Base_Video);
    vga_set_default_palette     (          );
    vga_set_address_for_drawing (0x01020000);
    vga_set_address_for_display (0x01020000);
}
//..............................................................................

//..............................................................................
const int delay_1 = 1000;
const int delay_2 = 4000;

#define border_width  5
#define panel_width  (17* 7 + 4*border_width)
#define panel_height ( 3*12 + 4*border_width)
//..............................................................................

//..............................................................................
char* text_to_show[8][3] =
{
    "Mode 1", "800x600 8bpp", "Page Size 469KB",
    "Mode 2", "800x600 4bpp", "Page Size 235KB",
    "Mode 3", "800x600 2bpp", "Page Size 118KB",
    "Mode 4", "800x600 1bpp", "Page Size  59KB",
    "Mode 5", "640x480 8bpp", "Page Size 300KB",
    "Mode 6", "640x480 4bpp", "Page Size 150KB",
    "Mode 7", "640x480 2bpp", "Page Size  75KB",
    "Mode 8", "640x480 1bpp", "Page Size  38KB"
};
//..............................................................................

//..............................................................................
void main (void)
{
   startup();
   unsigned char mode = VideoMode_800x600_bpp8;
   int offset_x = 0;
   int offset_y = 0;

   initialize();

   while(1)
   {

       if (mode > VideoMode_640x480_bpp1) mode = VideoMode_800x600_bpp8;

       vga_clear_screen();
       vga_set_video_mode(mode);
       timing_delay_ms(delay_1);
       vga_test_pattern();

       offset_x = ((vga_get_video_width()  - panel_width ) / 2);
       offset_y = ((vga_get_video_height() - panel_height) / 2);

       vga_fill (offset_x+1,
                 offset_y+1,
                 offset_x+panel_width-1,
                 offset_y+panel_height-1,
                 0);
       vga_rect (offset_x  ,
                 offset_y  ,
                 offset_x+panel_width  ,
                 offset_y+panel_height  ,
                 55);
       vga_rect (offset_x+border_width,
                 offset_y+border_width,
                 offset_x+panel_width-border_width,
                 offset_y+panel_height-+border_width,
                 55);

       vga_draw_string(offset_x+3*border_width, offset_y+2*border_width   , text_to_show[mode][0], 0, 37, 1);
       vga_draw_string(offset_x+3*border_width, offset_y+2*border_width+12, text_to_show[mode][1], 0, 37, 1);
       vga_draw_string(offset_x+3*border_width, offset_y+2*border_width+24, text_to_show[mode][2], 0, 37, 1);

       timing_delay_ms(delay_2);

       mode++;
   }
}
//..............................................................................

