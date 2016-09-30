#ifndef __IO_WB_VGA_H__
#define __IO_WB_VGA_H__
//..............................................................................

//..............................................................................
unsigned int vga_get_caret_pos_x ( void );
void         vga_set_caret_pos_x ( unsigned int x );
unsigned int vga_get_caret_pos_y ( void );
void         vga_set_caret_pos_y ( unsigned int y );
unsigned int vga_get_back_color  ( void );
void         vga_set_back_color  ( unsigned int color );
unsigned int vga_get_fore_color  ( void );
void         vga_set_fore_color  ( unsigned int color );
//..............................................................................

//..............................................................................
typedef enum
{
    vsm_wrap,
    vsm_wrap_and_clear,
    vsm_scroll
} vga_scroll_mode_t;
//..............................................................................

//..............................................................................
vga_scroll_mode_t vga_get_vertical_scroll_mode ( void );
void              vga_set_vertical_scroll_mode ( vga_scroll_mode_t mode );
//..............................................................................

//..............................................................................
void vga_draw_text  ( unsigned int x0,
                      unsigned int y0,
                      unsigned char * text,
                      unsigned char back_color,
                      unsigned char fore_color,
                      unsigned int transparent );
void vga_draw_string( unsigned int x0,
                      unsigned int y0,
                      unsigned char * string,
                      unsigned char back_color,
                      unsigned char fore_color,
                      unsigned int transparent );
int  vga_write      ( const char * buf,
                      int size );
//..............................................................................

//..............................................................................
#endif
