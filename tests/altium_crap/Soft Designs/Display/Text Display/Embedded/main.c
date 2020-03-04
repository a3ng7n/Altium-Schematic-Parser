/*****************************************************************************\
|*
|*  IN PACKAGE:         Software Platform examples
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:        This example shows how to use the serial interface to
|*                      write directly on the TFT through standard I/O calls.
|*
|*                      This interface features a subset of the ISO/IEC 6429 escape codes
|*                      (a.k.a. "ANSI-BBS") to control the cursor with and provide some
|*                      color control.
|*
|*                      Standard output (stdout) is connected to the graphics driver
|*                      Standard error (stderr) is connected to the terminal instrument
 */

#include <stdlib.h>
#include <stdio.h>


#include "bpreplay8.h"          // 8 points proportional font
#include <textdisplay.h>

#include <devices.h>
#include <timing.h>
#include <graphics.h>

posix_devctl_textdisplay_impl_t textio;     // Required for font switches (uses device control to install!)
graphics_t *graph;                          // Interface to graphics services
canvas_t *canvas;

static void init( void );

/**********************************************************************
|*
|*  FUNCTION    : main
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Program loop
 */

void main( void )
{
    init();

    fputs( "TFT Redirection example, file '" __FILE__ " compiled " __DATE__ ", " __TIME__ "\n"
           "Supported escape sequences:\n"
           "<esc>[<c>;<r>H     = move cursor position to column <c> and row <r>\n"
           "<esc>[0J           = clear from left top to cursor position\n"
           "<esc>[1J           = clear from cursor position to right bottom\n"
           "<esc>[2J           = clear screen and home cursor\n"
           "<esc>[K            = erase until end of line\n"
           "<esc>[s            = store current cursor position\n"
           "<esc>[u            = use previously stored cursor position\n"
           "<esc>[0m           = switch to normal font mode\n"
           "<esc>[1m           = switch to bold font mode\n"
           "<esc>[<color>m     = switch font or background color (see ISO 6429)", stderr );

    // Cursor position functions: store, gotoxy, use stored
    puts( "Save, gotoxy"            // Plain text
          "\x1B[s"                  // Followed by code for "store cursor position"
          "\x1B[12;2H"              // Goto row 12, column 2
          "text on 12,2"            // some more plain text
          "\x1B[u"                  // Restore and use previously stored cursor position
          " and back" );            // and print some plain text
    delay_ms( 1000 );

    // clear screen, C-style end of line
    printf( "\x1B[2JFirst line\nsecond\x1B[s line\nthird line\x1B[u" );
    delay_ms( 1000 );

    // Wipe until end of screen
    printf( "\x1B[0J" );
    delay_ms( 1000 );

    // Wipe until start of screen
    printf( "\x1B[1J" );
    delay_ms( 1000 );

    // Use a function from graphics, just to show you can:
    graphics_fill_canvas( canvas, GRAY18 );

    // Change font - bpmono8 is delivered as part of the graphics plugin
    textio.setfont( textio.device, &bpmono8 );
    puts( "\x1B[0;0HSwitched font to BPMono8" );

    // but we can change to a font provided by the application too
    textio.setfont( textio.device, &bpreplay8 );
    puts( "Switched to BPReplay8 font" );

    // Color demo:
    puts( "\x1B[1mBold foreground" );
    puts( "\x1B[0mNormal foreground" );
    puts( "\x1B[30mBlack foreground" );
    puts( "\x1B[31mRed foreground" );
    puts( "\x1B[32mGreen foreground" );
    puts( "\x1B[33mYellow foreground" );
    puts( "\x1B[34mBlue foreground" );
    puts( "\x1B[35mMagenta foreground" );
    puts( "\x1B[36mCyan foreground" );
    puts( "\x1B[37mWhite foreground" );

    puts( "\x1B[40m\x1B[KBlack background" );
    puts( "\x1B[41m\x1B[KRed background" );
    puts( "\x1B[42m\x1B[KGreen background" );
    puts( "\x1B[43m\x1B[KYellow background" );
    puts( "\x1B[44m\x1B[KBlue background" );
    puts( "\x1B[45m\x1B[KMagenta background" );
    puts( "\x1B[46m\x1B[KCyan background" );
    puts( "\x1B[47m\x1B[KWhite background" );
}

/**********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize the hardware & drivers
 */


static void init( void )
{
    // Not necessary for functioning of the text interface, but we use this to show
    // you can still use graphics, even though there's a text interface on top of the stack.
    graph = graphics_open( GRAPHICS_1 );
    canvas = graphics_get_visible_canvas(graph);

    // Install device specific functions through POSIX device control
    posix_devctl( fileno(stdout), DEVCTL_TEXTDISPLAY_IMPL, (void*)&textio, sizeof( textio ), NULL );
}
