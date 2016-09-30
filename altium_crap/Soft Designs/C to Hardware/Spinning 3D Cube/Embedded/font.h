#ifndef FONT_H
#define FONT_H

#include <stdint.h>

enum { fs_none = 0, fs_bold = 1, fs_italic = 2, fs_bold_italic = 3 };
typedef struct
{
    const unsigned short height;
    const unsigned short centreheight;
    const unsigned short *data;
} t_FONTDATA;

typedef struct
{
    const t_FONTDATA *fontdata;
    uint16_t color;
    char style;
} t_FONT;

#include "arial8.h"

void strdraw( short x0, short y0, char *str, t_FONT *font, uint16_t *vgabuf);
unsigned int strwidth( char *str, t_FONT *font );


#endif

