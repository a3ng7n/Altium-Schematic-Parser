#ifndef __OSDEPEND_H__
#define __OSDEPEND_H__

#include "nano_hw.h"

#define ROM __rom
#define XDATA __xdata
#define AT(addr) __at(addr)
#define BIT __bit

#define PUTTILE(t,x,y) tileMap[((y)<<6)+(x)]=t
#define WIPESPRITE(x)

// taken from key definitions in allegro
// - used by pacman - do not change
#define DIR_LEFT   82
#define DIR_RIGHT  83
#define DIR_UP     84
#define DIR_DOWN   85

#endif

